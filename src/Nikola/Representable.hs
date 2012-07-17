-- Copyright (c) 2009-2012
--         The President and Fellows of Harvard College.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
-- 3. Neither the name of the University nor the names of its contributors
--    may be used to endorse or promote products derived from this software
--    without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE UNIVERSITY AND CONTRIBUTORS ``AS IS'' AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED.  IN NO EVENT SHALL THE UNIVERSITY OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
-- OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Nikola.Representable (
    Representable(..),
    Elt,

    Vector(..),
    unsafeWithNewVector,
    unsafeFreeVector
  ) where

import Control.Applicative
import Control.Exception
import Control.Monad.Trans (liftIO)
#if defined(HMATRIX)
import Data.Packed.Development
import Data.Packed.Matrix
#endif /* defined(HMATRIX) */
import Data.Typeable
import qualified Data.Vector.Storable as V
import qualified Foreign.CUDA.Driver as CU
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

import Nikola.Exec
import Nikola.Syntax

-- | Vectors whose contents exist in GPU memory
data Vector a = Vector {-# UNPACK #-} !Int
                       {-# UNPACK #-} !(CU.DevicePtr (Rep a))
  deriving (Eq, Ord, Typeable)

instance Show (Vector a) where
  showsPrec n (Vector _ p) = showsPrec n p

instance Storable (Vector a) where
  sizeOf _            = sizeOf    (undefined :: Ptr a)
  alignment _         = alignment (undefined :: Ptr a)
  peek p              = Vector 1 `fmap` peek (castPtr p)
  poke p (Vector _ d) = poke (castPtr p) (CU.useDevicePtr d)

unsafeWithNewVector :: Storable (Rep a)
                    => Int
                    -> (Vector a -> IO b)
                    -> IO b
unsafeWithNewVector n =
    bracket alloc free
  where
    alloc = Vector n <$> CU.mallocArray n

    free (Vector _ ptr) = CU.free ptr

unsafeFreeVector :: Vector a -> IO ()
unsafeFreeVector (Vector _ devPtr) =
    CU.free devPtr

embeddedScalarType :: Representable a => a -> Tau
embeddedScalarType dummy =
    unScalarT (embeddedType dummy 0)

-- | A type that can be used in a GPU embedding.
class (Typeable a, Storable (Rep a)) => Representable a where
    -- | The /representation/ type for @a@ when a value of type @a@ is
    -- tranferred to the GPU.
    type Rep a :: *

    -- | Convert to representation type.
    toRep :: a -> IO (Rep a)

    -- | Convert from representation type.
    fromRep :: Rep a -> IO a

    -- | The embedded type that corresponds to 'a'.
    embeddedType :: a -> ParamIdx -> Rho

    -- | Extend the current execution context with an argument of type 'a' and
    -- then continue by performing an 'Ex' action..
    withArg :: a -> Ex b -> Ex b

    -- | An 'Ex' action that returns a result of type 'a'.
    returnResult :: Ex a

-- | A scalar type that can be used in GPU code. This is used to constrain the
-- set of legal types that can appear in containers, e.g., vectors and matrices.
class Representable a => Elt a where

instance Representable () where
    type Rep () = CInt

    toRep () = return  0

    fromRep _ = return ()

    embeddedType _ _ = ScalarT UnitT

    withArg _ act = do
        pushArg (IntArg 0)
        act

    returnResult =
        return ()

instance Elt () where

instance Representable Int where
    type Rep Int = CInt

    toRep = return . fromIntegral

    fromRep = return . fromIntegral

    embeddedType _ _ = ScalarT IntT

    withArg n act = do
        pushArg (IntArg n)
        act

    returnResult = do
        devPtr :: CU.DevicePtr (Rep Int) <- (CU.castDevPtr . allocPtr) <$> popAlloc
        x <- liftIO $ alloca $ \hostPtr -> do
             CU.peekArray 1 devPtr hostPtr
             peek hostPtr
        liftIO $ CU.free devPtr
        return (fromIntegral x)

instance Elt Int where

instance Representable Float where
    type Rep Float = CFloat

    toRep = return . realToFrac

    fromRep = return . realToFrac

    embeddedType _ _ = ScalarT FloatT

    withArg n act = do
        pushArg (FloatArg n)
        act

    returnResult = do
        devPtr :: CU.DevicePtr Float <- (CU.castDevPtr . allocPtr) <$> popAlloc
        x <- liftIO $ alloca $ \hostPtr -> do
             CU.peekArray 1 devPtr hostPtr
             peek hostPtr
        liftIO $ CU.free devPtr
        return (realToFrac x)

instance Elt Float where

instance (Elt a, Storable a, Storable (Rep a))
    => Representable [a] where
    type Rep [a] = Vector a

    toRep xs = do
        devPtr <- liftIO $ CU.mallocArray n
        allocaArray n $ \(ptr :: Ptr (Rep a)) -> do
            mapM toRep xs >>= pokeArray ptr
            CU.pokeArray n ptr devPtr
        return (Vector n devPtr)
      where
        n :: Int
        n = length xs

    fromRep (Vector n devPtr) = do
        xs <- allocaArray n $ \(ptr :: Ptr (Rep a)) -> do
              CU.peekArray n (CU.castDevPtr devPtr) ptr
              peekArray n ptr
        mapM fromRep xs

    embeddedType _ n =
        vectorT tau n
      where
        tau = embeddedScalarType (undefined :: a)

    withArg xs act = do
        Vector n devPtr <- liftIO $ toRep xs
        pushArg (VectorArg n (CU.castDevPtr devPtr))

        result <- act
        liftIO $ CU.free devPtr
        return result

    returnResult = do
        count :: Int          <- returnResult
        VectorAlloc _ devPtr  <- popAlloc
        xs <- liftIO $ fromRep (Vector count (CU.castDevPtr devPtr))
        liftIO $ CU.free devPtr
        return xs

instance (Elt a, Storable a)
    => Representable (Vector a) where
    type Rep (Vector a) = Vector a

    fromRep = return

    toRep = return

    embeddedType _ n =
        vectorT tau n
      where
        tau = embeddedScalarType (undefined :: a)

    withArg (Vector n devPtr) act = do
        pushArg (VectorArg n (CU.castDevPtr devPtr))
        act

    returnResult = do
        count :: Int         <- returnResult
        VectorAlloc _ devPtr <- popAlloc
        return $ Vector count (CU.castDevPtr devPtr)

instance (Elt a, Storable a)
    => Representable (V.Vector a) where
    type Rep (V.Vector a) = Vector a

    toRep v = do
        devPtr <- liftIO $ CU.mallocArray n
        liftIO $ V.unsafeWith v $ \ptr ->
                 CU.pokeArray n ptr devPtr
        return (Vector n (CU.castDevPtr devPtr))
      where
        n :: Int
        n = V.length v

    fromRep (Vector n devPtr) = do
        fptr <- mallocForeignPtrArray n
        withForeignPtr fptr $ \ptr ->
            CU.peekArray n (CU.castDevPtr devPtr) ptr
        return $ V.unsafeFromForeignPtr fptr 0 n

    embeddedType _ n =
        vectorT tau n
      where
        tau = embeddedScalarType (undefined :: a)

    withArg v act = do
        Vector n devPtr <- liftIO $ toRep v

        pushArg (VectorArg n (CU.castDevPtr devPtr))

        result <- act
        liftIO $ CU.free devPtr
        return result

    returnResult = do
        count :: Int         <- returnResult
        VectorAlloc _ devPtr <- popAlloc
        xs <- liftIO $ fromRep (Vector count (CU.castDevPtr devPtr))
        liftIO $ CU.free devPtr
        return xs

#if defined(HMATRIX)
deriving instance Typeable1 Matrix

-- | A helper function that adapts 'Data.Packed.Development.mat' to a more
-- traditional \"with\"-style interface. @f@ is passed the number of rows, the
-- number of columns, and a 'Ptr' to the matrix contents.
withMatrix  ::  Storable t
            =>  Matrix t
            ->  (CInt -> CInt -> Ptr t -> IO ())
            ->  IO ()
withMatrix m f = mat m $ \g -> g f

instance (Element a, Elt a, Storable a)
    => Representable (Matrix a) where
    type Rep (Matrix a) = CU.DevicePtr (Rep a)

    toRep _ = fail "toRep undefined for Matrix"

    fromRep _ = fail "fromRep undefined for Matrix"

    embeddedType _ n =
        matrixT tau n n n
      where
        tau = embeddedScalarType (undefined :: a)

    withArg m act = do
        devPtr <- liftIO $ CU.mallocArray n
        liftIO $ withMatrix (fmat m) $ \_ _ ptr ->
                 CU.pokeArray n ptr devPtr

        pushArg (MatrixArg r r c (CU.castDevPtr devPtr))

        result <- act
        liftIO $ CU.free devPtr
        return result
      where
        r, c, n :: Int
        r = rows m
        c = cols m
        n = r * c

    returnResult = do
        MatrixAlloc _ r c devPtr <- popAlloc
        r      <-  evalN r
        c      <-  evalN c
        let n  =   r * c
        liftIO $ do  m <- createMatrix ColumnMajor r c
                     withMatrix m $ \_ _ ptr ->
                         CU.peekArray n (CU.castDevPtr devPtr) ptr
                     CU.free devPtr
                     return m
#endif /* defined(HMATRIX) */