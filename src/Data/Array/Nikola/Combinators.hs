{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      : Data.Array.Nikola.Combinators
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Data.Array.Nikola.Combinators (
    iterate,
    iterateWhile,
    unfoldP,
    mapNest,
    fold,
    fold2
  ) where

import Prelude hiding (iterate)

import Data.Int
import Data.Typeable (Typeable)

import Data.Array.Nikola.Exp
import Data.Array.Nikola.Language.Reify
import Data.Array.Nikola.Language.Monad
import Data.Array.Nikola.Language.Syntax hiding (Exp, Var)
import Data.Array.Nikola.Repr.Push
import Data.Array.Nikola.Repr.Delayed
import Data.Array.Nikola.Shape
import Data.Array.Nikola.Array

-- | 'iterate n f x' iterates the function 'f' 'n' times with the initial value
-- 'x'. GHC cannot in general tell that the 't' in 'Exp t Int32' is the same 't'
-- that appears in the lifted type of 'a', so a type signature on 'n' may be
-- necessary to disambiguate.
iterate :: forall t a .
           ( IsElem (Exp t (Lifted t a))
           , Unlift t a)
        => Exp t Int32 -> (a -> a) -> a -> a
iterate n f x =
    unlift e
  where
    x' :: Exp t (Lifted t a)
    x' = lift x

    f' :: Exp t (Lifted t a) -> Exp t (Lifted t a)
    f' = lift . f . unlift

    e :: Exp t (Lifted t a)
    e = E $ IterateE (unE n) (delayE f') (unE x')

-- | 'iterateWhile n f x' iterates the function 'f' 'n' times, or until the
-- first component of the tuple returned by f is False, with the initial value
-- 'x'.
iterateWhile :: forall t a b .
                ( IsElem (Exp t (Lifted t a))
                , IsElem (Exp t (Lifted t b))
                , Lift t b
                , Lifted t b ~ Bool
                , Unlift t a)
             => Exp t Int32 -> (a -> (b, a)) -> a -> a
iterateWhile n f x =
    unlift e
  where
    x' :: Exp t (Lifted t a)
    x' = lift x

    f' :: Exp t (Lifted t a) -> (Exp t Bool, Exp t (Lifted t a))
    f' x = case f (unlift x) of
             (test, x') -> (lift test, lift x')

    e :: Exp t (Lifted t a)
    e = E $ IterateWhileE (unE n) (delayE f') (unE x')

fold :: Shape ix =>
        Source r a =>
        Unlift t a =>
        IsElem (Exp t (Lifted t a)) =>
        Typeable (Lifted t a) =>
        -- are these necessary?
        Typeable t =>
        IsElem (Exp t Int32) =>
     (a -> a -> a) -> a -> Array r (ix :. Exp t Ix) a -> Array D ix a
fold f b arr =
  let (sh :. n, lookup) = toFunction arr
      buildArray ix = snd $ iterate n iter (0,b)
        where
          iter (i,x) = (i+1, f (lookup (ix :. i)) x)
  in fromFunction sh buildArray

-- With the presence of mapNest, fold doesn't have to be so complicated.
fold2 ::
  forall a b t r .
  IsElem (Exp t Ix) =>
  IsElem (Exp t a) =>
  IsElem (Exp t b) =>
  (Exp t a -> Exp t b -> Exp t a) -> Exp t a -> Array PSH (Z :. Exp t Ix) (Exp t b) -> Exp t a
fold2 f a bs =
  E $ DelayedE $ do
    let APush sh bsM = bs
    vTmp <- gensym "tmp"
    loop <- reset $ do
      (i,x) <- bsM
      -- return $ bindE vTmp tau (ReturnE . unE $ f (varE $ V vTmp) (bs ! i)) (ReturnE UnitE)
      --   | LetE Var Type Occ Exp Exp
      return $ LetE vTmp tau Many (ReturnE $ unE $ f (varE $ V vTmp) x) (ReturnE UnitE)-- (VarE vTmp)

    return $ bindE vTmp tau (unE a) (ReturnE $ loop)

  where
    tau :: Type
    tau = ScalarT $ typeOf (undefined :: Exp t a)


-- | Construct a push array from a seed and a successor function.
unfoldP ::
  forall sh t a .
  Shape sh =>
  IsElem (Exp t a) =>
  Typeable a =>
    sh ->
    (sh -> Exp t a -> Exp t a) ->
    Exp t a ->
    Array PSH sh (Exp t a)
unfoldP sh f x = APush sh m
  where
    m :: P (sh, Exp t a)
    m = shift $ \k -> do -- k may for instance be the contents of loadP..
      vX <- gensym "x"
      loop <- reset $ do
        i <- seqfor sh
        body <- k (i, varE $ V vX)
        return $ (bindE vX tau (ReturnE $ unE $ f i (varE $ V vX)) body)
      return $ bindE vX tau (ReturnE $ unE x) loop

    tau :: Type
    tau = ScalarT $ typeOf (undefined :: Exp t a)

{-
mapNest ::
  forall sh t a b r.
  Shape sh =>
  IsElem a =>
  IsElem b =>
    Exp t Ix ->
    (sh -> a -> Array PSH (Z :. Exp t Ix) b) ->
    Array PSH sh a ->
    Array PSH (sh :. Exp t Ix) b
mapNest n f src = APush (extent src :. n) m
  where
    m :: P (sh :. Exp t Ix, b)
    m = do
      let APush srcSh srcM = src
      (i, x) <- srcM
      let APush fSh fM = f i x
      (Z :. j, fX) <- fM
      return (i :. j, fX)
-}

-- | @mapNest sh f src@ applies @f@ to every cell in @src@, yielding an extra
-- dimension. Each application of @f@ had better produce arrays of the same
-- shape: @sh@. Note that even though this is defined for Push arrays, delayed
-- arrays are easily convertible to Push arrays by means of 'mkPushArray'.
mapNest ::
  Shape sh =>
  Shape sh' =>
  IsElem a =>
  IsElem (Exp t Ix) =>
  IsElem b =>
  Source r a =>
      sh' -> (Exp t Ix -> Array D sh a -> Array PSH sh' b)
          -> Array r  (sh :. Exp t Ix) a
          -- -> Array PSH ((sh :. Exp t Ix) :+: sh')   b
          -> Array PSH ((Z :. Exp t Ix) :+: sh')   b
mapNest fSh fn xs =
  let sh :. i = extent xs
      resSh = combineSh (Z :. i) fSh
  in
  APush resSh $ do
    Z:. i <- parfor (Z :. i)
    let APush _ fM = fn i (fromFunction sh $ \sh -> xs ! (sh :. i))
    (i',x) <- fM
    return (combineSh (Z :. i) i', x)

-- Sequentialize the outermost for-loop.
sequentializeFor :: S.Exp -> S.Exp
sequentializeFor loop = runIdentity $ traverseFam go ExpA loop
  where
    go :: Traversal AST Identity
    -- go :: AST S.Exp -> S.Exp -> Identity S.Exp
    go ExpA (ForE _ vs is body) = return $ ForE SeqParFor vs is body
    go ExpA loop = traverseFam go ExpA loop

