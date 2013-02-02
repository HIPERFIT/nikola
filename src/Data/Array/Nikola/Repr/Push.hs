{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      : Data.Array.Nikola.Repr.Push
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Data.Array.Nikola.Repr.Push (
    PSH,
    Array(..),

    mkPushArray,
    unfoldPushArray,
    push
  ) where

import Data.Typeable (Typeable)

import Data.Array.Nikola.Array
import Data.Array.Nikola.Eval
import Data.Array.Nikola.Program
import Data.Array.Nikola.Shape

import Data.Array.Nikola.Exp
import Data.Array.Nikola.Language.Monad
import Data.Array.Nikola.Language.Syntax hiding (Var, Exp)

-- | "Push" arrays represented by a program that produces index, value pairs.
data PSH
  deriving (Typeable)

instance IsArray PSH a where
    data Array PSH sh a = APush sh (P (sh, a))

    extent (APush sh _) = sh

instance Shape sh => Load PSH sh e where
    loadP (APush _ m) marr = do
        p1 <- reset $ do  (i, x) <- m
                          unsafeWriteMArray marr i x
                          return $ ReturnE UnitE
        seqK p1 ()

-- | Construct a push array from a function mapping indices to values.
mkPushArray :: forall sh a . (Shape sh)
            => sh
            -> (sh -> a)
            -> Array PSH sh a
mkPushArray sh f = APush sh m
  where
    m :: P (sh, a)
    m = do  i <- parfor sh
            return (i, f i)

-- | Construct a push array from a seed and a successor function.
unfoldPushArray ::
  forall sh t a .
  Shape sh =>
  IsElem (Exp t a) =>
  Typeable a =>
    sh ->
    Exp t a ->
    (sh -> Exp t a -> Exp t a) ->
    Array PSH sh (Exp t a)
unfoldPushArray sh x f = APush sh m
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

-- | @mapNest n f src@ applies @f@ to every cell in @src@, yielding an extra
-- dimension. Each application of @f@ had better produce arrays of the same
-- length: @n@. Note that even though this is defined for Push arrays, delayed
-- arrays are easily convertible to Push arrays by means of 'mkPushArray'.
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

-- | Convert an array into a push array.
push :: (Shape sh, Source r e)
     => Array r sh e
     -> Array PSH sh e
push arr = mkPushArray (extent arr) (index arr)
