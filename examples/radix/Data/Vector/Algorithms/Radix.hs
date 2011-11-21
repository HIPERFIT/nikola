{-# LANGUAGE ScopedTypeVariables, BangPatterns, TypeOperators #-}

-- ---------------------------------------------------------------------------
-- |
-- Module      : Data.Vector.Algorithms.Radix
-- Copyright   : (c) 2008-2010 Dan Doel
-- Maintainer  : Dan Doel <dan.doel@gmail.com>
-- Stability   : Experimental
-- Portability : Non-portable (scoped type variables, bang patterns)
--
-- This module provides a radix sort for a subclass of unboxed arrays. The
-- radix class gives information on
--   * the number of passes needed for the data type
--
--   * the size of the auxiliary arrays
--
--   * how to compute the pass-k radix of a value
--
-- Radix sort is not a comparison sort, so it is able to achieve O(n) run
-- time, though it also uses O(n) auxiliary space. In addition, there is a
-- constant space overhead of 2*size*sizeOf(Int) for the sort, so it is not
-- advisable to use this sort for large numbers of very small arrays.
--
-- A standard example (upon which one could base their own Radix instance)
-- is Word32:
--
--   * We choose to sort on r = 8 bits at a time
--
--   * A Word32 has b = 32 bits total
--
--   Thus, b/r = 4 passes are required, 2^r = 256 elements are needed in an
--   auxiliary array, and the radix function is:
--
--    > radix k e = (e `shiftR` (k*8)) .&. 256

module Data.Vector.Algorithms.Radix (sort, sortBy, Radix(..)) where

import Prelude hiding (read, length)

import Control.Monad
import Control.Monad.Primitive

import qualified Data.Vector.Primitive.Mutable as PV
import Data.Vector.Generic.Mutable

import Data.Vector.Algorithms.Common (Comparison)

import Data.Bits
import Data.Int
import Data.Word


import Foreign.Storable

class Radix e where
  -- | The number of passes necessary to sort an array of es
  passes :: e -> Int
  -- | The size of an auxiliary array
  size   :: e -> Int
  -- | The radix function parameterized by the current pass
  radix  :: Int -> e -> Int

instance Radix Int where
  passes _ = sizeOf (undefined :: Int)
  {-# INLINE passes #-}
  size _ = 256
  {-# INLINE size #-}
  radix 0 e = e .&. 255
  radix i e
    | i == passes e - 1 = radix' (e `xor` minBound)
    | otherwise         = radix' e
   where radix' e = (e `shiftR` (i `shiftL` 3)) .&. 255
  {-# INLINE radix #-}

instance Radix Int8 where
  passes _ = 1
  {-# INLINE passes #-}
  size _ = 256
  {-# INLINE size #-}
  radix _ e = 255 .&. fromIntegral e `xor` 128
  {-# INLINE radix #-}

instance Radix Int16 where
  passes _ = 2
  {-# INLINE passes #-}
  size _ = 256
  {-# INLINE size #-}
  radix 0 e = fromIntegral (e .&. 255)
  radix 1 e = fromIntegral (((e `xor` minBound) `shiftR` 8) .&. 255)
  {-# INLINE radix #-}

instance Radix Int32 where
  passes _ = 4
  {-# INLINE passes #-}
  size _ = 256
  {-# INLINE size #-}
  radix 0 e = fromIntegral (e .&. 255)
  radix 1 e = fromIntegral ((e `shiftR` 8) .&. 255)
  radix 2 e = fromIntegral ((e `shiftR` 16) .&. 255)
  radix 3 e = fromIntegral (((e `xor` minBound) `shiftR` 24) .&. 255)
  {-# INLINE radix #-}

instance Radix Int64 where
  passes _ = 8
  {-# INLINE passes #-}
  size _ = 256
  {-# INLINE size #-}
  radix 0 e = fromIntegral (e .&. 255)
  radix 1 e = fromIntegral ((e `shiftR` 8) .&. 255)
  radix 2 e = fromIntegral ((e `shiftR` 16) .&. 255)
  radix 3 e = fromIntegral ((e `shiftR` 24) .&. 255)
  radix 4 e = fromIntegral ((e `shiftR` 32) .&. 255)
  radix 5 e = fromIntegral ((e `shiftR` 40) .&. 255)
  radix 6 e = fromIntegral ((e `shiftR` 48) .&. 255)
  radix 7 e = fromIntegral (((e `xor` minBound) `shiftR` 56) .&. 255)
  {-# INLINE radix #-}

instance Radix Word where
  passes _ = sizeOf (undefined :: Word)
  {-# INLINE passes #-}
  size _ = 256
  {-# INLINE size #-}
  radix 0 e = fromIntegral (e .&. 255)
  radix i e = fromIntegral ((e `shiftR` (i `shiftL` 3)) .&. 255)
  {-# INLINE radix #-}

instance Radix Word8 where
  passes _ = 1
  {-# INLINE passes #-}
  size _ = 256
  {-# INLINE size #-}
  radix _ = fromIntegral
  {-# INLINE radix #-}

instance Radix Word16 where
  passes _ = 2
  {-# INLINE passes #-}
  size   _ = 256
  {-# INLINE size #-}
  radix 0 e = fromIntegral (e .&. 255)
  radix 1 e = fromIntegral ((e `shiftR` 8) .&. 255)
  {-# INLINE radix #-}

instance Radix Word32 where
  passes _ = 4
  {-# INLINE passes #-}
  size   _ = 256
  {-# INLINE size #-}
  radix 0 e = fromIntegral (e .&. 255)
  radix 1 e = fromIntegral ((e `shiftR` 8) .&. 255)
  radix 2 e = fromIntegral ((e `shiftR` 16) .&. 255)
  radix 3 e = fromIntegral ((e `shiftR` 24) .&. 255)
  {-# INLINE radix #-}

instance Radix Word64 where
  passes _ = 8
  {-# INLINE passes #-}
  size   _ = 256
  {-# INLINE size #-}
  radix 0 e = fromIntegral (e .&. 255)
  radix 1 e = fromIntegral ((e `shiftR` 8) .&. 255)
  radix 2 e = fromIntegral ((e `shiftR` 16) .&. 255)
  radix 3 e = fromIntegral ((e `shiftR` 24) .&. 255)
  radix 4 e = fromIntegral ((e `shiftR` 32) .&. 255)
  radix 5 e = fromIntegral ((e `shiftR` 40) .&. 255)
  radix 6 e = fromIntegral ((e `shiftR` 48) .&. 255)
  radix 7 e = fromIntegral ((e `shiftR` 56) .&. 255)
  {-# INLINE radix #-}

instance (Radix i, Radix j) => Radix (i, j) where
  passes ~(i, j) = passes i + passes j
  {-# INLINE passes #-}
  size   ~(i, j) = size i `max` size j
  {-# INLINE size #-}
  radix k ~(i, j) | k < passes j = radix k j
                     | otherwise    = radix (k - passes j) i
  {-# INLINE radix #-}

-- | Sorts an array based on the Radix instance.
sort :: forall e m v. (PrimMonad m, MVector v e, Radix e)
     => v (PrimState m) e -> m ()
sort arr = sortBy (passes e) (size e) radix arr
 where
 e :: e
 e = undefined
{-# INLINE sort #-}

-- | Radix sorts an array using custom radix information
-- requires the number of passes to fully sort the array,
-- the size of of auxiliary arrays necessary (should be
-- one greater than the maximum value returned by the radix
-- function), and a radix function, which takes the pass
-- and an element, and returns the relevant radix.
sortBy :: (PrimMonad m, MVector v e)
       => Int               -- ^ the number of passes
       -> Int               -- ^ the size of auxiliary arrays
       -> (Int -> e -> Int) -- ^ the radix function
       -> v (PrimState m) e -- ^ the array to be sorted
       -> m ()
sortBy passes size rdx arr = do
  tmp    <- new (length arr)
  count  <- new size
  prefix <- new size
  radixLoop passes rdx arr tmp count prefix
{-# INLINE sortBy #-}

radixLoop :: (PrimMonad m, MVector v e)
          => Int                          -- passes
          -> (Int -> e -> Int)            -- radix function
          -> v (PrimState m) e            -- array to sort
          -> v (PrimState m) e            -- temporary array
          -> PV.MVector (PrimState m) Int -- radix count array
          -> PV.MVector (PrimState m) Int -- placement array
          -> m ()
radixLoop passes rdx src dst count prefix = go False 0
 where
 len = length src
 go swap k
   | k < passes = if swap
                    then body rdx dst src count prefix k >> go (not swap) (k+1)
                    else body rdx src dst count prefix k >> go (not swap) (k+1)
   | otherwise  = when swap (unsafeCopy src dst)
{-# INLINE radixLoop #-}

body :: (PrimMonad m, MVector v e)
     => (Int -> e -> Int)            -- radix function
     -> v (PrimState m) e            -- source array
     -> v (PrimState m) e            -- destination array
     -> PV.MVector (PrimState m) Int -- radix count
     -> PV.MVector (PrimState m) Int -- placement
     -> Int                          -- current pass
     -> m ()
body rdx src dst count prefix k = do
  set count 0
  countLoop k rdx src count
  unsafeWrite prefix 0 0
  prefixLoop count prefix
  moveLoop k rdx src dst prefix
{-# INLINE body #-}

countLoop :: (PrimMonad m, MVector v e)
          => Int -> (Int -> e -> Int)
          -> v (PrimState m) e -> PV.MVector (PrimState m) Int -> m ()
countLoop k rdx src count = go 0
 where
 len = length src
 go i
   | i < len    = unsafeRead src i >>= inc count . rdx k >> go (i+1)
   | otherwise  = return ()
{-# INLINE countLoop #-}

prefixLoop :: (PrimMonad m)
           => PV.MVector (PrimState m) Int -> PV.MVector (PrimState m) Int
           -> m ()
prefixLoop count prefix = go 1 0
 where
 len = length count
 go i pi
   | i < len   = do ci <- unsafeRead count (i-1)
                    let pi' = pi + ci
                    unsafeWrite prefix i pi'
                    go (i+1) pi'
   | otherwise = return ()
{-# INLINE prefixLoop #-}

moveLoop :: (PrimMonad m, MVector v e)
         => Int -> (Int -> e -> Int) -> v (PrimState m) e
         -> v (PrimState m) e -> PV.MVector (PrimState m) Int -> m ()
moveLoop k rdx src dst prefix = go 0
 where
 len = length src
 go i
   | i < len    = do srci <- unsafeRead src i
                     pf   <- inc prefix (rdx k srci)
                     unsafeWrite dst pf srci
                     go (i+1)
   | otherwise  = return ()
{-# INLINE moveLoop #-}

inc :: (PrimMonad m) => PV.MVector (PrimState m) Int -> Int -> m Int
inc arr i = unsafeRead arr i >>= \e -> unsafeWrite arr i (e+1) >> return e
{-# INLINE inc #-}