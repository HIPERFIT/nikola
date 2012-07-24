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

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Array.Nikola.Reify (
    ROpts(..),
    defaultROpts,

    Reifiable,
    reify,
    reifyEx,

    ReifiableFun,
    reifyLam,

    VApply,
    vapply
  ) where

import Control.Applicative
import Control.Monad.State
import Data.Dynamic
import System.Mem.StableName
import Text.PrettyPrint.Mainland

import Data.Array.Nikola.Language.Check
import Data.Array.Nikola.Language.Syntax
import Data.Array.Nikola.Representable
import Data.Array.Nikola.Reify.Monad

--prettyIO :: (MonadIO m) => Doc -> m ()
--prettyIO = liftIO . putStrLn . pretty 80

-- The main reification function. Note that we DO NOT cache the translations of
-- the function argument to expressions like MapE and ZipWithE because we want
-- the function in its original lambda form---the bodies of the lambda get
-- turned into the body of a loop rather than the body of a function.
reifyE :: DExp
       -> (DExp -> R DExp)
       -> R DExp
reifyE e kont = do
    obSharing <- getObserveSharing
    if obSharing
        then cacheDExp e (go e) kont
        else go e kont
  where
    reifyEs :: [DExp]
            -> ([DExp] -> R DExp)
            -> R DExp
    reifyEs [] kont = kont []

    reifyEs (e:es) kont = do
        reifyE  e  $ \e'  -> do
        reifyEs es $ \es' -> do
        kont (e':es')

    go :: DExp
       -> (DExp -> R DExp)
       -> R DExp
    go e@(VarE {}) kont =
        kont e

    go (DelayedE comp) kont =
        comp     $ \e  -> do
        reifyE e $ \e' -> do
        kont e'

    go (LetE v tau e1 e2) kont = do
        reifyE e1 $ \e1' -> do
        e2' <- extendVars [(v, tau)] $ do
               reifyE e2 kont
        kont $ LetE v tau e1' e2'

    go (LamE vtaus e) kont = do
        e' <- extendVars vtaus $ do
              reifyE e return
        kont $ LamE vtaus e'

    go (AppE e es) kont = do
        reifyE  e  $ \e'  -> do
        reifyEs es $ \es' -> do
        kont $ AppE e' es'

    go e@(BoolE {}) kont =
        kont e

    go e@(IntE {}) kont =
        kont e

    go e@(FloatE {}) kont =
        kont e

    go (UnopE op e) kont = do
        reifyE e $ \e' -> do
        kont $ UnopE op e'

    go (BinopE op e1 e2) kont = do
        reifyE e1 $ \e1' -> do
        reifyE e2 $ \e2' -> do
        kont $ BinopE op e1' e2'

    go (IfteE e1 e2 e3) kont = do
        reifyE e1 $ \e1' -> do
        e2' <- inBranch $ reifyE e2 return
        e3' <- inBranch $ reifyE e3 return
        kont $ IfteE e1' e2' e3'

    go (MapE f e) kont = do
        f' <- reifyBareLam f
        reifyE e $ \e' -> do
        kont $ MapE f' e'

    go (MapME f xs ys) kont = do
        f' <- reifyBareLam f
        reifyE xs $ \xs' -> do
        reifyE ys $ \ys' -> do
        kont $ MapME f' xs' ys'

    go (PermuteE xs is) kont =
        reifyE xs $ \xs' -> do
        reifyE is $ \is' -> do
        kont $ PermuteE xs' is'

    go (PermuteME xs is ys) kont =
        reifyE xs $ \xs' -> do
        reifyE is $ \is' -> do
        reifyE ys $ \ys' -> do
        kont $ PermuteME xs' is' ys'

    go (ZipWithE f e1 e2) kont = do
        f' <- reifyBareLam f
        reifyE e1 $ \e1' -> do
        reifyE e2 $ \e2' -> do
        kont $ ZipWithE f' e1' e2'

    go (ZipWith3E f e1 e2 e3) kont = do
        f' <- reifyBareLam  f
        reifyE e1 $ \e1' -> do
        reifyE e2 $ \e2' -> do
        reifyE e3 $ \e3' -> do
        kont $ ZipWith3E f' e1' e2' e3'

    go (ZipWith3ME f e1 e2 e3 e4) kont = do
        f' <- reifyBareLam f
        reifyE e1 $ \e1' -> do
        reifyE e2 $ \e2' -> do
        reifyE e3 $ \e3' -> do
        reifyE e4 $ \e4' -> do
        kont $ ZipWith3ME f' e1' e2' e3' e4'

    go (ScanE f z e) kont = do
        f' <- reifyBareLam f
        reifyE z $ \z' -> do
        reifyE e $ \e' -> do
        kont $ ScanE f' z' e'

    go (BlockedScanME f z e) kont = do
        f' <- reifyBareLam f
        reifyE z $ \z' -> do
        reifyE e $ \e' -> do
        kont $ BlockedScanME f' z' e'

    go (BlockedNacsME f z e) kont = do
        f' <- reifyBareLam f
        reifyE z $ \z' -> do
        reifyE e $ \e' -> do
        kont $ BlockedNacsME f' z' e'

    go (BlockedAddME xs sums) kont =
        reifyE xs   $ \xs'   -> do
        reifyE sums $ \sums' -> do
        kont $ BlockedAddME xs' sums'

-- Given a "thing" @x@ and a computation @comp@ that computes its translation to
-- a @DExp@, see if we already have a cached translation for @x@. If we do,
-- return it, otherwise compute it via @comp@ and then cache and return the
-- result.
cacheDExp :: Typeable a
          => a
          -> ((DExp -> R DExp) -> R DExp)
          -> (DExp -> R DExp)
          -> R DExp
cacheDExp x comp kont = do
    sn      <- liftIO $ makeStableName $! x
    maybe_e <- lookupStableName sn
    case maybe_e of
      Just e  -> kont e
      Nothing -> do  comp   $ \e  -> do
                     letE e $ \e' -> do
                     insertStableName sn e'
                     kont e'
  where
    letE :: DExp
         -> (DExp -> R DExp)
         -> R DExp
    letE e@(VarE {})   kont = kont e
    letE e@(BoolE {})  kont = kont e
    letE e@(IntE {})   kont = kont e
    letE e@(FloatE {}) kont = kont e

    letE e kont = do
        v    <- gensym "v"
        tau  <- check e
        body <- extendVars [(v, tau)] $
                kont (VarE v)
        return (LetE v tau e body)

-- | The @Reifiable@ class tells us what types can be reified to a @DExp@.
class Reifiable a where
    reify :: a -> IO DExp
    reify = reifyEx defaultROpts

    reifyEx :: ROpts -> a -> IO DExp

-- @DExp@'s and @Exp@'s are reified by running them through @reifyE@.
instance Reifiable DExp where
    reifyEx ropts e = runR ropts (reifyE e return)

instance Reifiable (Exp a) where
    reifyEx ropts = reifyEx ropts . unE

-- Functions that are instances of @ReifiableFun@ are reified by calling
-- @reifyFun@.
instance ReifiableFun a b => Reifiable (a -> b) where
    reifyEx ropts f = runR ropts (reifyFun f >>= reifyBareLam)

reifyLam :: ReifiableFun a b
         => (a -> b)
         -> (DExp -> R DExp)
         -> R DExp
reifyLam f kont = do
    e <- reifyFun f
    reifyE e kont

-- Reification let-binds all non-"simple" intermediate expressions, including
-- lambdas, but when we reify a function we /want/ the lambda. Therefore we
-- unwrap the let binding returned by @reifyE@.
reifyBareLam :: DExp -> R DExp
reifyBareLam f = do
    e <- reifyE f return
    case e of
      LamE {} -> return e
      LetE v _ e1@(LamE {}) (VarE v') | v' == v -> return e1
      _ -> faildoc $ text "Huh!? Reifying a lmbda produced something unexpected!" </> ppr e

-- @shiftLam@ builds a lambda out of a variable binding and a lambda body. If
-- the body is already a lambda, we shift the parameter indices in its bindings
-- by one and build a new, combined lambda. Our intermediate language requires
-- all lambda arguments be tupled, whereas Haskell does not, so we need to play
-- this game due to the fact that we're working with and embedded DSL.
shiftLamE :: Var -> Tau -> DExp -> DExp
shiftLamE v tau (LamE vtaus e) =
    LamE ((v, tau) : vs `zip` taus') e
  where
    (vs, taus) = unzip vtaus
    taus'      = map shiftType taus

    -- @shiftType@ shifts the parameter indices in a type up by 1.
    shiftType :: Tau -> Tau
    shiftType tau@(UnitT {})  = tau
    shiftType tau@(BoolT {})  = tau
    shiftType tau@(IntT {})   = tau
    shiftType tau@(FloatT {}) = tau

    shiftType (ArrayT tau ds ps) =
        ArrayT (shiftType tau) (map shiftN ds) (map shiftN ps)

    shiftType (FunT taus tau) =
        FunT (map shiftType taus) (shiftType tau)

    -- @shiftType@ shifts the parameter indices in an @N@ up by 1.
    shiftN :: N -> N
    shiftN (NDim   i (ParamIdx pi)) = NDim   i (ParamIdx (pi+1))
    shiftN (NPitch i (ParamIdx pi)) = NPitch i (ParamIdx (pi+1))

    shiftN n@(N {})     = n
    shiftN (NAdd n1 n2) = NAdd (shiftN n1) (shiftN n2)
    shiftN (NSub n1 n2) = NSub (shiftN n1) (shiftN n2)
    shiftN (NMul n1 n2) = NMul (shiftN n1) (shiftN n2)
    shiftN (NNegate n)  = NNegate (shiftN n)
    shiftN (NDiv n1 n2) = NDiv (shiftN n1) (shiftN n2)
    shiftN (NMod n1 n2) = NMod (shiftN n1) (shiftN n2)
    shiftN (NMin ns)    = NMin (map shiftN ns)
    shiftN (NMax ns)    = NMax (map shiftN ns)

shiftLamE v tau e =
    LamE [(v, tau)] e

-- | @reifyFun f kont@ reifies the function @f@ and passes the parameters and
-- body of the reified function to @kont@
class (Typeable a, Typeable b) => ReifiableFun a b where
    reifyFun :: (a -> b) -> R DExp

-- The next two instances represent the base cases for function
-- reification. They reify a function by gensym'ing a fresh variable name,
-- passing it to the function, and then reifying the result by calling @reifyE@.
instance (Representable a, Representable b) => ReifiableFun (Exp a)
                                                            (Exp b) where
    reifyFun f = do
        v          <- gensym "x"
        let fOfX   =  (unE . f . E) (VarE v)
        let tau    =  embeddedType (undefined :: a) (ParamIdx 0)
        extendVars [(v, tau)] $ do
        return $ shiftLamE v tau fOfX

instance (Representable a, Representable b) => ReifiableFun (Exp a)
                                                            (IO (Exp b)) where
    reifyFun f = do
        v          <- gensym "x"
        fOfX       <- liftIO $ unE <$> (f . E) (VarE v)
        let tau    =  embeddedType (undefined :: a) (ParamIdx 0)
        extendVars [(v, tau)] $ do
        return $ shiftLamE v tau fOfX

-- This is the inductive case. As with the base cases, we gensym a fresh
-- variable and pass it to @f@ to yield @g@. We reify @g@---itself a
-- function---by calling @reifyFun@.
instance (Representable a, ReifiableFun b c) => ReifiableFun (Exp a)
                                                             (b -> c) where
    reifyFun f = do
        v       <- gensym "x"
        let tau =  embeddedType (undefined :: a) (ParamIdx 0)
        let g   =  f (E (VarE v))
        extendVars [(v, tau)] $ do
        shiftLamE v tau <$> reifyFun g

-- | @vapply@ is a bit tricky... We first build a @DelayedE@ AST node containing
-- an action that reifies the lambda. Then we wrap the result in enough
-- (Haskell) lambdas and (Nikola) @AppE@ constructors to turn in back into a
-- Haskell function (at the original type) whose body is a Nikola application
-- term.
class (ReifiableFun a b) => VApply a b where
    vapply :: (a -> b) -> a -> b
    vapply f = vapplyk (DelayedE (cacheDExp f (reifyLam f))) []

    vapplyk :: DExp -> [DExp] -> a -> b

instance (Representable a, Representable b) => VApply (Exp a) (Exp b) where
    vapplyk f es = \e -> E $ AppE f (reverse (unE e : es))

instance (Representable a, VApply b c) => VApply (Exp a) (b -> c) where
    vapplyk f es = \e -> vapplyk f (unE e : es)