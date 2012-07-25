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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Array.Nikola.Backend.CUDA (
    module Data.Array.Nikola.Backend.CUDA.Target,
    module Data.Array.Nikola.Embed,
    module Data.Array.Nikola.Language.Smart,
    module Data.Array.Nikola.Language.Syntax,

    ROpts(..),
    compile,
    compileIO,
    compileTH,
    compileTHEx,
    withCompiledFunction,
    withCompiledCFun,

    Exp,
    withNewContext
  ) where

import Prelude hiding (catch)

import Control.Exception
import qualified Foreign.CUDA.Driver as CU

import Data.Array.Nikola.Backend.CUDA.Compile
import Data.Array.Nikola.Backend.CUDA.Target
import qualified Data.Array.Nikola.Embed as Embed
import Data.Array.Nikola.Embed hiding (Exp)
import Data.Array.Nikola.Language.Reify
import Data.Array.Nikola.Language.Smart
import Data.Array.Nikola.Language.Syntax

type Exp a = Embed.Exp CUDA a

withNewContext :: (CU.Context -> IO a) -> IO a
withNewContext kont = do
    CU.initialise []
    ndevs <- CU.count
    bracket (ctxCreate 0 ndevs) CU.destroy kont
  where
    ctxCreate :: Int -> Int -> IO CU.Context
    ctxCreate i n | i >= n = CU.cudaError "Can't create a context"
    ctxCreate i n =
        (CU.device i >>= \dev -> CU.create dev [])
      `catch` \(_ :: CU.CUDAException) -> ctxCreate (i+1) n