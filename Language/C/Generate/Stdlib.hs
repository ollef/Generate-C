{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}
-- | Some convenient functions for working with stdlib
module Language.C.Generate.Stdlib
  ( malloc
  , arrayMalloc
  , free
  ) where

import CGen
import TypeLists

includeStdlib = include "<stdlib.h>"

mallocFun :: Int :* () :-> Ptr ()
mallocFun = Function "malloc"

freeFun :: Ptr () :* () :-> ()
freeFun = Function "free"

malloc :: forall a. Type a => RValue (Ptr a)
malloc = cast $ mallocFun $$ sizeof (undefined :: a) :* ()

arrayMalloc :: forall a. Type a => RValue Int -> RValue (Ptr a)
arrayMalloc size = cast $ mallocFun $$
                            (sizeof (undefined :: a) *. size :*) ()

free :: forall a. Type a => RValue (Ptr a) -> RValue ()
free ptr = freeFun $$ (cast ptr :: RValue (Ptr ())) :* ()
