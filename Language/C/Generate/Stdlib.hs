{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}
-- | Some convenient functions for working with stdlib.
module Language.C.Generate.Stdlib
  ( includeStdlib
  , malloc
  , arrayMalloc
  , free
  ) where

import Language.C.Generate

-- | #include <stdlib.h>
includeStdlib :: Decl ()
includeStdlib = include "<stdlib.h>"

mallocFun :: Int :* () :-> Ptr ()
mallocFun = fun $ trustMe "malloc"

freeFun :: Ptr () :* () :-> ()
freeFun = fun $ trustMe "free"

-- | Allocate memory (sizeof(type)).
malloc :: forall a. Type a
       => RValue (Ptr a)
malloc = cast $ mallocFun $$ sizeof (undefined :: a) :* ()

-- | Allocate an array (sizeof(type) * size).
arrayMalloc :: forall a. Type a
            => RValue Int     -- ^ Size
            -> RValue (Ptr a) -- ^ Pointer to the array
arrayMalloc size = cast $ mallocFun $$
                            (sizeof (undefined :: a) *. size :*) ()

-- | Free some memory.
free :: forall a. Type a => RValue (Ptr a) -> RValue ()
free ptr = freeFun $$ (cast ptr :: RValue (Ptr ())) :* ()
