{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}
-- | Some convenient functions for working with stdlib.
module Language.C.Generate.Stdlib
  ( includeStdlib
  , malloc
  , arrayMalloc
  , free
  ) where

import Language.C.Generate as C

-- | @#include \<stdlib.h\>@
includeStdlib :: Decl ()
includeStdlib = include "<stdlib.h>"

mallocFun :: Function (Int -> IO (Ptr ()))
mallocFun = fun $ trustMe "malloc"

freeFun :: Function (Ptr () -> IO ())
freeFun = fun $ trustMe "free"

-- | Allocate memory (@sizeof(type)@).
malloc :: forall a. Type a
       => RValue (Ptr a)
malloc = cast (call mallocFun $ sizeof (undefined :: a) :: RValue (Ptr ()))

-- | Allocate an array (@sizeof(type) * size@).
arrayMalloc :: forall a t. (Type a, ToRValue t)
            => t Int          -- ^ Size
            -> RValue (Ptr a) -- ^ Pointer to the array
arrayMalloc size =
  cast (call mallocFun $ sizeof (undefined :: a) C.* size :: RValue (Ptr ()))

-- | Free memory.
free :: forall a t. (Type a, ToRValue t)
     => t (Ptr a) -- ^ Pointer to the memory location to free
     -> RValue ()
free ptr = call freeFun $ (cast ptr :: RValue (Ptr ()))
