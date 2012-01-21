{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, TypeOperators #-}
-- | Typed lists
module TypeLists where

data a :* b = a :* b deriving (Eq, Ord, Show)
infixr 3 :*

class TypeList a where
instance TypeList ()
instance TypeList as => TypeList (a :* as)

type family   TMap (f :: * -> *) (a :: *)
type instance TMap f ()        = ()
type instance TMap f (a :* as) = f a :* TMap f as
