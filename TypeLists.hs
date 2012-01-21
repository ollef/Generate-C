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

type family   Const c a
type instance Const c () = ()
type instance Const c (a :* as) = c :* Const c as

{-
type family   Zip a         b
type instance Zip ()        b         = ()
type instance Zip a         ()        = ()
type instance Zip (a :* as) (b :* bs) = (a, b) :* Zip as bs
-}

{-
class HomoGeneous a b where
instance HomoGeneous a () where
instance HomoGeneous a as => HomoGeneous a (a :. as) where
-}

{-
class Mappable as b where
  tlmap :: (a -> b) -> as -> Const b as
instance Mappable () b where
  tlmap f () = ()
instance Mappable as b => Mappable (a :. as) b where
  tlmap f (x :. xs) = f x :. tlmap f xs
-}

--class Zippable as bs where
  --tlzip :: 
