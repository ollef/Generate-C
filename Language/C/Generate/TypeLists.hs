{-# LANGUAGE TypeFamilies, TypeOperators #-}
-- | Heterogeneously typed lists.
module Language.C.Generate.TypeLists where

infixr 4 :>, |>

data a :> b = a :> b deriving (Eq, Ord, Show)

-- | Sugar for not having to write the null element of these lists
(|>) :: a -> b -> a :> b :> ()
a |> b = a :> b :> ()
