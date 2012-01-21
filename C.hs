{-# LANGUAGE FlexibleContexts, FlexibleInstances
           , MultiParamTypeClasses
           , ScopedTypeVariables
           , TypeFamilies
           , TypeOperators #-}
-- | A reasonably typesafe C code generation DSL
module C.C where

import Control.Monad.Writer
import Data.List
import Data.Char

import TypeLists

(<+>) :: String -> String -> String
x <+> "" = x
x <+> y  = x ++ " " ++ y
parens :: String -> String
parens s = "(" ++ s ++ ")"
tuple :: [String] -> String
tuple ss = parens $ intercalate ", " ss

-------------------------------------------------------------------------------
-- Haskell to C types
-------------------------------------------------------------------------------
data Ptr a
newtype LValue a = LValue {unLValue :: String} deriving Show
newtype RValue a = RValue {unRValue :: String} deriving Show
newtype a :-> b = Function {unFunction :: String}
  deriving (Eq, Ord, Show)
infixr 3 :->

-- | Give the type of something.
--   Invariant: The argument is not inspected.
--   Since you have to put the variable name in the middle of the type when
--   declaring function pointer variables this also takes a String argument
--   which is put in the right position for each type.
class Type a where
  typeOf :: a -> String -> String
instance Type Int where
  typeOf _ v = "int" <+> v
instance Type Char where
  typeOf _ v = "char" <+> v
instance Type Bool where
  typeOf _ v = "int" <+> v
instance Type () where
  typeOf _ v = "void" <+> v
instance Type a => Type (Ptr a) where
  typeOf _ v = typeOf (undefined :: a) "" ++ "*" <+> v
instance Type a => Type [a] where
  typeOf _ v = typeOf (undefined :: a) "" ++ "*" <+> v
instance Type a => Type (LValue a) where
  typeOf _ = typeOf (undefined :: a)
instance Type a => Type (RValue a) where
  typeOf _ = typeOf (undefined :: a)
instance (TypeListTypes as, Type b) => Type (as :-> b) where
  typeOf _ v = typeOf (undefined :: b) "" <+> parens ("*" ++ v ++ ")") ++ tuple (typeListTypes (undefined :: as))
-- instance Type a => Type (Name a) where
  -- typeOf _ = typeOf (undefined :: a)
  

class TypeListTypes a where
  typeListTypes :: a -> [String]
instance TypeListTypes () where
  typeListTypes _ = []
instance (Type a, TypeListTypes as) => TypeListTypes (a :* as) where
  typeListTypes _ = typeOf (undefined :: a) "" : typeListTypes (undefined :: as)

-------------------------------------------------------------------------------
-- RValue can be treated as a number
-------------------------------------------------------------------------------
instance Eq (RValue Int) where
  (==) = undefined

instance Num (RValue Int) where
  (+) = combine "+"
  (-) = combine "-"
  (*) = combine "*"
  fromInteger = RValue . show
  abs (RValue x)    = undefined -- TODO
  signum (RValue x) = undefined

combine op (RValue x) (RValue y) = RValue ("(" ++ x <+> op <+> y ++ ")")

eq :: RValue a -> RValue a -> RValue Int
eq (RValue a) (RValue b) = RValue ("(" ++ a <+> "==" <+> b ++ ")")

-------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------
class FunctionArgs a where
  functionArgs :: a -> [String]
instance FunctionArgs () where
  functionArgs () = []
instance FunctionArgs b => FunctionArgs (RValue a :* b) where
  functionArgs (RValue x :* xs) = x : functionArgs xs

($$) :: FunctionArgs (TMap RValue as)
     => as :-> b -> TMap RValue as -> RValue b
f $$ as = RValue $ unFunction f ++ parens (intercalate ", " $ functionArgs as)
infixr 0 $$

test :: Int :* Int :* () :-> Int
test = Function "test"

test2 :: RValue Int
test2 = test $$ 2 :* 3 :* ()

class FunctionParams s as z | s z -> as where
  functionParams :: s -> as -> z
instance FunctionParams () () () where
  functionParams () _ = ()
instance FunctionParams ss as os
      => FunctionParams (String :* ss) (a :* as) (LValue a :* os) where
  functionParams (s :* ss) _ = LValue s :* functionParams ss (undefined :: as)

defineFunction :: forall as b.
               ( TypeListTypes as
               , FunctionParams (Const String as) as (TMap LValue as))
               => Const String as -> (TMap LValue as -> String)
               -> Decl (as :-> Bool)
defineFunction args f = Function
                      $ f (functionParams args (undefined :: as)) ++ concat (typeListTypes (undefined :: as))

tester :: Int :* Char :* () :-> Bool
tester = defineFunction ("x" :* "y" :* ()) $
                       \( x  :*  y  :* ()) ->
                       unLValue y

{-

newtype Function a b = Function {unFunction :: String}
type TopDecl   = Writer String
type Statement = Writer String
newtype Name a = Name {unName :: String}
  deriving Show
newtype Ptr a = Ptr a
  deriving Show

(<+>) :: String -> String -> String
x <+> y = x ++ " " ++ y
(<$>) :: String -> String -> String
x <$> y = x ++ "\n" ++ y

indent :: String -> String
indent = unlines . map ("  " ++) . lines

class Type a where
  typeOf :: a -> String

instance Eq (RValue Int) where
  (==) = undefined

instance Num (RValue Int) where
  (+) = combine "+"
  (-) = combine "-"
  (*) = combine "*"
  fromInteger = RValue . show
  abs (RValue x)    = undefined
  signum (RValue x) = undefined

eq :: RValue a -> RValue a -> RValue Int
eq (RValue a) (RValue b) = RValue ("(" ++ a <+> "==" <+> b ++ ")")

combine op (RValue x) (RValue y) = RValue ("(" ++ x <+> op <+> y ++ ")")

instance Type Int where
  typeOf _ = "int"
instance Type Char where
  typeOf _ = "char"
instance Type Bool where
  typeOf _ = "int"
instance Type () where
  typeOf _ = "()"
instance Type a => Type (Ptr a) where
  typeOf _ = typeOf (undefined :: a) ++ "*"
instance Type a => Type [a] where
  typeOf _ = typeOf (undefined :: a) ++ "*"
instance Type a => Type (LValue a) where
  typeOf _ = typeOf (undefined :: a)
instance Type a => Type (RValue a) where
  typeOf _ = typeOf (undefined :: a)
instance Type a => Type (Name a) where
  typeOf _ = typeOf (undefined :: a)

genC :: TopDecl a -> (a, String)
genC = runWriter

mkName :: String -> Name a
mkName = Name . sanify
  -- Some names are reserved. Hack them away!
  where sanify "main"    = "_main"
        sanify "return"  = "_return"
        sanify "const"  = "_const"
        sanify ('.':xs)  = "_"  ++ sanify xs
        sanify ('\'':xs) = "_p" ++ sanify xs
        sanify (x:xs) | allowed x = x : sanify xs
                      | otherwise = "_" ++ show (ord x) ++ sanify xs
        sanify []     = []
        allowed x = between 'a' x 'z'
                 || between 'A' x 'Z'
                 || between '0' x '9'
                 || x == '_'
          where between x a y = ord x <= ord a && ord a <= ord y

scope :: Statement () -> Statement ()
scope stmt = do
  tell "{\n"
  tell $ indent $ execWriter stmt
  tell "}\n"

newVarAss :: Type a => Name a -> RValue a -> Statement (LValue a)
newVarAss n@(Name name) (RValue x) = do
  tell $ typeOf n <+> name <+> "=" <+> x ++ ";\n"
  return (LValue name)

newVar :: Type a => Name a -> Statement (LValue a)
newVar n@(Name name) = do
  tell $ typeOf n <+> name ++ ";\n"
  return $ LValue name

newGlobal :: Type a => Name a -> TopDecl (LValue a)
newGlobal n@(Name name) = do
  tell $ typeOf n <+> name ++ ";\n"
  return $ LValue name

exitFailure :: RValue a
exitFailure = RValue "exit(EXIT_FAILURE)\n"

mkMain :: Statement () -> TopDecl ()
mkMain stmts = tell $ unlines
  [ "int main(int argc, char **argv) {"
  , indent $ execWriter $ stmts >> creturn (0 :: RValue Int)
  , "}"
  ]


funDef :: (Type a, Type b) => Name a -> [Name b] -> ([RValue b] -> Statement ()) -> TopDecl (Function [b] a)
funDef name params fbody = do
  tell $ typeOf name <+> unName name
         ++ "(" ++ concat (intersperse ", " [typeOf param <+> unName param | param <- params])
         ++ ")" <+> execWriter (scope $ fbody $ map nameToRValue params)
  return $ Function $ unName name
  where
    nameToRValue :: Name a -> RValue a
    nameToRValue = RValue . unName

funDecl :: (Type a, Type b) => Name a -> [Name b] -> TopDecl (Function [b] a)
funDecl name params = do
  tell $ typeOf name <+> unName name
         ++ "(" ++ concat (intersperse ", " [typeOf param <+> unName param | param <- params])
         ++ ");\n"
  return $ Function $ unName name

staticFunCall :: forall a b. (Type a, Type b)
              => Name (Function [b] a) -> [RValue b] -> RValue a
staticFunCall (Name f) args = funCall (Function f :: Function [b] a) args

printInt :: RValue Int -> Statement ()
printInt (RValue n) = tell $ "printf(\"%d\", " ++ n ++ ");\n"

cputChar :: RValue Int -> Statement ()
cputChar (RValue n) = tell $ "putchar(" ++ n ++ ");\n"

funCall :: (Type a, Type b) => Function [b] a -> [RValue b] -> RValue a
funCall (Function name) args = RValue $ name
  ++ "(" ++ concat (intersperse ", " $ map unRValue args) ++ ")"

castFunCall :: forall a b c. (Type b, Type c) => RValue a -> [RValue b] -> RValue c
castFunCall (RValue f) args = funCall (Function f') args
  where f' = "((" ++ typeOf (undefined :: c) ++ "(*)" ++
                "(" ++ concat (intersperse ", " $ map typeOf args) ++ ")"
            ++ ")" ++ f ++ ")"

rvalue :: LValue a -> RValue a
rvalue (LValue x) = RValue x

stmt :: RValue a -> Statement ()
stmt (RValue x) = tell $ x ++ ";\n"

(=:) :: LValue a -> RValue a -> Statement ()
LValue x =: RValue y = tell $ x <+> "=" <+> y ++ ";\n"

deref :: RValue (Ptr a) -> LValue a
deref (RValue s) = LValue $ "(*" ++ s ++ ")"

ref :: RValue a -> RValue (Ptr a)
ref (RValue s) = RValue $ "(&" ++ s ++ ")"

sizeof :: Type a => a -> String
sizeof x = "sizeof(" ++ typeOf x ++ ")"

malloc :: forall a. Type a => RValue (Ptr a)
malloc = RValue $ "malloc(" ++ sizeof (undefined :: a) ++ ")"

arrayMalloc :: forall a. Type a => Int -> RValue (Ptr a)
arrayMalloc n = RValue $ "malloc(" ++ sizeof (undefined :: a) <+> "*" <+> show n ++ ")"
gcMalloc :: forall a. Type a => RValue (Ptr a)
gcMalloc = RValue $ "GC_MALLOC(" ++ sizeof (undefined :: a) ++ ")"


gcArrayMalloc :: forall a. Type a => Int -> RValue (Ptr a)
gcArrayMalloc n = RValue $ "GC_MALLOC(" ++ sizeof (undefined :: a) <+> "*" <+> show n ++ ")"

arrayIndex :: LValue (Ptr a) -> Int -> LValue a
arrayIndex (LValue x) n = LValue (x ++ "[" ++ show n ++ "]")

arrayIndexR :: RValue (Ptr a) -> Int -> RValue a
arrayIndexR (RValue x) n = RValue (x ++ "[" ++ show n ++ "]")

creturn :: RValue a -> Statement ()
creturn (RValue x) = tell $ "return" <+> x ++ ";\n"

cbreak :: Statement ()
cbreak = tell $ "break;\n"

cast :: forall a b. Type b => RValue a -> RValue b
cast (RValue x) = RValue $ "(" ++ typeOf (undefined :: b) ++ ")(" ++ x ++ ")"

castFun :: forall a b c. Type c => Function [b] a -> RValue c
castFun (Function f) = RValue $ "(" ++ typeOf (undefined :: c) ++ ")(" ++ f ++ ")"

switch :: RValue Int -> [(Int, Statement ())] -> Statement () -> Statement ()
switch (RValue x) cases defcase = do
  tell $ "switch(" ++ x ++ ") "
  scope $ do
    forM_ cases $ \ (i, stmt) -> do
      tell $ "case" <+> show i ++ ": "
      scope stmt
    tell $ "default: "
    scope defcase
    -}
