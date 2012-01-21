{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , GADTs
           , MultiParamTypeClasses
           , ScopedTypeVariables
           , TypeOperators #-}
-- | A reasonably typesafe C code generation DSL.
module CGen where

import Control.Monad.Identity
import Control.Monad.Trans.Identity
import Control.Monad.Writer
import Data.List
import Data.Char

import TypeLists

-------------------------------------------------------------------------------
-- Fixity declarations
-------------------------------------------------------------------------------
infixl 7 *., /.
infixl 6 +., -.
infix  4 ==., /=.
infix  2 =., =:
infixr 2 :->
infixr 0 $$

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------
(<+>) :: String -> String -> String
x <+> "" = x
x <+> y  = x ++ " " ++ y
parens :: String -> String
parens s = "(" ++ s ++ ")"
tuple :: [String] -> String
tuple ss = parens $ intercalate ", " ss

class MonadWriter String m => MonadEmit m where
  runEmit :: m a -> (a, String)

type Emit a = Writer String a
emit :: MonadEmit m => String -> m ()
emit x   = tell x
emitLn :: MonadEmit m => String -> m ()
emitLn x = emit $ '\n' : x

instance MonadEmit (WriterT String Identity) where
  runEmit = runWriter

braces :: MonadEmit m => m () -> m ()
braces x = do
  emit " {"
  indent x
  emit "}"

indent :: MonadEmit m => m () -> m ()
indent = emit . unlines . map ('\t' :) . lines . snd . runEmit

type TypedT t m a = IdentityT m a
runTypedT :: TypedT t m a -> m a
runTypedT = runIdentityT

-------------------------------------------------------------------------------
-- * Haskell to C types
-------------------------------------------------------------------------------
newtype LValue a = LValue {unLValue :: String} deriving Show
newtype RValue a = RValue String deriving Show
-- | Conversion to RValues.
class ToRValue t where rvalue :: t a -> RValue a
instance ToRValue RValue where rvalue = id
instance ToRValue LValue where rvalue (LValue x) = RValue x
unRValue :: ToRValue t => t a -> String
unRValue x = case rvalue x of
  RValue s -> s
-- | Pointer type.
data Ptr a
-- | Function type.
newtype as :-> b = Function {unFunction :: String}

-- | Give the type of something.
--   Invariant: The argument is not inspected.
--   Since you have to put the variable name in the middle of the type when
--   declaring function pointer variables this also takes a String argument
--   which is put in the variable position for each type.
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
instance Type Float where
  typeOf _ v = "float" <+> v
instance Type Double where
  typeOf _ v = "double" <+> v
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

-- | Give a list of types from a typed list of types.
class TypeListTypes a where
  typeListTypes :: a -> [String]
instance TypeListTypes () where
  typeListTypes _ = []
instance (Type a, TypeListTypes as) => TypeListTypes (a :* as) where
  typeListTypes _ = typeOf (undefined :: a) "" : typeListTypes (undefined :: as)

------------------------------------------------------------------------------
-- * Expressions
------------------------------------------------------------------------------
-- | Get the address of a value.
address :: ToRValue t => t a -> LValue (Ptr a)
address x = LValue $ parens $ "&" ++ unRValue x

-- | Dereference a pointer.
deref :: ToRValue t => t a -> LValue a
deref x = LValue $ parens $ "*" ++ unRValue x

-- | Type cast.
cast :: ToRValue t => t a -> RValue b
cast = RValue . unRValue

-- | Type cast a function.
castFun :: bs :-> a -> bs' :-> a'
castFun = fun . cast . funPtr

-- | Size of a datatype.
sizeof :: Type t => t -> RValue Int
sizeof x = RValue $ "sizeof" ++ parens (typeOf x "")

-- | Conditional (p ? t : f).
cond :: (Type a, ToRValue t, ToRValue t', ToRValue t'')
     => t Int -> t' a -> t'' a -> RValue a
cond pred t f = RValue $ parens
              $ unRValue pred <+> "?" <+> unRValue t <+> ":" <+> unRValue f

-------------------------------------------------------------------------------
-- ** Numeric
-- | The subset of @Type@s which you can do numeric operations on and create
--   constant literals of.
class Type a => NumType a where
  lit :: a -> RValue a
instance NumType Int where
  lit = RValue . show
instance NumType Float where
  lit f = RValue $ show f ++ "f"
instance NumType Double where
  lit = RValue . show
instance NumType Char where
  lit = RValue . show

-- | Int literal.
int :: Int -> RValue Int
int = lit
-- | Float literal.
float :: Float -> RValue Float
float = lit
-- | Double literal.
double :: Double -> RValue Double
double = lit
-- | Char literal.
char :: Char -> RValue Char
char = lit

-- | Equality.
(==.) :: (Type a, ToRValue t, ToRValue t')
      => t a -> t' a -> RValue Int
a ==. b = RValue $ parens $ unRValue a <+> "==" <+> unRValue b

-- | Inequality.
(/=.) :: (Type a, ToRValue t, ToRValue t')
      => t a -> t' a -> RValue Int
a /=. b = RValue $ parens $ unRValue a <+> "!=" <+> unRValue b

binop :: (Type a, ToRValue t, ToRValue t')
      => String -> t a -> t' a -> RValue a
binop op x y = RValue $ parens $ unRValue x <+> op <+> unRValue y

-- | Multiplication.
(*.) :: (Type a, NumType a, ToRValue t, ToRValue t')
     => t a -> t' a -> RValue a
(*.) = binop "*"
-- | Addition.
(+.) :: (Type a, NumType a, ToRValue t, ToRValue t')
     => t a -> t' a -> RValue a
(+.) = binop "+"
-- | Subtraction.
(-.) :: (Type a, NumType a, ToRValue t, ToRValue t')
     => t a -> t' a -> RValue a
(-.) = binop "-"
-- | Division.
(/.) :: (Type a, NumType a, ToRValue t, ToRValue t')
     => t a -> t' a -> RValue a
(/.) = binop "/"

------------------------------------------------------------------------------
-- * Statements
------------------------------------------------------------------------------
-- | Statements typed with the return type of the function they're in.
type Stmt t a = TypedT t (Writer String) a
runStmt :: Stmt t () -> String
runStmt = snd . runEmit . runTypedT

instance MonadEmit m => MonadEmit (IdentityT m) where
  runEmit = runEmit . runIdentityT

-- | Statement from a value.
stmt :: ToRValue t => t a -> Stmt r ()
stmt x = emitLn $ unRValue x ++ ";"

-- ** Return statements
-- | Empty return (for functions of void type).
retvoid :: Stmt (RValue ()) ()
retvoid = emitLn "return;"

-- | Return a value.
ret :: ToRValue t => t a -> Stmt a ()
ret x = emitLn $ "return" <+> unRValue x ++ ";"

------------------------------------------------------------------------------
-- ** Variables
-- | Create a variable with an initial value.
(=.) :: forall t a r. (Type a, ToRValue t)
     => String            -- ^ Variable name
     -> t a               -- ^ Initial value
     -> Stmt r (LValue a) -- ^ Variable
name =. val = do
  emitLn $ typeOf (undefined :: a) name <+> "=" <+> unRValue val ++ ";"
  return $ LValue name

-- | Create a variable with no initial value.
newvar :: forall a r. Type a
    => String            -- ^ Variable name
    -> Stmt r (LValue a) -- ^ Variable
newvar name = do
  emitLn $ typeOf (undefined :: a) name ++ ";"
  return $ LValue name

-- | Variable assignment.
(=:) :: (ToRValue t)
     => LValue a -- ^ Variable
     -> t a      -- ^ Value
     -> Stmt r ()
LValue var =: val = emitLn $ var <+> "=" <+> unRValue val ++ ";"

------------------------------------------------------------------------------
-- ** If statements
-- | If statement with an optional else branch.
iftme :: ToRValue t
      => t Int             -- ^ Predicate
      -> Stmt r ()         -- ^ "Then" branch
      -> Maybe (Stmt r ()) -- ^ Optional "Else" branch
      -> Stmt r ()
iftme p t mf = do
  emitLn $ "if" <+> parens (unRValue p)
  braces t
  case mf of
    Nothing -> return ()
    Just f  -> do emit $ "else"; braces f

-- | If statement with an else branch.
ifte :: ToRValue t
     => t Int     -- ^ Predicate
     -> Stmt r () -- ^ "Then" branch
     -> Stmt r () -- ^ "Else" branch
     -> Stmt r ()
ifte p t f = iftme p t (Just f)

-- | If and only if (no else branch).
iff :: ToRValue t
    => t Int     -- ^ Predicate
    -> Stmt r () -- "Then" branch
    -> Stmt r ()
iff p t = iftme p t Nothing

------------------------------------------------------------------------------
-- ** Loops
-- | While loops.
while :: ToRValue t
      => t Int     -- ^ Predicate
      -> Stmt r () -- ^ Loop body
      -> Stmt r ()
while p s = do
  emitLn $ "while" <+> parens (unRValue p)
  braces s

cbreak :: Stmt r ()
cbreak = emitLn "break;"

continue :: Stmt r ()
continue = emitLn "continue;"

-- | Switch statements.
switch :: ToRValue t
       => t Int              -- ^ Value to switch on
       -> [(Int, Stmt r ())] -- ^ Cases
       -> Stmt r ()          -- ^ Default case
       -> Stmt r ()
switch val cases def = do
  emitLn $ "switch" ++ parens (unRValue val)
  braces $ do
    forM_ cases $ \(i, st) -> do
      emitLn $ "case" <+> show i ++ ":"
      braces st
    emitLn $ "default: "
    braces def

------------------------------------------------------------------------------
-- * (Top-level) Declarations
------------------------------------------------------------------------------
type Decl a = Writer String a
runDecl :: Decl a -> (a, String)
runDecl = runWriter

------------------------------------------------------------------------------
-- ** Functions
-- | Get a list of arguments from a list of RValues.
class FunctionArgs as types where
  functionArgs :: as -> types -> [String]
instance FunctionArgs () () where
  functionArgs () _ = []
instance (ToRValue t, FunctionArgs as types)
      => FunctionArgs (t a :* as) (a :* types) where
  functionArgs (x :* xs) _ = unRValue x
                           : functionArgs xs (undefined :: types)

-- | Function application.
($$) :: forall as types b. FunctionArgs as types
     => types :-> b -> as -> RValue b
f $$ as = RValue $ unFunction f
                ++ tuple (functionArgs as (undefined :: types))

-- | Forward declaration of a function.
declareFunction :: forall ss as b.
  ( TypeListTypes as
  , Type b
  ) => String          -- ^ Function name
    -> Decl (as :-> b) -- ^ Resulting function declaration
declareFunction name = do
  emitLn $ typeOf (undefined :: b) name
        ++ tuple (typeListTypes (undefined :: as)) ++ ";"
  return $ Function name

-- | Get a typed list of LValue parameters with the right names.
class FunctionParams names types params where
  functionParams :: names -> types -> params
instance FunctionParams () () () where
  functionParams () _ = ()
instance FunctionParams names types params
      => FunctionParams (String   :* names)
                        (t        :* types)
                        (LValue t :* params) where
  functionParams (n :* ns) _ = LValue n
                            :* functionParams ns (undefined :: types)

-- | Get a list of function parameter name declarations.
class FunctionDef names types where
  functionDef :: names -> types -> [String]
instance FunctionDef () () where
  functionDef () _ = []
instance (Type t, FunctionDef names types)
      => FunctionDef (String :* names) (t :* types) where
  functionDef (n :* ns) _ = (typeOf (undefined :: t) n)
                          : functionDef ns (undefined :: types)

-- | Define a function that has not been declared before.
defineNewFunction :: forall ss as b.
  ( FunctionDef ss as
  , FunctionParams ss as (TMap LValue as)
  , Type b
  ) => String           -- ^ Function name
    -> ss               -- ^ Parameter names
    -> (  as :-> b       -- ^ The function can be used recursively
       -> TMap LValue as -- ^ Parameters
       -> Stmt b ()      -- ^ Body
       )                -- ^ Function body
    -> Decl (as :-> b)  -- ^ Resulting function definition
defineNewFunction name args f = do
  emitLn $ typeOf (undefined :: b) name
        ++ tuple (functionDef args (undefined :: as))
  braces $
    emit $ runStmt $ f (Function name) $ functionParams args (undefined :: as)
  return $ Function name

-- | Define a function that has been declared before.
defineFunction :: forall ss as b.
  ( FunctionDef ss as
  , FunctionParams ss as (TMap LValue as)
  , Type b
  ) => as :-> b          -- ^ Declared function
    -> ss                -- ^ Parameter names
    -> (  TMap LValue as  -- ^ Parameters
       -> Stmt b ()       -- ^ Body
       )                 -- ^ Function body
    -> Decl ()           -- ^ Does not result in a new function 
                         --   (use the declared one)
defineFunction (Function name) args f = do
  f :: as :-> b <- defineNewFunction name args (const f)
  return ()

-- | The type of the main function (int main(int argc, int** argv)).
type MainType = (Int :* Ptr (Ptr Int) :* ()) :-> Int

-- | Create a function called main with parameters argc and argv.
makeMain :: (MainType
              -> (LValue Int :* LValue (Ptr (Ptr Int)) :* ())
              -> Stmt Int ()
            ) -> Decl MainType
makeMain = defineNewFunction "main" ("argc" :* "argv" :* ())

-- | Function to function pointer.
funPtr :: b :-> as -> RValue (b :-> as)
funPtr (Function name) = RValue name

-- | Function pointer to function.
fun :: ToRValue t => t (b :-> as) -> b :-> as
fun x = Function $ unRValue x

{-
newGlobal :: Type a => Name a -> TopDecl (LValue a)
newGlobal n@(Name name) = do
  tell $ typeOf n <+> name ++ ";\n"
  return $ LValue name

mkMain :: Statement () -> TopDecl ()
mkMain stmts = tell $ unlines
  [ "int main(int argc, char **argv) {"
  , indent $ execWriter $ stmts >> creturn (0 :: RValue Int)
  , "}"
  ]

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

{-
data Constant a = (Num a, NumType a) => Constant a
instance Show a => Show (Constant a) where
  show (Constant x) = "Constant" <+> show x
instance Eq a => Eq (Constant a) where
  Constant x == Constant y = x == y
instance (NumType a, Num a) => Num (Constant a) where
  Constant x + Constant y = Constant $ x + y
  Constant x * Constant y = Constant $ x * y
  Constant x - Constant y = Constant $ x - y
  abs (Constant x)        = Constant $ abs x
  signum (Constant x)     = Constant $ signum x
  fromInteger i           = Constant $ fromInteger i
instance ToRValue Constant where
  rvalue (Constant x) = lit x
-}
