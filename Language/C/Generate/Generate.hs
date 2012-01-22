{-# LANGUAGE EmptyDataDecls
           , FlexibleContexts
           , FlexibleInstances
           , FunctionalDependencies
           , GeneralizedNewtypeDeriving
           , MultiParamTypeClasses
           , ScopedTypeVariables
           , TypeOperators
           , UndecidableInstances #-}
-- | A reasonably typesafe embedded C code generation DSL.
module Language.C.Generate.Generate where
import Prelude hiding ((<), (+))

import Control.Monad
import Control.Monad.Trans.Writer
import Data.List

import Language.C.Generate.TypeLists

-------------------------------------------------------------------------------
-- Fixity declarations
-------------------------------------------------------------------------------
infixl 9 !
infixl 8 *, /
infixl 7 +, -
infix  6 ==, /=, <, >, <=, >=
infix  5 &&
infix  4 ||
infix  2 =., =:

-------------------------------------------------------------------------------
-- Helper functions
-------------------------------------------------------------------------------
(<+>) :: String -> String -> String
x <+> "" = x
x <+> y  = x ++ " " ++ y
parens :: String -> String
parens s = "(" ++ s ++ ")"
brackets :: String -> String
brackets s = "[" ++ s ++ "]"
tuple :: [String] -> String
tuple ss = parens $ intercalate ", " ss

class Monad m => MonadEmit m where
  runEmit :: m a -> (a, String)
  emit    :: String -> m ()

emitLn :: MonadEmit m => String -> m ()
emitLn x = emit $ x ++ "\n"

braces :: MonadEmit m => m () -> m ()
braces x = do
  emitLn "{"
  indent x
  emitLn "}"

indent :: MonadEmit m => m () -> m ()
indent = emit . unlines . map ('\t' :) . lines . snd . runEmit

-------------------------------------------------------------------------------
-- Code generation
-------------------------------------------------------------------------------
class Generate a where
  -- | Generate C code from something. Using it for anything other than 'Decl'
  -- is mostly useful for debugging purposes (or for use with 'trustMe').
  generate :: a -> String
instance Generate (RValue a) where
  generate = unRValue
instance Generate (LValue a) where
  generate = unRValue
instance Generate (Stmt r a) where
  generate = snd . runEmit
instance Generate (Decl a) where
  generate = snd . runEmit

-------------------------------------------------------------------------------
-- Haskell to C types
-------------------------------------------------------------------------------
-- | C values.
newtype Val lr a = Val {unVal :: String} deriving Show
-- | Values that can be used on the left-hand side of assignments.
data L
-- | Values that can't be assigned
data R

{-
class ToRValue t where
  -- | Conversion to 'RValue's.
  rvalue :: t a -> RValue a
instance ToRValue RValue where rvalue = id
instance ToRValue LValue where rvalue (LValue x) = RValue x
unRValue :: ToRValue t => t a -> String
unRValue x = case rvalue x of
  RValue s -> s
-}

-- | Pointer type.
data Ptr a
-- | Function type.
newtype Function as = Function {unFunction :: String}

-- | Give the type of something.
--   Invariant: The argument is not inspected.
--   Since you have to put the variable name in the middle of the type when
--   declaring function pointer variables this also takes a String argument
--   which is put in the variable position for each type.
class Type a where
  typeOf :: a -> String -> String
instance Type () where
  typeOf _ v = "void" <+> v
instance Type Int where
  typeOf _ v = "int" <+> v
instance Type Char where
  typeOf _ v = "char" <+> v
instance Type Float where
  typeOf _ v = "float" <+> v
instance Type Double where
  typeOf _ v = "double" <+> v
instance Type a => Type (Ptr a) where
  typeOf _ v = typeOf (undefined :: a) "" ++ "*" <+> v
instance Type a => Type (LValue a) where
  typeOf _ = typeOf (undefined :: a)
instance Type a => Type (RValue a) where
  typeOf _ = typeOf (undefined :: a)
instance FunctionType a => Type (Function a) where
  typeOf _ v = res "" <+> parens ("*" ++ v) ++ tuple (map ($ "") params)
    where (params, res) = functionTypeView (undefined :: a)

-- | Get a list of type functions from a function type
class FunctionType a where
  functionType :: a -> [String -> String]
instance Type a => FunctionType (IO a) where
  functionType _ = [typeOf (undefined :: a)]
instance (InhabitedType a, FunctionType b) => FunctionType (a -> b) where
  functionType _ = typeOf (undefined :: a) : functionType (undefined :: b)

-- | Get the parameter and return types from a function type list
functionTypeView :: forall a. FunctionType a
                 => a -> ([String -> String], String -> String)
functionTypeView _ = (init types, last types)
  where types = functionType (undefined :: a)

-- | The subset of 'Type's which have values.
class Type a => InhabitedType a where
instance InhabitedType Int where
instance InhabitedType Char where
instance InhabitedType Float where
instance InhabitedType Double where
instance Type a => InhabitedType (Ptr a) where
instance FunctionType a => InhabitedType (Function a) where

------------------------------------------------------------------------------
-- Expressions
------------------------------------------------------------------------------
-- | Size of a datatype.
sizeof :: Type t => t -> RValue Int
sizeof x = RValue $ "sizeof" ++ parens (typeOf x "")

-- | Conditional (@predicate ? true : false@).
cond :: (Type a, ToRValue t, ToRValue t', ToRValue t'')
     => t Int    -- ^ Predicate
     -> t' a     -- ^ True expression
     -> t'' a    -- ^ False expression
     -> RValue a
cond p t f = RValue $ parens
           $ unRValue p <+> "?" <+> unRValue t <+> ":" <+> unRValue f

-------------------------------------------------------------------------------
-- Pointers
-- | Get the address of a value.
address :: ToRValue t => t a -> LValue (Ptr a)
address x = LValue $ parens $ "&" ++ unRValue x

-- | Dereference a pointer.
deref :: (InhabitedType a, ToRValue t) => t (Ptr a) -> LValue a
deref x = LValue $ parens $ "*" ++ unRValue x

-- | Function to function pointer.
funPtr :: Function a -> RValue (Function a)
funPtr (Function name) = RValue name

-- | Function pointer to function.
fun :: ToRValue t => t (Function a) -> Function a
fun x = Function $ unRValue x

-- | Null pointer.
nullPtr :: RValue (Ptr a)
nullPtr = RValue "0"

-- | Array indexing.
(!) :: (ToRValue t, ToRValue t')
    => t (Ptr a) -> t' Int -> LValue a
arr ! i = LValue $ unRValue arr ++ brackets (unRValue i)

-------------------------------------------------------------------------------
-- Type casting
-- | Type cast.
cast :: forall t a b. (Type b, ToRValue t) => t a -> RValue b
cast x = RValue $ parens $ parens (typeOf (undefined :: b) "") ++ (unRValue x)

-- | Type cast a function.
castFun :: (FunctionType a, FunctionType b) => Function a -> Function b
castFun = fun . cast . funPtr

-------------------------------------------------------------------------------
-- Numeric
-- | The subset of 'Type's which you can do numeric operations on and create
--   constant literals of.
class InhabitedType a => NumType a where
  -- | Create a C literal from a Haskell value.
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
-- | Bool literal.
bool :: Bool -> RValue Int
bool False = lit 0
bool True  = lit 1

boolbinop :: (Type a, ToRValue t, ToRValue t')
          => String -> t a -> t' a -> RValue Int
boolbinop op x y = RValue $ parens $ unRValue x <+> op <+> unRValue y

(==), (/=)
  :: (Type a, ToRValue t, ToRValue t')
  => t a -> t' a -> RValue Int
(==) = boolbinop "=="
(/=) = boolbinop "!="

(<), (>), (<=), (>=)
  :: (NumType a, ToRValue t, ToRValue t')
  => t a -> t' a -> RValue Int
(<) = boolbinop "<"
(>) = boolbinop ">"
(<=) = boolbinop "<="
(>=) = boolbinop ">="

(&&), (||)
  :: (ToRValue t, ToRValue t')
  => t Int -> t' Int -> RValue Int
(&&) = boolbinop "&&"
(||) = boolbinop "||"

-- | Logical negation (@!x@).
not :: ToRValue t
    => t Int -> RValue Int
not x = RValue $ parens $ "!" ++ unRValue x

binop :: (Type a, ToRValue t, ToRValue t')
      => String -> t a -> t' a -> RValue a
binop op x y = RValue $ parens $ unRValue x <+> op <+> unRValue y

(+), (-), (*), (/)
  :: (NumType a, ToRValue t, ToRValue t')
  => t a -> t' a -> RValue a
(+) = binop "+"
(-) = binop "-"
(*) = binop "*"
(/) = binop "/"

------------------------------------------------------------------------------
-- Function calls
-- | Get a Haskell function taking the arguments from a C function's type
class FunctionArgs funtype restype | funtype -> restype where
  functionArgs :: Function a -> [String]
               -> funtype -> restype
instance FunctionArgs (IO a) (RValue a) where
  functionArgs (Function f) args _ = RValue $ f ++ tuple (reverse args)
instance (FunctionArgs b b', ToRValue t)
      => FunctionArgs (a -> b) (t a -> b') where
  functionArgs f args _ = \val ->
    (functionArgs f (unRValue val : args) (undefined :: b) :: b')

-- | Function calls
call :: forall f res. FunctionArgs f res
     => Function f -- ^ Function
     -> res        -- ^ Arguments and result
call f = functionArgs f [] (undefined :: f)

------------------------------------------------------------------------------
-- Untrusted code
-- | \"Trust me, I know what I'm doing\". Insert whatever code you want as an
--   'LValue' at any type.
trustMe :: String   -- ^ C code
        -> LValue a
trustMe = LValue

------------------------------------------------------------------------------
-- Statements
------------------------------------------------------------------------------
-- | Statements typed with the return type of the function they're in.
newtype Stmt r a = Stmt {unStmt :: Writer String a}
  deriving (Monad)

instance MonadEmit (Stmt r) where
  runEmit = runWriter . unStmt
  emit    = Stmt . tell

-- | Statement from a value.
stmt :: ToRValue t => t a -> Stmt r ()
stmt x = emitLn $ unRValue x ++ ";"

------------------------------------------------------------------------------
-- Return statements
-- | Empty return (for functions of void type).
ret_ :: Stmt () ()
ret_ = emitLn "return;"

-- | Return a value.
ret :: (InhabitedType a, ToRValue t) => t a -> Stmt a ()
ret x = emitLn $ "return" <+> unRValue x ++ ";"

------------------------------------------------------------------------------
-- | Scopes
scope :: Stmt r () -> Stmt r ()
scope = braces

------------------------------------------------------------------------------
-- Variables
-- | Create a variable with an initial value.
(=.) :: forall t a r. (InhabitedType a, ToRValue t)
     => String            -- ^ Variable name
     -> t a               -- ^ Initial value
     -> Stmt r (LValue a) -- ^ Variable
name =. val = do
  emitLn $ typeOf (undefined :: a) name <+> "=" <+> unRValue val ++ ";"
  return $ LValue name

-- | Create a variable with no initial value.
newvar :: forall a r. InhabitedType a
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
-- Conditional statements
-- | If statement with an optional else branch.
iftme :: ToRValue t
      => t Int             -- ^ Predicate
      -> Stmt r ()         -- ^ \"Then\" branch
      -> Maybe (Stmt r ()) -- ^ Optional \"Else\" branch
      -> Stmt r ()
iftme p t mf = do
  emit $ "if" <+> parens (unRValue p) ++ " "
  braces t
  case mf of
    Nothing -> return ()
    Just f  -> do emit $ "else "; braces f

-- | If statement with an else branch.
ifte :: ToRValue t
     => t Int     -- ^ Predicate
     -> Stmt r () -- ^ \"Then\" branch
     -> Stmt r () -- ^ \"Else\" branch
     -> Stmt r ()
ifte p t f = iftme p t (Just f)

-- | If and only if (no else branch).
iff :: ToRValue t
    => t Int     -- ^ Predicate
    -> Stmt r () -- ^ \"Then\" branch
    -> Stmt r ()
iff p t = iftme p t Nothing

-- | Switch statements.
switch :: ToRValue t
       => t Int              -- ^ Value to switch on
       -> [(Int, Stmt r ())] -- ^ Cases
       -> Stmt r ()          -- ^ Default case
       -> Stmt r ()
switch val cases def = do
  emit $ "switch" ++ parens (unRValue val) ++ " "
  braces $ do
    forM_ cases $ \(i, st) -> do
      emit $ "case" <+> show i ++ ": "
      braces st
    emit $ "default: "
    braces def

------------------------------------------------------------------------------
-- Loops
-- | While loops.
while :: ToRValue t
      => t Int     -- ^ Predicate
      -> Stmt r () -- ^ Loop body
      -> Stmt r ()
while p s = do
  emit $ "while" <+> parens (unRValue p) ++ " "
  braces s

-- | For loops (currently desugared to while loops because C's for loops don't
--   fit the model of how expressions contra statements work in this DSL).
for :: ToRValue t
    => Stmt r () -- ^ Initialise
    -> t Int     -- ^ Predicate
    -> Stmt r () -- ^ Increase
    -> Stmt r () -- ^ Loop body
    -> Stmt r ()
for i p inc s = do
  i
  while p (do s; inc)

-- | Loop between a start and an end value increasing by a step value.
forFromTo :: (Num n, NumType n, ToRValue t, ToRValue t', ToRValue t'')
          => String -- ^ Name of the loop variable
          -> t   n  -- ^ Start
          -> t'  n  -- ^ Step
          -> t'' n  -- ^ End
          -> (RValue n -> Stmt r ()) -- ^ Loop body
          -> Stmt r ()
forFromTo v start step end body = braces $ do
  i <- v =. start
  while (i < end) $ do
    body (rvalue i)
    i =: i + step

-- | Break.
break :: Stmt r ()
break = emitLn "break;"

-- | Continue.
continue :: Stmt r ()
continue = emitLn "continue;"

------------------------------------------------------------------------------
-- Declarations (top-level)
------------------------------------------------------------------------------
-- | A top-level declaration
newtype Decl a = Decl {unDecl :: Writer String a}
  deriving (Monad)
instance MonadEmit Decl where
  runEmit = runWriter . unDecl
  emit    = Decl . tell

------------------------------------------------------------------------------
-- Preprocessor directives
-- | Include directive.
include :: String -- ^ Filename; either \"\<file.h\>\" or \"\\\"file.h\\\"\"
        -> Decl ()
include file = emitLn $ "#include" <+> file

------------------------------------------------------------------------------
-- Global variables
declareGlobal :: forall a. InhabitedType a
              => String          -- ^ Global name
              -> Decl (LValue a) -- ^ Global variable
declareGlobal name = do
  emitLn (typeOf (undefined :: a) name)
  return $ LValue name

------------------------------------------------------------------------------
-- Functions
-- | Forward declaration of a function.
declareFunction :: forall f.
  ( FunctionType f
  ) => String            -- ^ Function name
    -> Decl (Function f) -- ^ Resulting function declaration
declareFunction name = do
  emitLn $ res "" <+> name
        ++ tuple (map ($ "") params) ++ ";"
  return $ Function name
  where (params, res) = functionTypeView (undefined :: f)

-- | Get a list of parameter names of a C function.
class FunctionType funtype => FunctionParams names funtype where
  functionParams :: names -> funtype -> [String]
instance Type a => FunctionParams () (IO a) where
  functionParams () _ = []
instance (InhabitedType a, FunctionParams names b)
      => FunctionParams (String :> names) (a -> b) where
  functionParams (n :> ns) _ = n : functionParams ns (undefined :: b)

-- | Get the Haskell function that can be used for defining the body of the
--   C function.
class FunctionDef params fun def res where
  functionDef :: params -> String -> String
              -> fun -> def -> Decl (Function res)
instance FunctionDef () (IO a) (Stmt a ()) res where
  functionDef () f defline _ body = do
    emit defline
    braces $
      emit $ snd . runEmit $ body
    return $ Function f
instance FunctionDef params b def res
      => FunctionDef (String :> params) (a -> b) (LValue a -> def) res where
  functionDef (param :> ps) f defline _ bodyf =
    functionDef ps f defline (undefined :: b) (bodyf $ LValue param)

-- | Define a function that has not been declared before.
defineNewFunction :: forall f def names.
  ( FunctionDef names f def f
  , FunctionParams names f
  ) => String              -- ^ Function name
    -> names               -- ^ Parameter names
    -> (Function f -> def) -- ^ Function from input to function body;
                           --   the function can be used recursively
    -> Decl (Function f)
defineNewFunction name params bodyf =
  functionDef params name defline (undefined :: f) (bodyf $ Function name)
  where paramNames          = functionParams params (undefined :: f)
        (inptypes, restype) = functionTypeView (undefined :: f)
        defline    = restype "" <+> name ++ tuple (zipWith ($) inptypes
                                                               paramNames)

-- | Define a function that has been declared before.
defineFunction :: forall def names f.
  ( FunctionDef names f def f
  , FunctionParams names f
  ) => Function f -- ^ Declared function
    -> names      -- ^ Parameter names
    -> def        -- ^ Function from input to function body
    -> Decl ()    -- ^ Does not return a new function (use the declared one)
defineFunction (Function name) params bodyf = do
  _ <- defineNewFunction name params (const bodyf) :: Decl (Function f)
  return ()

------------------------------------------------------------------------------
-- Main
-- | The type of the main function (@int main(int argc, int** argv)@).
type MainType = Function (Int -> Ptr (Ptr Int) -> IO Int)

-- | Create a function called main with parameters @argc@ and @argv@.
makeMain :: (MainType -> LValue Int
                      -> LValue (Ptr (Ptr Int))
                      -> Stmt Int ()) -- ^ @main -> argc -> argv -> Body@
         -> Decl MainType
makeMain = defineNewFunction "main" ("argc" |> "argv")

------------------------------------------------------------------------------
-- Comments
------------------------------------------------------------------------------
-- | Top-level comment.
commentDecl :: String -> Decl ()
commentDecl s = emitLn $ "/*" <+> s <+> "*/"

-- | Comment in a function.
comment :: String -> Stmt r ()
comment s = emitLn $ "/*" <+> s <+> "*/"
