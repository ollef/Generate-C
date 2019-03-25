{-# LANGUAGE EmptyDataDecls
           , FlexibleContexts
           , FlexibleInstances
           , FunctionalDependencies
           , GADTs
           , GeneralizedNewtypeDeriving
           , MultiParamTypeClasses
           , ScopedTypeVariables
           , TypeOperators
           , UndecidableInstances 
           , Rank2Types #-}
-- | A reasonably typesafe embedded C code generation DSL.
module Language.C.Generate.Generate 
    ( module Language.C.Generate.Generate 
    , module Data.Proxy
    ) where
import Prelude hiding ((+), (-), (*), (/),
                       (==), (/=), (<), (>), (<=), (>=),
                       (&&), (||))
import qualified Prelude

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.Char
import Data.List
import qualified Data.Map as M
import Data.Map(Map)
import Data.Maybe
import Data.Monoid
import Data.Proxy

-------------------------------------------------------------------------------
-- Fixity declarations
-------------------------------------------------------------------------------
infixl 9 !
infixl 8 *, /
infixl 7 +, -
infix  6 ==, /=, <, >, <=, >=
infix  5 &&
infix  4 ||
infix  4 :->
infixr 3 :>
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
  runEmit :: m a -> ((a, Map String Int), String)
  emit    :: String -> m ()

emitLn :: MonadEmit m => String -> m ()
emitLn x = emit $ x ++ "\n"

braces :: Stmt r () -> Stmt r ()
braces x = do
  emitLn "{"
  indent x
  emitLn "}"

indent :: Stmt r () -> Stmt r ()
indent m =
  let (((), names), s) = runEmit m
   in do
     Stmt $ put names
     emit (unlines $ map ('\t' :) $ lines s)

-------------------------------------------------------------------------------
-- Code generation
-------------------------------------------------------------------------------
class Generate a where
  -- | Generate C code from something. Using it for anything other than 'Decl'
  -- is mostly useful for debugging purposes (or for use with 'trustMe').
  generate :: a -> String
instance Generate (Val lr a) where
  generate = unVal
instance Generate (Stmt r a) where
  generate = snd . runEmit
instance Generate (Decl a) where
  generate = snd . runEmit

-------------------------------------------------------------------------------
-- Haskell to C types
-------------------------------------------------------------------------------
-- | C values.
newtype Val lr a = Val {unVal :: String} deriving Show

-- | Left or right.
data L
data R
-- | Values that can be used on the left-hand side of assignments.
type LVal a = Val L a
-- | Values that can't be assigned.
type RVal a = Val R a
-- | Convert an LVal to an RVal.
rval :: LVal a -> RVal a
rval (Val x) = Val x

-- | Pointer type.
data Ptr a
-- | Struct type.
data Struct t
-- | Function type.
newtype Fun t = Fun {unFun :: String}

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
instance Type a => Type (Val lr a) where
  typeOf _ = typeOf (undefined :: a)
instance FunType a => Type (Fun a) where
  typeOf _ v = res "" <+> parens ("*" ++ v) ++ tuple (map ($ "") params)
    where (params, res) = funTypeView (undefined :: a)
instance StructClass a => Type (Struct a) where
  typeOf _ v = "struct" <+> structName (undefined :: a) <+> v

-- | Get a list of type functions from a function type
class FunType a where
  funType :: a -> [String -> String]
instance Type a => FunType (IO a) where
  funType _ = [typeOf (undefined :: a)]
instance (InhabitedType a, FunType b) => FunType (a -> b) where
  funType _ = typeOf (undefined :: a) : funType (undefined :: b)

-- | Get the parameter and return types from a function type list
funTypeView :: forall a. FunType a
                 => a -> ([String -> String], String -> String)
funTypeView _ = (init types, last types)
  where types = funType (undefined :: a)

-- | The subset of 'Type's which have values.
class Type a => InhabitedType a where
instance InhabitedType Int where
instance InhabitedType Char where
instance InhabitedType Float where
instance InhabitedType Double where
instance Type a => InhabitedType (Ptr a) where
instance FunType a => InhabitedType (Fun a) where
instance StructClass a => InhabitedType (Struct a) where

------------------------------------------------------------------------------
-- Expressions
------------------------------------------------------------------------------
-- | Size of a datatype.
sizeof :: Type t => t -> RVal Int
sizeof x = Val $ "sizeof" ++ parens (typeOf x "")

-- | Conditional (@predicate ? true : false@).
cond :: Type a
     => Val lr   Int -- ^ Predicate
     -> Val lr'  a   -- ^ True expression
     -> Val lr'' a   -- ^ False expression
     -> RVal a
cond p t f = Val $ parens
           $ unVal p <+> "?" <+> unVal t <+> ":" <+> unVal f

-------------------------------------------------------------------------------
-- Pointers
-- | Get the address of a value.
address :: Val lr a -> RVal (Ptr a)
address x = Val $ parens $ "&" ++ unVal x

-- | Dereference a pointer.
deref :: InhabitedType a => Val lr (Ptr a) -> LVal a
deref x = Val $ parens $ "*" ++ unVal x

-- | Function to function pointer.
funPtr :: Fun a -> RVal (Fun a)
funPtr (Fun name) = Val name

-- | Function pointer to function.
fun :: Val lr (Fun a) -> Fun a
fun x = Fun $ unVal x

-- | Null pointer.
nullPtr :: LVal (Ptr a)
nullPtr = Val "0"

-- | Array indexing.
(!) :: Val lr (Ptr a) -> Val lr' Int -> LVal a
arr ! i = Val $ unVal arr ++ brackets (unVal i)

-------------------------------------------------------------------------------
-- Type casting
-- | Type cast.
cast :: forall lr a b. Type b => Val lr a -> Val lr b
cast x = Val $ parens $ parens (typeOf (undefined :: b) "") ++ (unVal x)

-- | Type cast a function.
castFun :: (FunType a, FunType b) => Fun a -> Fun b
castFun = fun . cast . funPtr

-------------------------------------------------------------------------------
-- Numeric
-- | The subset of 'Type's which you can do numeric operations on and create
--   constant literals of.
class InhabitedType a => NumType a where
  -- | Create a C literal from a Haskell value.
  lit :: a -> RVal a
instance NumType Int where
  lit = Val . show
instance NumType Float where
  lit f = Val $ show f ++ "f"
instance NumType Double where
  lit = Val . show
instance NumType Char where
  lit = Val . show

-- | Int literal.
int :: Int -> RVal Int
int = lit
-- | Float literal.
float :: Float -> RVal Float
float = lit
-- | Double literal.
double :: Double -> RVal Double
double = lit
-- | Char literal.
char :: Char -> RVal Char
char = lit
-- | Bool literal.
bool :: Bool -> RVal Int
bool False = lit 0
bool True  = lit 1

boolbinop :: Type a => String -> Val lr a -> Val lr' a -> RVal Int
boolbinop op x y = Val $ parens $ unVal x <+> op <+> unVal y

(==), (/=)
  :: Type a
  => Val lr a -> Val lr' a -> RVal Int
(==) = boolbinop "=="
(/=) = boolbinop "!="

(<), (>), (<=), (>=)
  :: NumType a
  => Val lr a -> Val lr' a -> RVal Int
(<)  = boolbinop "<"
(>)  = boolbinop ">"
(<=) = boolbinop "<="
(>=) = boolbinop ">="

(&&), (||)
  :: Val lr Int -> Val lr' Int -> RVal Int
(&&) = boolbinop "&&"
(||) = boolbinop "||"

-- | Logical negation (@!x@).
not :: Val lr Int -> RVal Int
not x = Val $ parens $ "!" ++ unVal x

binop :: Type a => String -> Val lr a -> Val lr' a -> RVal a
binop op x y = Val $ parens $ unVal x <+> op <+> unVal y

(+), (-), (*), (/)
  :: NumType a
  => Val lr a -> Val lr' a -> RVal a
(+) = binop "+"
(-) = binop "-"
(*) = binop "*"
(/) = binop "/"

------------------------------------------------------------------------------
-- Strings
-- | Create a string literal from a Haskell string.
string :: String -> RVal (Ptr Char)
string x = Val $ "\"" ++ x ++ "\""

------------------------------------------------------------------------------
-- Function calls
-- | Get a Haskell function taking the arguments from a C function's type
class FunArgs funtype restype | restype -> funtype where
  funArgs :: Fun a -> [String] -> funtype -> restype
instance (a ~ b, lr ~ R) => FunArgs (IO a) (Val lr b) where
  funArgs (Fun f) args _ = Val $ f ++ tuple (reverse args)
instance (a ~ a', FunArgs b b') => FunArgs (a -> b) (Val lr a' -> b') where
  funArgs f args _ = \val ->
    (funArgs f (unVal val : args) (undefined :: b) :: b')

-- | Function calls
call :: forall f res. FunArgs f res
     => Fun f -- ^ Function
     -> res   -- ^ Arguments and result
              --   ('RVal' of the function's return type)
call f = funArgs f [] (undefined :: f)

-- | Get a Haskell function taking the arguments from a C function's type
class SFunArgs funtype r restype | restype -> r where
  sfunArgs :: Fun a -> [String] -> r -> funtype -> restype
instance SFunArgs (IO a) r (Stmt r ()) where
  sfunArgs (Fun f) args _  _ = stmt $ Val $ f ++ tuple (reverse args)
instance (SFunArgs b r b', a' ~ Val lr a) => SFunArgs (a -> b) r (a' -> b') where
  sfunArgs f args r _ = \val ->
    (sfunArgs f (unVal val : args) r (undefined :: b) :: b')

-- | A single function call statement
scall :: forall f r res. SFunArgs f r res
      => Fun f -- ^ Function
      -> res   -- ^ Arguments and result ('Stmt')
scall f = sfunArgs f [] (undefined :: r) (undefined :: f)

------------------------------------------------------------------------------
-- Untrusted code
-- | \"Trust me, I know what I'm doing\". Insert whatever code you want as a 
--   value at any type.
trustMe :: String   -- ^ C code
        -> Val lr a
trustMe = Val

-------------------------------------------------------------------------------
-- Scope handling
-------------------------------------------------------------------------------
type UsedVarsT m a = StateT (Map String Int) m a

freshName :: Monad m => String -> UsedVarsT m String
freshName v = do
  let v' = reverse $ dropWhile isDigit (reverse v)
  when (null v') $ error $ "C.Generate: invalid name: " ++ v
  n <- gets $ fromMaybe 0 . M.lookup v'
  modify $ M.insert v' (n Prelude.+ 1)
  return $ v' ++ if n Prelude.== 0 then "" else show n

inNewScope :: Monad m => UsedVarsT m a -> UsedVarsT m a
inNewScope m = do
  u <- get
  r <- m
  put u
  return r

------------------------------------------------------------------------------
-- Statements
------------------------------------------------------------------------------
-- | Statements typed with the return type of the function they're in.
newtype Stmt r a = Stmt {unStmt :: UsedVarsT (Writer String) a}
  deriving (Applicative, Functor, Monad)

instance MonadEmit (Stmt r) where
  runEmit = runWriter . flip runStateT mempty . unStmt
  emit    = Stmt . lift . tell

-- | Statement from a value.
stmt :: Val lr a -> Stmt r ()
stmt x = emitLn $ unVal x ++ ";"

------------------------------------------------------------------------------
-- Return statements
-- | Empty return (for functions of void type).
ret_ :: Stmt () ()
ret_ = emitLn "return;"

-- | Return a value.
ret :: InhabitedType a => Val lr a -> Stmt a ()
ret x = emitLn $ "return" <+> unVal x ++ ";"

------------------------------------------------------------------------------
-- | Scopes
scope :: Stmt r () -> Stmt r ()
scope = Stmt . inNewScope . unStmt . braces

------------------------------------------------------------------------------
-- Variables
-- | Create a variable with an initial value.
(=.) :: forall lr a r. InhabitedType a
     => String          -- ^ Variable name
     -> Val lr a        -- ^ Initial value
     -> Stmt r (LVal a) -- ^ Variable
name =. val = do
  name' <- Stmt $ freshName name
  emitLn $ typeOf (undefined :: a) name' <+> "=" <+> unVal val ++ ";"
  return $ Val name'

-- | Create a variable with no initial value.
newvar :: forall a r. InhabitedType a
    => String          -- ^ Variable name
    -> Stmt r (LVal a) -- ^ Variable
newvar name = do
  name' <- Stmt $ freshName name
  emitLn $ typeOf (undefined :: a) name' ++ ";"
  return $ Val name'

-- | Variable assignment.
(=:) :: LVal a   -- ^ Variable
     -> Val lr a -- ^ Value
     -> Stmt r ()
Val var =: val = emitLn $ var <+> "=" <+> unVal val ++ ";"

------------------------------------------------------------------------------
-- Conditional statements
-- | If statement with an optional else branch.
iftme :: Val lr Int        -- ^ Predicate
      -> Stmt r ()         -- ^ \"Then\" branch
      -> Maybe (Stmt r ()) -- ^ Optional \"Else\" branch
      -> Stmt r ()
iftme p t mf = do
  emit $ "if" <+> parens (unVal p) ++ " "
  scope t
  case mf of
    Nothing -> return ()
    Just f  -> do emit $ "else "; scope f

-- | If statement with an else branch.
ifte :: Val lr Int -- ^ Predicate
     -> Stmt r ()  -- ^ \"Then\" branch
     -> Stmt r ()  -- ^ \"Else\" branch
     -> Stmt r ()
ifte p t f = iftme p t (Just f)

-- | If and only if (no else branch).
iff :: Val lr Int -- ^ Predicate
    -> Stmt r ()  -- ^ \"Then\" branch
    -> Stmt r ()
iff p t = iftme p t Nothing

-- | Switch statements.
switch :: Val lr Int         -- ^ Value to switch on
       -> [(Int, Stmt r ())] -- ^ Cases
       -> Stmt r ()          -- ^ Default case
       -> Stmt r ()
switch val cases def = do
  emit $ "switch" ++ parens (unVal val) ++ " "
  scope $ do
    forM_ cases $ \(i, st) -> do
      emit $ "case" <+> show i ++ ": "
      scope st
    emit $ "default: "
    scope def

------------------------------------------------------------------------------
-- Loops
-- | While loops.
while :: Val lr Int -- ^ Predicate
      -> Stmt r ()  -- ^ Loop body
      -> Stmt r ()
while p s = do
  emit $ "while" <+> parens (unVal p) ++ " "
  scope s

-- | For loops (currently desugared to while loops because C's for loops don't
--   fit the model of how expressions contra statements work in this DSL).
for :: Stmt r ()  -- ^ Initialise
    -> Val lr Int -- ^ Predicate
    -> Stmt r ()  -- ^ Increase
    -> Stmt r ()  -- ^ Loop body
    -> Stmt r ()
for i p inc s = do
  i
  while p (do s; inc)

-- | Loop between a start and an end value increasing by a step value.
forFromTo :: (Num n, NumType n)
          => String                -- ^ Name of the loop variable
          -> Val lr   n            -- ^ Start
          -> Val lr'  n            -- ^ Step
          -> Val lr'' n            -- ^ End
          -> (RVal n -> Stmt r ()) -- ^ Loop body
          -> Stmt r ()
forFromTo v start step end body = scope $ do
  i <- v =. start
  while (i < end) $ do
    body (rval i)
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
newtype Decl a = Decl {unDecl :: UsedVarsT (Writer String) a}
  deriving (Applicative, Functor, Monad)
instance MonadEmit Decl where
  runEmit = runWriter . flip runStateT mempty . unDecl
  emit    = Decl . lift . tell

stmtToDecl :: Stmt r a -> Decl a
stmtToDecl = Decl . unStmt

------------------------------------------------------------------------------
-- Preprocessor directives
-- | Include directive.
include :: String -- ^ Filename; either \"\<file.h\>\" or \"\\\"file.h\\\"\"
        -> Decl ()
include file = emitLn $ "#include" <+> file

------------------------------------------------------------------------------
-- Global variables
declareGlobal :: forall a. InhabitedType a
              => String        -- ^ Global name
              -> Decl (LVal a) -- ^ Global variable
declareGlobal name = do
  name' <- Decl $ freshName name
  emitLn $ typeOf (undefined :: a) name' ++ ";"
  return $ Val name'

------------------------------------------------------------------------------
-- Lists
-- | Lists of fixed length.
data a :> b = a :> b deriving (Eq, Ord, Show)

class NameList a b | a -> b where
  nameList :: a -> Decl b
instance NameList [Char] (String :> ()) where
  nameList s = do
    s' <- Decl $ freshName s
    return $ s' :> ()
instance NameList () () where
  nameList = return
instance NameList as bs => NameList (String :> as) (String :> bs) where
  nameList (a :> as) = do
    a' <- Decl $ freshName a
    as' <- nameList as
    return $ a' :> as'

------------------------------------------------------------------------------
-- Functions
-- | Forward declaration of a function.
declareFun :: forall f.
  ( FunType f
  ) => String       -- ^ Function name
    -> Decl (Fun f) -- ^ Resulting function declaration
declareFun name = do
  name' <- Decl $ freshName name
  emitLn $ res "" <+> name'
        ++ tuple (map ($ "") params) ++ ";"
  return $ Fun name'
  where (params, res) = funTypeView (undefined :: f)

-- | Get a list of parameter names of a C function.
class FunType funtype => FunParams names funtype where
  funParams :: names -> funtype -> [String]
instance Type a => FunParams () (IO a) where
  funParams () _ = []
instance (InhabitedType a, FunParams names b)
      => FunParams (String :> names) (a -> b) where
  funParams (n :> ns) _ = n : funParams ns (undefined :: b)

-- | Get the Haskell function that can be used for defining the body of the
--   C function.
class FunDef params fun def res | params fun res -> def where
  funDef :: params -> String -> String -> fun -> def -> Decl (Fun res)
instance FunDef () (IO a) (Stmt a ()) res where
  funDef () f defline _ body = do
    emit $ defline ++ " "
    stmtToDecl $ scope $ body
    return $ Fun f
instance FunDef params b def res
      => FunDef (String :> params) (a -> b) (LVal a -> def) res where
  funDef (param :> ps) f defline _ bodyf =
      funDef ps f defline (undefined :: b) (bodyf $ Val param)

-- | Helper function to define a function that has not been declared before.
defineNewFun' :: forall f def nl names.
  ( NameList names nl
  , FunDef nl f def f
  , FunParams nl f
  ) => String         -- ^ Function name
    -> names          -- ^ Parameter names
    -> (Fun f -> def) -- ^ Function from input to function body;
                      --   the function can be used recursively
    -> Decl (Fun f)
defineNewFun' name params bodyf = Decl $ inNewScope $ unDecl $ do
  nl <- nameList params
  funDef nl
         name
         (defline nl)
         (undefined :: f)
         (bodyf $ Fun name)
  where paramNames nl       = funParams nl
                                        (undefined :: f)
        (inptypes, restype) = funTypeView (undefined :: f)
        defline nl = restype "" <+> name ++ tuple (zipWith ($) inptypes
                                                               (paramNames nl))
-- | Define a function that has not been declared before.
defineNewFun :: forall f def nl names.
  ( NameList names nl
  , FunDef nl f def f
  , FunParams nl f
  ) => String         -- ^ Function name
    -> names          -- ^ Parameter names
    -> (Fun f -> def) -- ^ Function from input to function body;
                      --   the function can be used recursively
    -> Decl (Fun f)
defineNewFun name params bodyf = do
  name' <- Decl $ freshName name
  defineNewFun' name' params bodyf

-- | Define a function that has been declared before.
defineFun :: forall def nl names f.
  ( NameList names nl
  , FunDef nl f def f
  , FunParams nl f
  ) => Fun f   -- ^ Declared function
    -> names   -- ^ Parameter names
    -> def     -- ^ Function from input to function body
    -> Decl () -- ^ Does not return a new function (use the declared one)
defineFun (Fun name) params bodyf = do
  _ <- defineNewFun' name params (const bodyf) :: Decl (Fun f)
  return ()

------------------------------------------------------------------------------
-- Main
-- | The type of the main function (@int main(int argc, char** argv)@).
type MainType = Fun (Int -> Ptr (Ptr Char) -> IO Int)

-- | Create a function called main with parameters @argc@ and @argv@.
makeMain :: (MainType -> LVal Int
                      -> LVal (Ptr (Ptr Char))
                      -> Stmt Int ()) -- ^ @main -> argc -> argv -> Body@
         -> Decl MainType
makeMain = defineNewFun "main" ("argc" :> "argv")

------------------------------------------------------------------------------
-- Structs

class StructClass t where
  structName :: t -> String

newtype t :-> a = Field (forall lr. Val lr (Struct t) -> LVal a)

data StructEnd t = StructEnd

class StructBody names fields where
  structBody :: forall proxy. names -> proxy fields -> [String]
instance StructBody () () where
  structBody () _ = []
instance (Type a, StructBody names fields)
      => StructBody ([Char] :> names)
                    (t :-> a :> fields) where
  structBody (n :> ns) _ =
    typeOf (undefined :: a) n : structBody ns (Proxy :: Proxy fields)
instance StructBody ([Char] :> ()) (t :-> a :> ()) =>
         StructBody ([Char] :> ()) (t :-> a) where
  structBody ns _ = structBody ns (Proxy :: Proxy (t :-> a :> ()))

class StructFields struct names fields | fields -> struct where
  structFields :: forall proxy. names -> proxy struct -> fields
instance StructFields t () (StructEnd t) where
  structFields () _ = StructEnd
instance StructFields t names fields
      => StructFields t ([Char] :> names)
                        (t :-> a :> fields) where
  structFields (n :> ns) _ =
    Field (\v -> Val $ unVal v ++ "." ++ n) :> structFields ns (Proxy :: Proxy t)
instance StructFields t ([Char] :> ()) (t :-> a :> StructEnd t) =>
         StructFields t ([Char] :> ()) (t :-> a) where
  structFields ns _ = case structFields ns (Proxy :: Proxy t) :: (t :-> a :> StructEnd t) of
    a :> StructEnd -> a

defineStruct :: forall t nl names fields.
  ( StructClass t
  , NameList names nl
  , StructBody nl fields
  , StructFields t nl fields
  ) => names       -- ^ Field names
    -> Decl fields -- ^ Field accessors
defineStruct ns = do
  emit $ typeOf (undefined :: Struct t) "" ++ " {\n"
  nl <- nameList ns
  stmtToDecl $
    indent $ mapM_ (emitLn . (++ ";"))
           $ structBody nl (Proxy :: Proxy fields)
  emit "};\n"
  return $ structFields nl (Proxy :: Proxy t)

------------------------------------------------------------------------------
-- Comments
------------------------------------------------------------------------------
-- | Comment (works for 'Decl' and 'Stmt').
comment :: MonadEmit m => String -> m ()
comment s = emitLn $ "/*" <+> s <+> "*/"
