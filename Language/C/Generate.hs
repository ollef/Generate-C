-- | A reasonably typesafe embedded C code generation DSL.
module Language.C.Generate
  (
  -- * Code generation
    Generate, generate
  -- * C types
  , LValue, RValue, Ptr, (:->), Type, typeOf, ToRValue, rvalue
  -- * Expressions
  , sizeof, cond
  -- ** Pointers
  , address, deref, funPtr, fun, nullPtr, (!.)
  -- ** Type casting
  , cast, castFun
  -- ** Literals
  , lit, int, float, double, char, bool
  -- ** Numeric
  , (==.), (/=.)
    -- | Equality, inequality.
  , (<.), (>.), (<=.), (>=.)
    -- | Less than, greater than, less than or equal, greater than or equal.
  , (&&.), (||.)
    -- | Boolean and, boolean or.
  , (+.), (-.), (*.), (/.)
    -- | Addition, subtraction, multiplication, division.

  -- ** Function calls
  , ($$)
  -- ** Untrusted code
  , trustMe
  -- * Statements
  , Stmt, stmt
  , retvoid, ret
  -- ** Variables
  , (=.), newvar, (=:)
  -- ** Conditional
  , ifte, iff, switch
  -- ** Loops
  , while, for, cbreak, continue
  -- * Declarations (top-level)
  , Decl
  -- ** Preprocessor directives
  , include
  -- ** Globals
  , declareGlobal
  -- ** Functions
  , declareFunction, defineNewFunction, defineFunction
  -- ** Main
  , MainType, makeMain
  -- * Typed lists
  , (:*)(..)
  ) where

import Language.C.Generate.Generate
import Language.C.Generate.TypeLists
