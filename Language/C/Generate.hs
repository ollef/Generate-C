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
  -- ** Return statements
  , retvoid, ret
  -- ** Scopes
  , scope
  -- ** Variables
  , (=.), newvar, (=:)
  -- ** Conditional
  , ifte, iff, switch
  -- ** Loops
  , while, for, forFromTo, cbreak, continue
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
  -- * Comments
  , commentDecl, comment
  ) where

import Language.C.Generate.Generate
import Language.C.Generate.TypeLists
