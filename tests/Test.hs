{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}
module Test where

import Language.C.Generate

test :: Int :* Int :* () :-> Int
test = fun $ trustMe "test"

test2 :: RValue Int
test2 = test $$ int 2 :* int 3 :* ()

tester :: Decl (Int :* Char :* () :-> Int)
tester = do
  f <- declareFunction "test"
  defineFunction f ("x" :* "y" :* ()) $
    \(x :* y :* ()) -> do
      z <- newvar "z"
      z =: lit 13
      ifte (z ==. lit 0)
           (z =: lit 3)
           (z =: lit 4)
      iff  (z ==. lit 2)
           (z =: lit 3)
      while x $ do
        x =: lit 4
        x =: lit 5
      ret $ x +. z ==. x -- x *. x ==. x
  return f

theMain :: Decl MainType
theMain = makeMain $
    \main (argc :* argv :* ()) -> do
      x <- "x" =. funPtr main
      stmt $ fun x $$ argc :* argv :* ()
      switch (lit 0)
        [(0, cbreak)
        ,(1, do
          argc =: lit 2
          cbreak
         )
        ] cbreak
      while (lit 0) continue
      ret $ lit (0 :: Int)

functions = do
  tester
  theMain
  test :: () :-> ()
       <- defineNewFunction "test" () $ \test () -> do
    ptr :: LValue (Ptr ()) <- "ptr" =. nullPtr
    -- x <- "x" =. deref ptr -- Shouldn't work
    retvoid
    -- ret $ deref ptr -- Shouldn't work
  return ()
