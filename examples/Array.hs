{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}
import Language.C.Generate
import Language.C.Generate.Stdlib

printint :: Generate (t Int) => t Int -> Stmt r ()
printint n = stmt $ rvalue $ trustMe
           $ "printf(\"%d\\n\", " ++ generate n ++ ")"

program = do
  include "<stdio.h>"
  includeStdlib
  commentDecl "Forward declarations!"
  map :: (Int :* () :-> Int) :* Ptr Int :* Int :* () :-> ()
      <- declareFunction "map"
  add5 :: Int :* () :-> Int
      <- declareFunction "add5"

  printArr :: Ptr Int :* Int :* () :-> ()
           <- defineNewFunction "printArr" ("arr" :* "len" :* ()) $
                                \printArr  ( arr  :*  len  :* ()) -> do
    forFromTo "i" (lit 0) (lit 1) len $ \i ->
      printint $ arr !. i

  commentDecl "Program entry point"
  makeMain $ \main args -> do
    comment "Create a function pointer"
    f   <- "f" =. funPtr add5
    len <- "len" =. int 10
    comment "Allocate and initialise the array"
    arr <- "arr" =. arrayMalloc len
    forFromTo "i" (lit 0) (lit 1) len $ \i ->
      arr !. i =: i
    comment "Map our function over the array"
    stmt $ map $$ f :* arr :* len :* ()
    comment "Print the array"
    stmt $ printArr $$ arr :* len :* ()
    comment "Clean up"
    stmt $ free arr
    ret $ lit 0

  defineFunction map ("f" :* "xs" :* "len" :* ()) $
                    \( f  :*  xs  :*  len  :* ()) -> do
    forFromTo "i" (lit 0) (lit 1) len $ \i -> do
      xs !. i =: fun f $$ xs !. i :* ()

  defineFunction add5 ("x" :* ()) $
                     \( x  :* ()) -> do
    ret $ x +. lit 5

main = writeFile "Array.c" $ generate program
