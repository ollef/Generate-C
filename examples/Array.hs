{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}
import Language.C.Generate as C
import Language.C.Generate.Stdlib

printint :: Generate (t Int) => t Int -> Stmt r ()
printint n = stmt $ rvalue $ trustMe
           $ "printf(\"%d\\n\", " ++ generate n ++ ")"

cprintArr :: LValue (Ptr Int) -> LValue Int -> Stmt () ()
cprintArr arr len = do
  forFromTo "i" (lit 0) (lit 1) len $ \i ->
    printint $ arr ! i

cmap :: LValue (Function (Int -> IO Int)) -> LValue (Ptr Int) -> LValue Int -> Stmt () ()
cmap f xs len = do
    forFromTo "i" (lit 0) (lit 1) len $ \i -> do
      xs ! i =: call (fun f) (xs ! i)

program = do
  include "<stdio.h>"
  includeStdlib
  commentDecl "Forward declarations!"
  map :: Function (Function (Int -> IO Int) -> Ptr Int -> Int -> IO ())
      <- declareFunction "map"
  add5 :: Function (Int -> IO Int)
      <- declareFunction "add5"

  printArr :: Function (Ptr Int -> Int -> IO ())
           <- defineNewFunction "printArr" ("arr" |> "len") $
                                \rec -> cprintArr

  commentDecl "Program entry point"
  makeMain $ \main argc argv -> do
    comment "Create a function pointer"
    f   <- "f" =. funPtr add5
    len <- "len" =. int 10
    comment "Allocate and initialise the array"
    arr <- "arr" =. arrayMalloc len
    forFromTo "i" (lit 0) (lit 1) len $ \i ->
      arr ! i =: i
    comment "Map our function over the array"
    stmt $ call map f arr len
    comment "Print the array"
    stmt $ call printArr arr len
    comment "Clean up"
    stmt $ free arr
    ret $ lit 0

  defineFunction map ("f" :> "xs" |> "len") cmap

  defineFunction add5 ("x" :> ()) $
                     \  (x :: LValue Int) -> do
    ret $ x C.+ lit 5

main = writeFile "Array.c" $ generate program
