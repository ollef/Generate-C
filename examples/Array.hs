{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}
import Prelude hiding ((+))
import Language.C.Generate
import Language.C.Generate.Stdlib

printint :: Val lr Int -> Stmt r ()
printint n = stmt $ trustMe
           $ "printf(\"%d\\n\", " ++ generate n ++ ")"

program = do
  include "<stdio.h>"
  includeStdlib
  commentDecl "Forward declarations!"
  map :: Fun (Fun (Int -> IO Int) -> Ptr Int -> Int -> IO ())
      <- declareFun "map"
  add5 :: Fun (Int -> IO Int)
      <- declareFun "add5"

  printArr :: Fun (Ptr Int -> Int -> IO ())
           <- defineNewFun "printArr" ("arr" |> "len") $ \_ arr len -> do
    forFromTo "i" (lit 0) (lit 1) len $ \i ->
      printint $ arr ! i

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
    scall map f arr len

    comment "Print the array"
    scall printArr arr len

    comment "Clean up"
    free arr

    ret $ lit 0

  defineFun map ("f" :> "xs" |> "len") $ \f xs len -> do
    forFromTo "i" (lit 0) (lit 1) len $ \i -> do
      xs ! i =: call (fun f) (xs ! i)

  defineFun add5 (one "x") $ \x -> do
    ret $ x + lit 5

main = writeFile "Array.c" $ generate program
