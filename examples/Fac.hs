{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}
import Language.C.Generate as C

printint :: Generate (t Int) => t Int -> Stmt r ()
printint n = stmt $ rvalue $ trustMe
           $ "printf(\"%d\\n\", " ++ generate n ++ ")"

program = do
  include "<stdio.h>"
  commentDecl "The factorial function"
  fac :: Function (Int -> IO Int)
      <- defineNewFunction "fac" ("x" :> ())
                         $ \fac    (x :: LValue Int)  -> do
    ifte (x C.== int 0)
      (ret $ int 1)
      (ret $ x C.* (call fac (x C.- int 1) :: RValue Int))

  commentDecl "Program entry point"
  makeMain $ \main argc argv -> do
    comment "Call fac with argument 5"
    printint (call fac $ int 5 :: RValue Int)
    ret $ lit 0

main = writeFile "Fac.c" $ generate program
