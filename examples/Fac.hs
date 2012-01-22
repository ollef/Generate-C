{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}
import Language.C.Generate as C

printint :: Val lr Int -> Stmt r ()
printint n = stmt $ trustMe
           $ "printf(\"%d\\n\", " ++ generate n ++ ")"

program = do
  include "<stdio.h>"
  commentDecl "The factorial function"
  fac :: Fun (Int -> IO Int)
      <- defineNewFun "fac" (one "x") $ \fac x -> do
    ifte (x C.== int 0)
      (ret $ int 1)
      (ret $ x C.* call fac (x C.- int 1))

  commentDecl "Program entry point"
  makeMain $ \main argc argv -> do
    comment "Call fac with argument 5"
    printint (call fac $ int 5)
    ret $ lit 0

main = writeFile "Fac.c" $ generate program
