{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}
import Prelude hiding ((+), (==), (*), (-))
import Language.C.Generate

printint :: Val lr Int -> Stmt r ()
printint n = stmt $ trustMe
           $ "printf(\"%d\\n\", " ++ generate n ++ ")"

program = do
  include "<stdio.h>"

  comment "The factorial function"
  fac :: Fun (Int -> IO Int)
      <- defineNewFun "fac" "x" $ \fac x -> do
    ifte (x == lit 0)
      (ret $ lit 1)
      (ret $ x * call fac (x - lit 1))

  comment "Program entry point"
  makeMain $ \main argc argv -> do
    comment "Call fac with argument 5"
    printint (call fac $ lit 5)
    ret $ lit 0

main = writeFile "Fac.c" $ generate program
