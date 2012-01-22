{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}
import Language.C.Generate

printint :: Generate (t Int) => t Int -> Stmt r ()
printint n = stmt $ rvalue $ trustMe
           $ "printf(\"%d\\n\", " ++ generate n ++ ")"

program = do
  include "<stdio.h>"
  commentDecl "The factorial function"
  fac :: Int :* () :-> Int
      <- defineNewFunction "fac" ("x" :* ())
                         $ \fac  ( x  :* ())
      -> do
    ifte (x ==. int 0)
      (ret $ int 1)
      (ret $ x *. (fac $$ x -. int 1 :* ()))

  commentDecl "Program entry point"
  makeMain $ \main args -> do
    comment "Call fac with argument 5"
    printint (fac $$ int 5 :* ())
    ret $ lit 0

main = writeFile "Fac.c" $ generate program
