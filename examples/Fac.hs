{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}
import Language.C.Generate

printint :: Generate (t Int) => t Int -> Stmt r ()
printint n = stmt $ rvalue $ trustMe $ "printf(\"%d\\n\", " ++ generate n ++ ")"

program = do
  include "<stdio.h>"
  fac :: Int :* () :-> Int
      <- defineNewFunction "fac" ("x" :* ())
                         $ \fac  ( x  :* ())
      -> do
    ifte (x ==. int 0)
      (ret $ int 1)
      (ret $ x *. (fac $$ x -. int 1 :* ()))

  makeMain $ \main args -> do
    printint (fac $$ int 5 :* ())
    ret $ lit 0

main = writeFile "Fac.c" $ generate program
