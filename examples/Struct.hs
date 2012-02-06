{-# LANGUAGE EmptyDataDecls, ScopedTypeVariables, TypeOperators #-}
import Prelude hiding ((+), (==), (*), (-))
import Language.C.Generate

printint :: Val lr Int -> Stmt r ()
printint n = stmt $ trustMe
           $ "printf(\"%d\\n\", " ++ generate n ++ ")"

data MyStruct
instance StructClass MyStruct where
  structName _ = "MyStruct"

program = do
  include "<stdio.h>"

  comment "Define MyStruct"

  ((Field getx :: MyStruct :-> Int) :>
   (Field gety :: MyStruct :-> Float)
   ) <- defineStruct ("x" :> "y")

  comment "Program entry point"
  makeMain $ \main argc argv -> do
    comment "Call fac with argument 5"
    v <- newvar "v"
    getx v =: lit 4
    printint $ getx v
    ret $ lit 0

main = writeFile "Struct.c" $ generate program
