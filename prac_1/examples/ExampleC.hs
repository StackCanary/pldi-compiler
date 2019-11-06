module ExampleA where

import Main


-- Datatype A = Ctor_A B | A_Int Int
-- Datatype B = Ctor_B A | B_Int Int

decls :: [Decl]
decls =  [Decl_a "A" [Constr "Ctor_A" ["B"], Constr "A_INT" ["Int"]],
          Decl_a "B" [Constr "Ctor_B" ["A"], Constr "B_INT" ["Int"]]
         ]
  
main :: IO ()
main = do Main.compile decls

