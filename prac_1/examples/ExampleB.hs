module ExampleA where

import Main


decls :: [Decl]
decls =  [
          Decl_a "List" [Constr "Cons" ["Int", "List"], Constr "Null" []]
         ]
  
main :: IO ()
main = do Main.compile decls

