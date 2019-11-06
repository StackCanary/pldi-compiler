module ExampleA where

import Main



decls :: [Decl]
decls =  [
  Decl_b "main" [] (Expr_d (INT 3))
         ]
  
main :: IO ()
main = do Main.compile decls
