module ExampleA where

import Main



decls :: [Decl]
decls =  [
  Decl_b "main" [] (Expr_e (Expr_e (Expr_d (INT 2)) Mul (Expr_d (INT 3))) Add (Expr_e (Expr_d (INT 4)) Div_a (Expr_d (INT 2))))
         ]
  
main :: IO ()
main = do Main.compile decls
