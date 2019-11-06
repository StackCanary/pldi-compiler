module ExampleA where

import Main


decls :: [Decl]
decls =  [Decl_a "Shape" [Constr "Rect" ["Int", "Int"], Constr "Square" ["Int"]],
          Decl_b "main" [] (ExprCtr "Rect" ["Int", "Int"] [Expr_d (INT 3), Expr_d (INT 3)])
         ]
  
main :: IO ()
main = do Main.compile decls

