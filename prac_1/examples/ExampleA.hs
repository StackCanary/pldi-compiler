module ExampleA where

import Main


decls :: [Decl]
decls =  [
          Decl_a "Shape" [Constr "Rect" ["Int", "Int"], Constr "Square" ["Int", "Int"]]
         ]
  
main :: IO ()
main = do Main.compile decls
