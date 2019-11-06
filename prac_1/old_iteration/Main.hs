import Data.List
import Data.Char
import System.IO

import qualified Data.Map as Map



-- Abstract Syntax Tree --

data Const   = IntegeR Int               -- const   := <int>
             | Boolean Bool             --          | <bool>
             deriving (Show, Read)

data Op      = Add                          -- op      := +
             | Sub                          --          | -
             | Mul                          --          | *
             | Div_a                        --          | /
             | Div_b                        --          | div
             | Eqq                          --          | ==
             | GTT                           --         | >
             deriving (Show, Read)

type Id      = String



data Patt    = Patt_a { get_variable_name :: Id}                    -- patt    := <id>
             | Patt_b Id [Id]               --          | <id> <id>*
             | Patt_c Const                 --          | <constant>
             | Patt_d [Patt]                --          | [ <patt> , .. , <patt> ]
             deriving (Show, Read)

data Patts   = Patts [Patt]                 -- patts   := <patt>*
             deriving (Show, Read)        

data Match   = Arrow Patt Expr             -- match   := <patt> -> <expr>
             deriving (Show, Read)

data Rules   = Rule_a Match                -- rules   := <match> [ "|"  <rules> ]
             | Rule_b Match [Rules]        --            
             deriving (Show, Read)

data Expr    = Expr_a Id                    -- expr    := <id> <expr>*
             | Expr_b Id [Expr]             -- 
             | Expr_c Id [Expr]             -- 
             | Expr_d Const                 --          | <constant>
             | Expr_e Expr Op Expr          --          | <expr> <op> <expr>
             | Expr_f [Expr]                --          | [ <expr> , ... , <expr>]
             | Expr_g Expr Rules            --          | case <expr>  of <rules>
             | Expr_h Expr                  --          | ( <expr> )
             deriving (Show, Read)

data Constr  = Constr { constr_id :: Id, constr_args :: [Id] }               -- constr  := <id> <id>*
             deriving (Show, Read)

data Constrs = Constrs { constr :: [Constr] }             -- constrs := <constr> | ... | <constr>
             deriving (Show, Read)

data Decl    = Decl_a { decl_id :: Id, decl_constrs :: Constrs }            -- decl    := datatype <id> = <constrs>
             | Decl_b Id Patts Expr         --          |  <id> <patts> = <expr>
             deriving (Show, Read)

data Decls   = Decls [Decl]                 -- decls   := <decl> ; ... ; <decl>
             deriving (Show, Read)

data Prog    = Patt Decls                   -- prog    := patt <decls>
             deriving (Show, Read)

--------------------------




-- ByteCode --

type Attribute_Types = String

data Bytecode = Nop
              | And
              | ILoad Int
              | Ldc Int
              | IAdd | ISub | IMul | IDiv
              | If_icmpne String
              | If_icmplt String
              | Goto String
              | Label String
              | Dup
              | Invstat String String String
              | Invspec String String String
              | Invvirt String String String
              | Instant String String -- name [args]
              | Call String
              | Ctor String
              | Method  String [String] String
              | PField  String String
              | New String
              | Context [Bytecode] 
              | Class String String [Attribute_Types]
              | Super String
              | Func String [String] -- name [args]
              | Case String [Bytecode] [Bytecode] -- function: if [bytecode_1 is true] then [bytecode_2]
              | Else String [Bytecode]            -- function: else [bytecode_2]
              deriving (Show, Read)


-- Unfortunately, my datastructure is not polymorphic so I can't make it a functor
bmap :: (Bytecode -> Bytecode) -> Bytecode -> Bytecode
bmap f (Else a b1   ) = (Else a (fmap f b1)            )
bmap f (Case a b1 b2) = (Case a (fmap f b1) (fmap f b2))
bmap f b = f b
  
line = intercalate "\n"

write_bcode :: Bytecode -> String
write_bcode        Nop    =  "nop"
write_bcode        And    = "iand"
write_bcode     (ILoad i) = "iload " ++ show i
write_bcode       (Ldc i) = "ldc "   ++ show i
write_bcode       IAdd    = "iadd"
write_bcode       ISub    = "isub"
write_bcode       IMul    = "imul"
write_bcode       IDiv    = "idiv"
write_bcode (If_icmpne l) = "If_icmpne " ++ l
write_bcode (If_icmplt l) = "If_icmplt " ++ l
write_bcode      (Goto l) = "goto " ++ l
write_bcode     (Label l) =  show l ++ ":"
write_bcode        Dup    = "dup"
write_bcode   (Invstat f a r) = "invokestatic "  ++ f ++ "(" ++ a ++ ")" ++ r
write_bcode   (Invspec f a r) = "invokespecial " ++ f ++ "(" ++ a ++ ")" ++ r
write_bcode   (Invvirt f a r) = "invokevirtual " ++ f ++ "(" ++ a ++ ")" ++ r
write_bcode   (Instant f a   ) = line ["new " ++ f, "dup", write_bcode $ Invspec (f ++ "/<init>") a "V"]
write_bcode       (New s) = "new " ++ s
write_bcode (Method name args retval) = ".method public " ++ name ++ "(" ++ concat args ++ ")" ++ retval
write_bcode (        Func "main" args) = ".method public static main([Ljava/lang/String;)V"
write_bcode (        Func  name  args) = ".method public static " ++ name ++ "(" ++ concat args ++ ")" ++ "Ljava/lang/Object;"
write_bcode (PField field typ_ ) = "putfield " ++ field ++ " " ++ typ_
write_bcode (Else fname bcode) = line $ fmap write_bcode bcode           -- function: else [bytecode_2]                          -- TODO 
write_bcode (Case fname bcode_1 bcode_2) = line $ [(line $ fmap write_bcode bcode_1), (line $ fmap write_bcode bcode_2)]         -- TODO (add labels and jumps)

write_bcode (Class class_name super_name attributes) = line [".class public " ++ class_name, -- arguments
                                                                ".super " ++ super_name,
                                                                line (zipWith write_bcode_attribute [0,1..] attributes),
                                                                write_bcode (Method "<init>" (map attr_to_str attributes) "V"),
                                                                "aload 0",
                                                                "invokespecial " ++ super_name ++ "/<init>()V",
                                                                line $ reverse ["aload " ++ show i | (i, attr) <- zip [1,2..] attributes],
                                                                line [write_bcode (PField (class_name ++ "/_" ++ show i) (attr_to_str attr)) | (i, attr) <- zip [0,1..] attributes],
                                                                "return",
                                                                ".end method",
                                                                
                                                                write_bcode (Method "equals" ["Ljava/lang/Object;"] "Z"),
                                                                "aload 1",
                                                                "instanceof " ++ class_name,
                                                                "ifeq fail",
                                                                "aload 1",
                                                                "checkcast "  ++ class_name,
                                                                "astore 2",
                                                                line [line ["aload 0",
                                                                            "getfield " ++ class_name ++ "/_" ++ show i ++ " " ++ (attr_to_str attr),
                                                                            "aload 2",
                                                                            "getfield " ++ class_name ++ "/_" ++ show i ++ " " ++ (attr_to_str attr),
                                                                            write_bcode (Invvirt ((attr_to_str attr) ++ ".equals") "Ljava/lang/Object;" "Z"),
                                                                            "ifeq fail"
                                                                           ] | (i, attr) <- zip [0,1..] attributes],
                                                                "ldc 1",
                                                                "ireturn",
                                                                "fail:",
                                                                "ldc 0",
                                                                "ireturn",
                                                                ".end method",
                                                                
                                                                write_bcode (Method  "toString" [] "Ljava/langString;"),
                                                                write_bcode (Instant "java/lang/StringBuilder" ""),
                                                                "astore 1",

                                                                line [line ["aload 1",
                                                                            "aload 0",
                                                                            "getfield " ++ class_name ++ "/_" ++ show i ++ " " ++ (attr_to_str attr),
                                                                             write_bcode (Invvirt "java/lang/StringBuilder.append" "Ljava/lang/Object;" "Ljava/lang/StringBuilder;"),
                                                                             "pop"
                                                                           ] | (i, attr) <- zip [0,1..] attributes],

                                                                "aload 0",
                                                                write_bcode (Invvirt "java/lang/StringBuilder.toString" "" "Ljava/lang/String;"),
                                                                "areturn",
                                                                ".end method" 
                                                                ]
                                                         
                                                         
write_bcode (Super class_name) = line [".class public " ++ class_name,
                                        ".super java/lang/Object",
                                        "\n",
                                        ".method public <init>()V",
                                        "aload 0",
                                        "invokespecial java/lang/Object/<init>()V",
                                        "return",
                                        ".end method"
                                      ]


write_bcode evil_unrecognised_patt = "<BUT HOW?>" ++ show evil_unrecognised_patt ++ "</BUT HOW?>"

empty_class :: String -> Bytecode 
empty_class class_name = Super class_name



attr_to_str :: Id -> String
attr_to_str  "Int" = "Ljava/lang/Integer;"
attr_to_str "Bool" = "Ljava/lang/Integer;"
attr_to_str  attr  =  attr

write_bcode_attribute :: Int -> Id -> String
write_bcode_attribute i  "Int" = ".field private _" ++ show i ++ " Ljava/lang/Integer;"
write_bcode_attribute i "Bool" = ".field private _" ++ show i ++ " Ljava/lang/Integer;"
write_bcode_attribute i other  = ".field private _" ++ show i ++ " " ++ other

--------------


  -- Compiler --

  
var_patts_only :: [Patt] -> Bool
var_patts_only [] = True
var_patts_only (Patt_a _ :patts) = var_patts_only patts
var_patts_only (_ :patts) = False

-- A function given a patt and expr, maps variable names to numbers

is_variable :: Patt -> Bool
is_variable (Patt_a _) = True
is_variable _          = False

variable_dict :: [Patt] -> Map.Map String Int
variable_dict variables =  Map.fromList $ zip (map get_variable_name (filter is_variable variables)) [0,1..]


compile_decls :: Decls -> [Bytecode]
compile_decls (Decls decls) = concat $ fmap compile_decl decls


-- decl    := datatype <id> = <constrs> 
--          | <id> <patts>  = <expr>  
compile_decl :: Decl -> [Bytecode]

compile_decl (Decl_a type_name constructors) = (compile_type_name type_name)
                                               ++
                                               (compile_constructors type_name constructors) -- Datatype Declaration generates classes

compile_decl (Decl_b func_name (Patts patts) expr)
  | (var_patts_only patts) && (not . null $ patts) = _else_                     -- else <expr>
  | (null $ patts)                                 = _else_ ++ _func_
  |  otherwise                                     = _case_ ++ _func_           -- if <patts> then <expr> else jmp to next_label
  where compiled_patt = compile_patts patts
        variable_maps = variable_dict patts                                     -- A Dictionary mapping arguments to numbers 
        compiled_expr = compile_expr variable_maps expr
        _else_ = [Else func_name compiled_expr]
        _case_ = [Case func_name compiled_patt compiled_expr]
        _func_ = [Func func_name [unpatt p | p <- patts]]

unpatt :: Patt -> Id
unpatt (Patt_a var) = undefined
unpatt (Patt_b ctr [arg]) = ctr
unpatt (Patt_c const) =  case const of IntegeR _ -> "Ljava/lang/Integer;"
                                       Boolean _ -> "Ljava/lang/Integer;"
unpatt (Patt_d [patts]) = undefined

  


compile_patts :: [Patt] -> [Bytecode] -- A Compiled Pattern generates an if statement testing for equality if (first_arg == Patt_1 && secnd_arg == Patt_2) 
compile_patts patts = (concat $ (zipWith compile_patt [0,1..] patts)) ++ (take (length patts - 1) $ cycle [And])

-- TODO v v v v
                                                                  
compile_patt :: Int -> Patt -> [Bytecode]
compile_patt i (Patt_a var)       = undefined -- Should never be called

compile_patt i (Patt_b ctr args) = compile_ctor_args args   -- Push args onto stack?
                                    ++
                                    [ILoad i,
                                     Ctor ctr,
                                    (Invvirt (ctr ++ ".equals") "Ljava/lang/Object" "Z")] -- iload -> Instantiate Object -> Equaltiy
                                    
compile_patt i (Patt_c const)     = (compile_const const)
                                    ++
                                    [ILoad i,
                                      (Invstat "Java/lang/Integer.valueOf" "I" "Ljava/lang/Integer;"),
                                      (Invvirt "java/lang/Integer.equals" "Ljava/lang/Object" "Z")]
                                    -- iload -> Instantiate Object -> Equaltiy -- iload -> Instantiate Object -> Equality
                                     
compile_patt i (Patt_d [patts])   = undefined -- Instantiate Patts and Push -> Instantiate List Object and add to list -> Equality


compile_ctor_args :: [Id] -> [Bytecode]
compile_ctor_args _ = undefined

-- TODO ^ ^ ^ ^
-- ONE ISSUE TO DEAL WITH IS CALLING FUNCTIONS because you need it's types, so ... we need to deal with them later using a map

compile_expr :: Map.Map String Int ->  Expr -> [Bytecode]
compile_expr map (Expr_a var      )  = [ILoad $ map Map.! var]
compile_expr map (Expr_b ctr [e]  )  = (concat $ fmap (compile_expr map) [e]) ++ [Ctor ctr]         -- expr := <id> <expr>*   push expressions onto stack and call constructor for id or argument 
compile_expr map (Expr_c fcn [e]  )  = (concat $ fmap (compile_expr map) [e]) ++ [Call fcn]         -- expr := <id> <expr>*   push expressions onto stack and call constructor for id or argument 
compile_expr map (Expr_d const    )  =  compile_const const                                               -- | <constant>              
compile_expr map (Expr_e e1 op e2 )  = (compile_expr map e1)
                                       ++
                                       [Invvirt "java/lang/Integer.intValue" "" "I"]
                                       ++
                                       (compile_expr map e2)
                                       ++
                                       [Invvirt "java/lang/Integer.intValue" "" "I"]
                                       ++
                                       compile_op op    -- | <expr> <op> <expr>
                                       ++
                                       [Invstat "Java/lang/Integer.valueOf" "I" "Ljava/lang/Integer;"]
compile_expr map (Expr_f [e]      )  = reverse (concat $ fmap (compile_expr map) [e])
                                       ++
                                       [Instant "java/util/ArrayList" ""]  -- compile e's, push onto list Object                    -- | [ <expr> , ... , <expr>]
                                       ++
                                       [Invvirt "java/util/ArrayList.add" "Ljava/lang/Object;" "Z" | expr <- [e]]
                                       
compile_expr map (Expr_g e rules  )  = undefined                                                          -- | case <expr>  of <rules> 
compile_expr map (Expr_h e        )  = compile_expr map e                                                 -- | ( <expr> )


compile_const :: Const -> [Bytecode]
compile_const (IntegeR i) = [Ldc i, Invstat "Java/lang/Integer.valueOf" "I" "Ljava/lang/Integer;"]
compile_const (Boolean b) = [Ldc $ fromEnum b, Invstat "Java/lang/Integer.valueOf" "I" "Ljava/lang/Integer;"]

compile_op :: Op -> [Bytecode]
compile_op Add     = [IAdd]
compile_op Sub     = [ISub]
compile_op Mul     = [IMul]
compile_op Div_a   = [IDiv]
compile_op Div_b   = [IDiv]
compile_op Eqq     = [Context [If_icmpne "a", Ldc 1, Goto "b", Label "a", Ldc 0, Label "b"]]
compile_op GTT     = [Context [If_icmplt "a", Ldc 1, Goto "b", Label "a", Ldc 0, Label "b"]]

-- Compile Datatype declaration

compile_type_name :: Id -> [Bytecode]
compile_type_name type_name = [empty_class type_name]       -- EMITS PARENT CLASS

-- constrs := <constr> | ... | <constr>
compile_constructors :: Id -> Constrs -> [Bytecode]
compile_constructors type_name (Constrs constructors) = concat $ fmap (compile_constructor type_name) constructors

-- constr  := <id> <id>*
compile_constructor ::  Id -> Constr -> [Bytecode]  -- EMITS CHILD THAT EXTENDS PARENT
compile_constructor  type_name (Constr constructor_name arguments) = return $ Class constructor_name type_name arguments -- Arguments???

----------------------------------




-- Emit bytecode with labels adjusted (Requires State)
fix_context :: [Bytecode] -> [Bytecode]  -- TODO v v v
fix_context _ = undefined

-- Emit bytecode with funct arguments corrected
fix_call :: Map.Map String [String] -> Bytecode -> Bytecode
fix_call map (Call fcn) = Invstat fcn (concat $ map Map.! fcn) "Ljava/lang/Object;"
fix_call map b          = b

fix_calls :: [Bytecode] -> [Bytecode]
fix_calls bcode = let my_map = (get_func_to_args_map bcode) in map ((bmap . fix_call) my_map) bcode
    
-- Emit bytecode with ctors arguments corrected
fix_ctor :: Map.Map String [String] -> Bytecode -> Bytecode
fix_ctor map (Ctor ctr) = Instant ctr (concat $ map Map.! ctr)
fix_ctor map b          = b

fix_ctors :: [Bytecode] -> [Bytecode]
fix_ctors bcode = let my_map = (get_ctor_to_args_map bcode) in map ((bmap . fix_ctor) my_map) bcode

-- Require one map from function to arguments, ez use get_funcs

get_class :: [Bytecode] -> [Bytecode]
get_class bcode = [b | b@(Class {}) <- bcode]

get_super :: [Bytecode] -> [Bytecode]
get_super bcode = [b | b@(Super {}) <- bcode]

get_cases :: [Bytecode] -> Map.Map String [Bytecode]
get_cases bcode = let cases = [b | b@(Case {}) <- bcode] in Map.fromListWith (++) [(k, [Case k b1 b2]) | Case k b1 b2 <- cases]

get_elses :: [Bytecode] -> Map.Map String [Bytecode]
get_elses bcode = let elses = [b | b@(Else {}) <- bcode] in Map.fromListWith (++) [(k, [Else k b2]) | Else k b2 <- elses]

get_funcs :: [Bytecode] -> Map.Map String [Bytecode]
get_funcs bcode = let funcs = [b | b@(Func {}) <- bcode] in Map.fromListWith (++) [(k, [Func k b2]) | Func k b2 <- funcs]


mak_func :: Bytecode -> Maybe [Bytecode] -> [Bytecode] -> String
mak_func func (Nothing)    else_ = line [write_bcode func, line $ fmap write_bcode else_, ".end method\n"]
mak_func func (Just cases) else_ = line [write_bcode func, line (fmap write_bcode cases), line $ fmap write_bcode else_, ".end method\n"]




get_ctor_to_type_map :: [Bytecode] -> Map.Map String String
get_ctor_to_type_map classes = Map.fromList $ [ (class_name, super_name) | Class class_name super_name _ <- classes]

get_func_to_args_map :: [Bytecode] -> Map.Map String [String]
get_func_to_args_map funcs   = Map.fromList $ [ (func_name,  args) |  Func func_name args <- funcs]

get_ctor_to_args_map :: [Bytecode] -> Map.Map String [String]
get_ctor_to_args_map classes = Map.fromList $ [ (class_name, args) | Class class_name _ args <- classes]




mak_funcs :: [Bytecode] -> String
mak_funcs bcode = let _cases_ = get_cases bcode :: Map.Map String [Bytecode]
                      _elses_ = get_elses bcode :: Map.Map String [Bytecode]
                      _funcs_ = get_funcs bcode :: Map.Map String [Bytecode]
                  in line [mak_func (head (_funcs_ Map.! func_name)) (Map.lookup func_name _cases_) _else_  | (func_name, _else_) <- Map.toList _elses_] ++ "\n"
                    

-- I FOCUS ON CLASSES

-- Class String (Maybe String) [Attribute_Types]

o_dir = "output_directory/"

write_class :: Bytecode -> IO ()
write_class (Class class_name super_name attributes) = do writeFile (o_dir ++ class_name ++ ".j") (write_bcode (Class class_name super_name attributes))

write_super :: Bytecode -> IO ()
write_super (Super class_name) = do writeFile (o_dir ++ class_name ++ ".j") (write_bcode (Super class_name))

write_funcs :: [Bytecode] -> IO()
write_funcs bcode = do writeFile (o_dir ++ "Main.j") (mak_funcs . fix_ctors . fix_calls $ bcode)


                       
  









--- Parse -----

  

---------------











