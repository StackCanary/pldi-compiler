import Data.List
import Data.Char
import System.IO
import Data.Function
import qualified Data.Map as Map

-- Copied from https://stackoverflow.com/questions/36300448 -- 
import Data.Map (empty, alter, toList)
-- End Copy Citation - - - - - - - - - - - - - - - - - - - - -

o_dir = "output_directory/"

compile :: [Decl] -> IO ()
compile decls = do write_functions decls
                   write_data_decl decls

-- Abstract Syntax Tree --                 -- Concrete Syntax Tree --
data Const   = INT Int                     -- const   := <int>
             | Boolean Bool                --          | <bool>
             deriving (Show, Read)

data Op      = Add                         -- op      := +
             | Sub                         --          | -
             | Mul                         --          | *
             | Div_a                       --          | /
             | Div_b                       --          | div
             | Eqq                         --          | ==
             | GTT                         --          | >
             deriving (Show, Read)

type Id      = String

data Match   = Arrow MyPatt Expr           -- match   := <patt> -> <expr>
             deriving (Show, Read)

data Rules   = Rule_a Match                -- rules   := <match> [ "|"  <rules> ]
             | Rule_b Match [Rules]                   
             deriving (Show, Read)

data Expr    = ExprVar Id                  -- expr    := <id> <expr>*
             | ExprFcn Id Int [Expr]       -- 
             | ExprCtr Id [String] [Expr]  -- 
             | Expr_d Const                --          | <constant>
             | Expr_e Expr Op Expr         --          | <expr> <op> <expr>
             | Expr_f [Expr]               --          | [ <expr> , ... , <expr>]
             | Expr_g Expr Rules           --          | case <expr>  of <rules>
             | Expr_h Expr                 --          | ( <expr> )
             deriving (Show, Read)

data Constr  = Constr { constr_id :: Id, constr_args :: [Id] } -- constr  := <id> <id>*
             deriving (Show, Read)


data Decl    = Decl_a Id [Constr]             -- decl    := datatype <id> = <constrs> 
             | Decl_b { decl_id :: Id,
                        decl_patts ::[MyPatt],
                        decl_expr :: Expr }   --          | <id> <patts> = <expr> 
             deriving (Show, Read)

data Decls   = Decls [Decl]                 -- decls   := <decl> ; ... ; <decl>
             deriving (Show, Read)

data Prog    = Patt Decls                   -- prog    := patt <decls>
             deriving (Show, Read)

-- <patt> := <id> | <id> <id>* | <constant> | [<patt>, ..]
data MyPatt = MyPatt_a String | MyPatt_b PCtor | MyPatt_c Const | MyPatt_d [MyPatt]
            deriving (Show, Read)

-- This represents a constructor pattern's arguments.
data Ident = IdentVar String String 
           | IdentCtr PCtor
           | IdentCst Const
           deriving (Show, Read)
-- This represents a constructor in pattern matching.
data PCtor = PattCtor String [Ident]
           deriving (Show, Read)

-- Emit label (used as an argument in goto)
label :: Int -> String
label i = "Label" ++ show i

-- Emit a label identifier tag e.g. "label1"
label_tag :: Int -> String
label_tag i = "Label" ++ show i ++ ":"

-- Concatenate and insert a new line between each element 
line = intercalate "\n"

-- Compile Patterns into instanceof -> checkcast -> Equality Testing
patt :: Int -> Int -> MyPatt -> String
-- Variable Patterns do not need to compile into anything     
patt patt_no objects_argument_number (MyPatt_a var)  
  = ""
-- Check if constructors are equal and then match identifiers
patt patt_no objects_argument_number (MyPatt_b (PattCtor ctor identifiers))
  = line ["aload "      ++ show objects_argument_number,
          "instanceof " ++ ctor,
          "ifeq "       ++ label patt_no,
          "checkcast "  ++ ctor,
           line [ ident patt_no ctor_field_no ctor identifier | (identifier, ctor_field_no) <- zip identifiers [0, 1..]]
         ]
-- Constant Patterns check for equality against an Integer Object    
patt patt_no objects_argument_number (MyPatt_c constant)
  = line [(compile_const constant),
           "aload "       ++ show objects_argument_number,
           write_bcode (Invvirt "java/lang/Object.equals" "Ljava/lang/Object;" "Z"),
           "ifeq "       ++  label patt_no
         ]
-- Unimplimited List Pattern    
patt patt_no objects_argument_number (MyPatt_d list_patts)
  = undefined
                                                                      
-- Identifiers represent fields in constructor patterns
ident :: Int -> Int -> String -> Ident -> String
-- A variable identifier emits no code
ident patt_no constructor_field_number parent_ctr (IdentVar var typ_)
  = ""
-- Recursively call getfield to extract an attribute from an object correlating to the pattern
ident patt_no constructor_field_number parent_ctr (IdentCtr (PattCtor ctor identifiers))
  = line $
      [ line
        [
          write_bcode (GField (parent_ctr ++ "/_" ++ show constructor_field_number) ctor),
          ident patt_no ctor_field_no ctor identifier
        ] | (identifier, ctor_field_no) <- zip identifiers [0, 1..]]
-- Extract a constant from the object 
ident patt_no constructor_field_number parent_ctr (IdentCst constant)
  = line
    [write_bcode (GField (parent_ctr ++ "/_" ++ show constructor_field_number) "Ljava/lang/Integer;"),
     (compile_const constant),
     write_bcode (Invvirt "java/lang/Integer.equals" "Ljava/lang/Object;" "Z")]
--------------------------


-- Copied from https://stackoverflow.com/questions/36300448 -- 
-- (My solution changed the order of things (using maps), so I copied this from stackoverflow)
fun :: (Foldable t, Ord k) => t (k, [a]) -> [(k, [a])]
fun = toList . foldr go empty
    where
    go (k, x) acc  = alter (inc x) k acc
    inc x Nothing  = Just x
    inc x (Just y) = Just (x ++ y)

-- End Copy Citation - - - - - - - - - - - - - - - - - - - -

numbs = [0,1..]

-- Seperate Functions and Datatype (We want to compile Datatype first)
datatype_decls :: [Decl] -> [Decl]
datatype_decls decls = [decl_a | decl_a@(Decl_a {}) <- decls]

function_decls :: [Decl] -> [Decl]
function_decls decls = [decl_b | decl_b@(Decl_b {}) <- decls]

-- Returns the number of Objects a function takes as an argument
patt_count :: [Decl] -> Int
patt_count decls = length (decl_patts (head decls))

-- Group func decls by name, insert a class prolog and a main method. Compile func decls.
compile_functions :: [Decl] -> String
compile_functions decls =
  let
    f_decls = group_decl_bs_by_fname decls -- (Group decls by name)
  in
    function_eplg                          -- (Emit class epilog)
    ++
    "\n\n"
    ++
    function_main                          -- (Emit main method which calls the compiled main method and prints ret value)
    ++
    "\n\n"
    ++
    (line
      [ line [(function_head fname (patt_count decls)), line [compile_decls fname decls], function_tail, "\n"] | (fname, decls) <-f_decls]
     )                                     -- Compile each group of declarations

-- Compile a group of declarations for a specific function name
compile_decls :: String -> [Decl] -> String
compile_decls fname decls
  = line [compile_decl fname patt_no patts my_expr | (Decl_b _ patts my_expr, patt_no)  <- zip decls numbs]

-- Compile a single declaration
compile_decl :: String -> Int -> [MyPatt] -> Expr -> String
compile_decl fname patt_no patts my_expr =
  let
    compiled_patts = line [patt patt_no object_arg_no my_patt | (my_patt, object_arg_no) <- zip patts numbs]
    compiled_exprs = line $ (fmap write_bcode) (expr undefined my_expr)
  in
    line [compiled_patts, compiled_exprs, "areturn", label_tag patt_no]


-- Write compiled code to Main.j
write_functions :: [Decl] -> IO ()
write_functions decls = do writeFile (o_dir ++ "Main.j") (compile_functions decls)

-- Write compiled class to separate class files
write_data_decl :: [Decl] -> IO ()
write_data_decl decls = 
  let
    datadecls = datatype_decls decls
    compiled_datadecls = concat $ fmap compile_decl_a datadecls :: [Bytecode]
  in
    do (mapM_ write_super) compiled_datadecls
       (mapM_ write_class) compiled_datadecls
       
-- Write Child class to file
write_class :: Bytecode -> IO ()
write_class (Class class_name super_name attributes) = do writeFile (o_dir ++ class_name ++ ".j") (write_bcode (Class class_name super_name attributes))
write_class _ = do return ()

-- Write Super Class to file
write_super :: Bytecode -> IO ()
write_super (Super class_name) = do writeFile (o_dir ++ class_name ++ ".j") (write_bcode (Super class_name))
write_super _ = do return ()
                                        

-- Function declaration 
function_head :: String -> Int -> String
function_head name patt_no = ".method public static "
                                   ++
                                   name
                                   ++ "("
                                   ++ (concat . take patt_no . repeat) "Ljava/lang/Object;"
                                   ++ ")"
                                   ++ "Ljava/lang/Object;"
                                   ++ "\n" 
                                   ++ (".limit stack "  ++ show 500 ++ "\n")
                                   ++ (".limit locals " ++ show 500)
                                   
-- Function Tail                                   
function_tail :: String
function_tail = ".end method"

-- Emits the class declaration for Main.j
function_eplg :: String
function_eplg =
  line [".class public Main",
        ".super java/lang/Object",
         ".method public <init>()V",
         "aload_0",
         "invokespecial java/lang/Object/<init>()V",
         "return",
         ".end method"]
  
-- Emits the main method which calls and prints the user's main function
function_main :: String
function_main = line [".method public static main([Ljava/lang/String;)V",
                      ".limit stack 4",
                      ".limit locals 4",
                      "invokestatic Main.main()Ljava/lang/Object;",
                      "astore_1",
                      "getstatic java/lang/System.out Ljava/io/PrintStream;",
                      "aload_1",
                      "invokevirtual java/io/PrintStream/println(Ljava/lang/Object;)V",
                      "return",
                      ".end method"
                     ]


-- Given an expression, compile into code

-- Expressions --
expr :: Map.Map String String -> Expr -> [Bytecode]
expr var_map (ExprVar var      ) = [Raw (var_map Map.! var)] 
expr var_map (ExprCtr ctr t e)   = [New ctr, -- expr := <id> <expr>*   push expressions onto stack and call constructor for id or argument

                                    Dup]
                                   ++
                                   (concat $ fmap (expr var_map) e)

                                   ++
                                   [
                                    Invspec (ctr ++ "/<init>") (concat (fmap attr_to_str t)) "V"
                                   ]

expr var_map (ExprFcn fcn n e) = (concat $ fmap (expr var_map) e)
                                   ++
                                   [Call fcn n]
expr var_map (Expr_d const     ) = [Raw (compile_const const)]
expr var_map (Expr_e e1 op e2  ) = (expr var_map e1)
                                   ++
                                   [Invvirt "java/lang/Integer.intValue" "" "I"]
                                   ++
                                   (expr var_map e2)
                                   ++
                                   [Invvirt "java/lang/Integer.intValue" "" "I"]
                                   ++
                                   compile_op op    -- | <expr> <op> <expr>
                                   ++
                                   [Invstat "java/lang/Integer.valueOf" "I" "Ljava/lang/Integer;"]

                                   
expr var_map (Expr_f e       ) = reverse (concat $ fmap (expr var_map) e)
                                   ++
                                   [Instant "java/util/ArrayList" ""]  -- compile e's, push onto list Object                    -- | [ <expr> , ... , <expr>]
                                   ++
                                   [Invvirt "java/util/ArrayList.add" "Ljava/lang/Object;" "Z" | expr <- e]

                                   
expr var_map (Expr_g e rules   ) = undefined                                                          -- | case <expr>  of <rules> 
expr var_map (Expr_h e         ) = expr var_map e                                                 -- | ( <expr> )


group_decl_bs_by_fname :: [Decl] -> [(String, [Decl])]
group_decl_bs_by_fname decls =
  let
    decl_bs = [b | b@(Decl_b {}) <- decls]
  in
    fun [(fname, [Decl_b fname patts expr]) | Decl_b fname patts expr  <- decl_bs]



-- Assembler constant into an Integer Object
compile_const :: Const -> String
compile_const (INT i) = line ["ldc " ++ show i,  write_bcode (Invstat "java/lang/Integer.valueOf" "I" "Ljava/lang/Integer;")]
compile_const (Boolean b) = line ["ldc " ++ (show . fromEnum) b, write_bcode (Invstat "java/lang/Integer.valueOf" "I" "Ljava/lang/Integer;")]

-- Compile Operators +, -, * ...
compile_op :: Op -> [Bytecode]
compile_op Add     = [IAdd]
compile_op Sub     = [ISub]
compile_op Mul     = [IMul]
compile_op Div_a   = [IDiv]
compile_op Div_b   = [IDiv]
compile_op Eqq     = [Context [If_icmpne "a", Ldc 1, Goto "b", Label "a", Ldc 0, Label "b"]]
compile_op GTT     = [Context [If_icmplt "a", Ldc 1, Goto "b", Label "a", Ldc 0, Label "b"]]


-- An intermediate Bytecode Representation --

type Attribute_Types = String

data Bytecode = Nop
              | And
              | ILoad Int
              | ALoad Int
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
              | Call String  Int
              | Ctor String [String]
              | Method  String [String] String
              | PField  String String
              | GField  String String
              | New String
              | Context [Bytecode] 
              | Class String String [Attribute_Types]
              | Super String
              | Func String [String] -- name [args]
              | Case String [Bytecode] [Bytecode] -- function: if [bytecode_1 is true] then [bytecode_2]
              | Else String [Bytecode]            -- function: else [bytecode_2]
              | Raw  String
              deriving (Show, Read)


-- Unfortunately, my datastructure is not polymorphic so I can't make it a functor, Map's over [bytecode] inside 
bmap :: (Bytecode -> Bytecode) -> Bytecode -> Bytecode
bmap f (Else a b1   ) = (Else a  (fmap f b1)            )
bmap f (Case a b1 b2) = (Case a  (fmap f b1) (fmap f b2))
bmap f (Context   b1) = (Context (fmap f b1)            )
bmap f b = f b
  
-- Transform Bytecode intermediate rep to a String
write_bcode :: Bytecode -> String
write_bcode  (Call f n)   =  undefined
write_bcode  (Ctor f t)   =  undefined
write_bcode        Nop    =  "nop"
write_bcode        And    = "iand"
write_bcode     (ILoad i) = "iload " ++ show i
write_bcode     (ALoad i) = "aload " ++ show i
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
write_bcode       (Raw t) = t
write_bcode   (Invstat f a r) = "invokestatic "  ++ f ++ "(" ++ a ++ ")" ++ r
write_bcode   (Invspec f a r) = "invokespecial " ++ f ++ "(" ++ a ++ ")" ++ r
write_bcode   (Invvirt f a r) = "invokevirtual " ++ f ++ "(" ++ a ++ ")" ++ r
write_bcode   (Instant f a   ) = line ["new " ++ f, "dup", write_bcode $ Invspec (f ++ "/<init>") a "V"]
write_bcode       (New s) = "new " ++ s
write_bcode (Method name args retval) = ".method public " ++ name ++ "(" ++ concat args ++ ")" ++ retval
write_bcode (        Func "main" args) = ".method public static main([Ljava/lang/String;)V"
write_bcode (        Func  name  args) = ".method public static " ++ name ++ "(" ++ concat args ++ ")" ++ "Ljava/lang/Object;"
write_bcode (PField field typ_ ) = "putfield " ++ field ++ " " ++ typ_
write_bcode (GField field typ_ ) = "getfield " ++ field ++ " " ++ typ_
write_bcode (Else fname bcode) = line $ fmap write_bcode bcode           -- function: else [bytecode_2]                          -- TODO 
write_bcode (Case fname bcode_1 bcode_2) = line $ [(line $ fmap write_bcode bcode_1), (line $ fmap write_bcode bcode_2)]         -- TODO (add labels and jumps)

-- Emit the code for a Class file featuring a constructor, attributes, equals and toString
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
                                                         

-- Emit the code for a super class
write_bcode (Super class_name) = line [".class public " ++ class_name,
                                        ".super java/lang/Object",
                                        ".method public <init>()V",
                                        "aload 0",
                                        "invokespecial java/lang/Object/<init>()V",
                                        "return",
                                        ".end method"
                                      ]

-- An unregcongised pattern generates an error message
write_bcode evil_unrecognised_patt = "<Not Recognised>" ++ show evil_unrecognised_patt ++ "</Not Recognised>"


-- Represents a class with a default constructor and no attributes or methods
empty_class :: String -> Bytecode 
empty_class class_name = Super class_name

-- Transform Constant types to JVM types
attr_to_str :: Id -> String
attr_to_str  "Int" = "Ljava/lang/Integer;"
attr_to_str "Bool" = "Ljava/lang/Integer;"
attr_to_str  attr  =  attr

-- Emit field in class
write_bcode_attribute :: Int -> Id -> String
write_bcode_attribute i  "Int" = ".field private _" ++ show i ++ " Ljava/lang/Integer;"
write_bcode_attribute i "Bool" = ".field private _" ++ show i ++ " Ljava/lang/Integer;"
write_bcode_attribute i other  = ".field private _" ++ show i ++ " " ++ other

-- Collect all class bytecode intermediate reps
get_class :: [Bytecode] -> [Bytecode]
get_class bcode = [b | b@(Class {}) <- bcode]

-- Collect all super class bytecode intermediate reps
get_super :: [Bytecode] -> [Bytecode]
get_super bcode = [b | b@(Super {}) <- bcode]

-- -- Compile Datatype declaration

compile_decl_a :: Decl -> [Bytecode]
compile_decl_a (Decl_a type_name constructors) = (compile_type_name type_name)
                                                 ++
                                                 (concat $ fmap (compile_constructor type_name) constructors)
                                                 
compile_type_name :: Id -> [Bytecode]
compile_type_name type_name = [empty_class type_name] -- EMITS PARENT CLASS

-- constr  := <id> <id>*
compile_constructor ::  Id -> Constr -> [Bytecode]    -- EMITS CHILD THAT EXTENDS PARENT
compile_constructor  type_name (Constr constructor_name arguments) = [Class constructor_name type_name arguments]
