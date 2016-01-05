To run Interpretor use
:l Interpretor.hs

The language for understanding is
data Const = IConst Integer | BConst Bool | FConst1 String (Const -> Const) | FConst2 String (Const -> Const -> Const) 
data Exp = Econ Const | Var String | Lambda String  Exp | Appl  Exp Exp | IfExp Exp Exp Exp | LetRec String String Exp Exp deriving (Eq,Show)
data Combinator = S | K | I | B |  C | CIf | Y deriving (Eq,Show)
data CExp = Ccon Const | CVar String | Cop Combinator |CAppl CExp CExp  deriving (Eq,Show)


Run and compile the language using examles like
compileAndRun (Appl (Lambda "x" (Var "x")) (Econ (IConst 5)))
compileAndRun (Appl (Lambda "x" (Econ (IConst 5))) (Appl (Appl (Var "div") (Econ (IConst 1))) (Econ (IConst 0))))
compileAndRun (Appl (Lambda "x" (IfExp (Appl (Appl (Var"==") (Var "x"))(Econ (IConst 0))) (Var "x") (Appl (Appl (Var "+")(Var "x")) (Econ (IConst 1))))) (Econ (IConst 3)))
compileAndRun (Appl (Lambda "x" (IfExp (Appl (Appl (Var"==") (Var "x")) (Econ (IConst 0)))(Var "x")(Appl (Appl (Var "+")(Var "x"))(Econ (IConst 1)))))(Econ (IConst 0)))
