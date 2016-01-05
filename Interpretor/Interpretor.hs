--Interpretor for simple language
data Const = IConst Integer | BConst Bool | FConst1 String (Const -> Const) | FConst2 String (Const -> Const -> Const) 

instance Show Const where
  show (IConst a)	 = show a
  show (BConst a)	 = show a
  show (FConst1 a b) = "<prim1:"++a++">"
  show (FConst2 a b) = "<prim2:"++a++">"
  
instance Eq Const where
	(IConst a)==(IConst b)		 = a==b
	(BConst a)==(BConst b) 		 = a==b
	(FConst1 a b)==(FConst1 c d) = a==c
	(FConst2 a b)==(FConst2 c d) = a==c
	

primAbs :: Const -> Const
primAbs (IConst a) = (IConst (abs a))

primPlus :: Const -> Const -> Const
primPlus (IConst a) (IConst b) = (IConst (a+b))

primMinus :: Const -> Const -> Const
primMinus (IConst a) (IConst b) = (IConst (a-b))

primMult :: Const -> Const -> Const
primMult (IConst a) (IConst b) = (IConst (a*b))

primDiv :: Const -> Const -> Const
primDiv (IConst a) (IConst b) = (IConst (a`div`b))

primEqualto :: Eq a => a -> a -> Const
primEqualto a b = (BConst (a==b))

{-
IConst 5
BConst True
FConst1 "abs" primAbs
FConst2 "+" primPlus 
-}	


data Exp = Econ Const | Var String | Lambda String  Exp | Appl  Exp Exp | IfExp Exp Exp Exp | LetRec String String Exp Exp deriving (Eq,Show)
{-
Econ (IConst 2)
Var "x"
Lambda "x" (Var "x")
IfExp (Econ (BConst True)) (Var "x") (Var "y")
Appl (Lambda "x" (Var "x")) (Econ (IConst 2))
-}

data Combinator = S | K | I | B |  C | CIf | Y deriving (Eq,Show)
data CExp = Ccon Const | CVar String | Cop Combinator |CAppl CExp CExp  deriving (Eq,Show)
{-
Ccon (IConst 5)
CVar "x"
Cop S
Cop CIf
CAppl (Cop I) (CVar "x")
CAppl (CAppl (Cop S) (Cop K)) (Cop K)
-}


initEnv :: [([Char], Const)]
initEnv=[("abs",(FConst1 "abs" primAbs)),
		 ("+",(FConst2 "+" primPlus)),
		 ("-",(FConst2 "-" primMinus)),
		 ("*",(FConst2 "*" primMult)),
		 ("div",(FConst2 "div" primDiv)),
		 ("==",(FConst2 "==" primEqualto))];

compile :: Exp -> CExp		 
compile (Econ x) 		 = Ccon x
compile (Var x)		     = CVar x
compile (IfExp m1 m2 m3) = CAppl (CAppl (CAppl (Cop CIf) (compile m1)) (compile m2)) (compile m3)
compile (Appl m n)       = CAppl (compile m) (compile n)   
compile (Lambda x m) 	 = abstract x (compile m) initEnv
compile (LetRec x y f v) = abstract x (CAppl (abstract y (compile f) initEnv) (compile v)) initEnv


abstract :: String -> CExp -> t -> CExp
abstract x (Ccon y) env 	   		= CAppl (Cop K) (Ccon y)
abstract x (CVar y) env | x==y 		= Cop I
						| otherwise = CAppl (Cop K) (case (lookup y initEnv) of 
														Nothing -> (CVar y)
														Just a ->  (Ccon a)       )
abstract x (Cop a) env         		= CAppl (Cop K) (Cop a)
abstract x (CAppl m n) env 	   		= case (abstract x m env ) of 
										(CAppl (Cop K) mc1)-> case (abstract x n env) of 
																(Cop I) 				->  mc1
																(CAppl (Cop K) nc1)	    -> CAppl (Cop K) (CAppl mc1 nc1)
																(nc1)                   -> CAppl (CAppl (Cop B) (mc1)) nc1
										mc1                -> case  (abstract x n env) of
																(CAppl (Cop K) nc1)	    -> CAppl (CAppl (Cop C) (mc1)) nc1
																(nc1)   			    -> CAppl (CAppl (Cop S) (mc1)) nc1
{-
initEnv
compile (Econ (IConst 5))
compile (Var "x")
compile (IfExp (Var "x") (Var "y") (Var "z"))
compile (Lambda "x" (Var "x"))
compile (Appl (Var "abs") (Econ (IConst (-2))))
compile (Var "abs")
compile (Lambda "x" (Appl (Var "abs") (Var "x")))
compile (Lambda "x" (Appl (Appl (Var "+") (Var "x")) (Econ (IConst 1))))
-}

reduceComb :: CExp -> CExp
reduceComb (CAppl (Cop I) x ) 											 = x
reduceComb (CAppl (CAppl (Cop K) x ) y ) 								 = x
reduceComb (CAppl (CAppl (CAppl (Cop S) f ) g ) x ) 					 = CAppl (CAppl f x ) (CAppl g x )
reduceComb (CAppl (CAppl (CAppl (Cop B) f ) g ) x ) 					 = CAppl f (CAppl g x)
reduceComb (CAppl (CAppl (CAppl (Cop C) f ) x ) y ) 					 = CAppl (CAppl f y ) x
reduceComb (CAppl (CAppl (CAppl (Cop CIf) (Ccon (BConst True))) x ) y )  = x
reduceComb (CAppl (CAppl (CAppl (Cop CIf) (Ccon (BConst False))) x ) y ) = y
reduceComb (CAppl (Ccon (FConst1 x y )) (Ccon z))                        = Ccon (y z)
reduceComb (CAppl (CAppl (Ccon (FConst2 x y )) (Ccon z)) (Ccon w))       = Ccon (y z w)
reduceComb (CAppl (CAppl (Cop Y) x) y)                                   = (CAppl (CAppl (x) (Cop Y)) (y)) 
reduceComb (CAppl x y)                                                   = (CAppl (reduceComb x) (reduceComb y))
reduceComb	x                                                            = x
{-
reduceComb (CAppl (Cop I) (CVar "X"))
reduceComb (CAppl (CAppl (Cop K) (CVar "X")) (CVar "Y"))
reduceComb (CAppl (CAppl (CAppl (Cop S) (CVar "F")) (CVar "G")) (CVar "X"))
reduceComb (CAppl (CAppl (CAppl (Cop B) (CVar "F")) (CVar "G")) (CVar "X"))
reduceComb (CAppl (CAppl (CAppl (Cop C) (CVar "F")) (CVar "X")) (CVar "Y"))
reduceComb (CAppl (CAppl (CAppl (Cop CIf) (Ccon (BConst True))) (CVar "X"))
reduceComb (CAppl (CAppl (CAppl (Cop CIf) (Ccon (BConst False))) (CVar "X")) (CVar "Y"))
reduceComb (CAppl (Ccon (FConst1 "abs" primAbs)) (Ccon (IConst (-2))))
reduceComb (CAppl (CAppl (Ccon (FConst2 "+" primPlus))(Ccon (IConst (-2))))(Ccon (IConst 5))) 
-}

run :: CExp -> CExp																
run tr = run1 (reduceComb tr) tr 
	where run1 new old = if new==old then new else (run new)
{-
run (CAppl (Cop I) (CAppl (Cop I) (Ccon (IConst 5))))
-}

compileAndRun :: Exp -> CExp	
compileAndRun x = run (compile x)
	
{-
compileAndRun (Appl (Lambda "x" (Var "x")) (Econ (IConst 5)))
compileAndRun (Appl (Lambda "x" (Econ (IConst 5))) (Appl (Appl (Var "div") (Econ (IConst 1))) (Econ (IConst 0))))
compileAndRun (Appl (Lambda "x" (IfExp (Appl (Appl (Var"==") (Var "x"))(Econ (IConst 0))) (Var "x") (Appl (Appl (Var "+")(Var "x")) (Econ (IConst 1))))) (Econ (IConst 3)))
compileAndRun (Appl (Lambda "x" (IfExp (Appl (Appl (Var"==") (Var "x")) (Econ (IConst 0)))(Var "x")(Appl (Appl (Var "+")(Var "x"))(Econ (IConst 1)))))(Econ (IConst 0)))
-}


						