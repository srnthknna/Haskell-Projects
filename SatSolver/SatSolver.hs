import Data.Char
import Data.List

--Common Solver
class (Eq c, Show c) => Config c where
  successors :: c -> [c]

solveAll :: (Config c) => (c -> Bool) -> c -> [c]
solveAll isGoal c = let restSolutions = concat [solveAll isGoal c' | c' <- successors c] 
                    in if isGoal c then c:restSolutions else restSolutions

solve :: (Config c) => (c -> Bool) -> c -> (Maybe c)
solve isGoal c = case solveAll isGoal c of
                   []   -> Nothing
                   x:xs -> Just x


-------------------------------------------------------------
--Sat Solver
data BExp =  Var String | Not BExp | And BExp BExp | Or BExp BExp | BConst Bool deriving (Show,Eq)
{-
And (Var "a") (Var "b")
Or (Var "a") (Var "b")
Not (Var "a")
-}
--Constructors for BExp
--to evaluate the expression
run :: BExp -> [(String,Bool)] -> Bool
run (And first second)  env  = (run first env) && (run second env) 
run (Or first second )  env  = (run first env) || (run second env)
run (Not first)         env  = not (run first env)
run (BConst first)      env  = first
run (Var first)         env  = value env first

--to findvariables in the BExp
findvars :: BExp -> [String]
findvars (And first second)  = (findvars first ) ++ (findvars second ) 
findvars (Or first second )  = (findvars first ) ++ (findvars second )
findvars (Not first)         = (findvars first )
findvars (BConst first)      = []
findvars (Var first)         = first:[]
{-
findvars (And (Var "a") (Var "b"))
findvars (Or (Var "a") (Var "b"))
-}

--helper function to find the value of key in the tuple list
value :: [(String, Bool)] -> String -> Bool                        
value env c = case lookup c env of
                Nothing -> False
                Just n  -> n
{-
value [("a",True),("b",True)] "a"
value [("a",True),("b",False)] "b"
-}

data SatConfig = SatConfig [(String,Bool)] [String] BExp Int deriving Eq
{-
SatConfig [("a",True),("b",True)] ["a","b"] (And (Var "a") (Var "b")) 2
SatConfig [("a",True),("b",True)] ["a","b"] (Or (Var "a") (Var "b")) 2
-}

--show instance for SatConfig
instance Show SatConfig where
  show (SatConfig env a b c ) = show env
{-
SatConfig [("a",True),("b",True)] ["a","b"] (And (Var "a") (Var "b")) 2
SatConfig [("a",True),("b",True)] ["a","b"] (Or (Var "a") (Var "b")) 2
-}

--making SatConfig a instance of Config
instance Config SatConfig where
  successors (SatConfig env [] _ _  ) = []
  successors (SatConfig env c@(var:vars) bexp i) = 
    [(SatConfig ((var,b):env) vars bexp (i-1)) | b<-[True,False] ]
{-
successors (SatConfig [] ["a","b"] (Or (Var "a") (Var "b")) 2)
successors (SatConfig [("a",True)] ["b"] (And (Var "a") (Var "b")) 2)
-}


isGoal :: SatConfig -> Bool
isGoal (SatConfig [] b c d)   = False
isGoal (SatConfig a b c d) = if d==0 then (satisfy (a) c) else False
{-
isGoal (SatConfig [("a",True)] ["b"] (And (Var "a") (Var "b")) 2)
isGoal (SatConfig [("a",True),("b",True)] [] (And (Var "a") (Var "b")) 2)
-} 

--helper for isGoal function
satisfy :: [(String,Bool)] -> BExp -> Bool
satisfy env a = if (run a env)==True then True else False
{-
satisfy [("a",True),("b",True)] (And (Var "a") (Var "b"))
satisfy [("a",True)] (And (Var "a") (Var "b"))
-}

satSolve :: BExp -> (Maybe SatConfig)
satSolve a = 
  let sigma      = nub (findvars a)
      initConfig = (SatConfig [] sigma a (length sigma) )
  in solve isGoal initConfig
{-
satSolve (Or (And (Not (Var "a")) (Not (Var "b"))) (And (Not (Var "a")) (Not (Var "b"))))
satSolve (Or (And (Not (Var "a")) (Not (Var "b"))) (Or (Not (Var "c")) (Not (Var "d"))))
satSolve (And (And (Not (Var "c")) (Not (Var "a"))) (Or (Not (Var "c")) (Not (Var "c"))))
satSolve (Not (Not (And (Not (Var "a")) (Not (Var "a")))))
satSolve (And (BConst False) (Var "a"))
-}