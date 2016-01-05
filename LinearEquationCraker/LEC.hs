import Data.List
import Data.Ratio


sub :: Num a => [a] -> [a] -> [a]
sub [] []         = []
sub [] (y:ys)     = (-y):(sub [] ys)
sub xs []     	  = xs
sub (x:xs) (y:ys) = (x-y):(sub xs ys)
{-
sub [2,2,3] [2,5,12] 
sub [3,4,5,6] [1,1,1,1]
-}

scaleList :: Num t => t -> [t] -> [t]
scaleList a [] = []
scaleList a (x:xs) = (a*x):(scaleList a xs)
{-
scaleList (1/2) [2,5,12]
scaleList (2) [2,5,12]
-}

subScale :: [Double] -> [Double] -> [Double]
subScale [] []  = []
subScale [] [a] = []
subScale [a] [] = []
subScale (x:xs) (y:ys) = sub (tail (scaleList (((lcm x y)`div`y)) (y:ys))) (tail (scaleList (((lcm x y)`div`x)) (x:xs)))
{-
subScale [2,2,3,10] [2,5,12,31]
subScale [2,3,3,8] [4,-2,2,4]
-}

--helper to find the first element with first non zeror element
findit :: (Num a, Eq a) => [[a]] -> [a]
findit ((x:xs):xxs) = if x/=0 then (x:xs) else (if xxs/=[] then (findit xxs) else [])
{-
findit [[1,-5,-5],[0,-4,-12],[1,1,1]]
findit [[0,-5,-5],[0,-4,-12],[1,1,1]]
-}
--The Complexity is O(n) as find takes O(n) and delete takes O(n) so it becomes O(n) in total
nonZeroFirst :: (Num a, Eq a) => [[a]] -> [[a]]
nonZeroFirst xxs = if xs==[] then (error "No Such Elements Found") else xs:(delete xs xxs)
         where xs=findit xxs
{-
nonZeroFirst [[0,-5,-5],[-8,-4,-12]]
nonZeroFirst [[0,-5,-5],[0,-4,-12]]
-}

instance (Integral Double)  where
 toInteger a = round a
 quotRem a b = (fromIntegral(a/b),a-(b*(fromIntegral(a/b))))
 div a b = a/b

triangulate :: [[Double]] -> [[Double]]
triangulate (xs:ys:[]) = [xs] ++scalehead xs (ys:[])
triangulate s = [head s1] ++( triangulate (nonZeroFirst (scalehead (head s1) ( (tail s1)))) )
  where s1= nonZeroFirst s
{-
triangulate [[2,3,3,8],[2,3,-2,3],[4,-2,2,4]]
triangulate [[1.0,2.0,-3.0,4.0,12.0],[2.0,2.0,-2.0,3.0,10.0],[0.0,1.0,1.0,0.0,-1.0],[1.0,-1.0,1.0,-2.0,-4.0]]
-}
--helper to scale head of the list with other elements to make left most elements zero
scalehead :: [Double] -> [[Double]] -> [[Double]]
scalehead s [] = []
scalehead s (x:y)= (if (head x)/=0 then (subScale s x) else (drop 1 x)):(scalehead s y)
{-
scalehead [2,3,3,8] [[2,3,-2,3],[4,-2,2,4]]
scalehead [1.0,2.0,-3.0,4.0,12.0] [[2.0,2.0,-2.0,3.0,10.0],[0.0,1.0,1.0,0.0,-1.0],[1.0,-1.0,1.0,-2.0,-4.0]]
-}


dot :: Num a => [a] -> [a] -> a
dot []     []     =  0
dot xs     []     =  0
dot []     xs     =  0
dot (f:fs) (s:ss) = (f*s) + (dot fs ss)
{-
dot [1,2] [3,4]
dot [1,2,5] [3,4,6]
-}

solveLine :: Fractional a => [a] -> [a] -> a
solveLine xs ys =  ((last xs) -(dot (init (tail xs)) ys ))/(head xs)  
{-
solveLine [2,3,3,8] [1,1]
solveLine [-5,-5] []
-}

solveTriangular :: Fractional a => [[a]] -> [a]
solveTriangular s2@(s:[]) = (solveLine (head s2) []):[] 
solveTriangular (x:ys) = [(solveLine x ((solveTriangular ys)))]++((solveTriangular ys))
{-
solveTriangular [[2.0,3.0,3.0,8.0],[-8.0,-4.0,-12.0],[-5.0,-5.0]]
solveTriangular [[1.0,2.0,-3.0,4.0,12.0],[-2.0,4.0,-5.0,-14.0],[6.0,-5.0,-16.0],[1.0,2.0]]
-}

solveSystem :: [[Double]] -> [Double]
solveSystem xxs = solveTriangular (triangulate xxs)
{-
solveSystem [[2,3,3,8],[2,3,-2,3],[4,-2,2,4]]
solveSystem [[1.0,2.0,-3.0,4.0,12.0],[2.0,2.0,-2.0,3.0,10.0],[0.0,1.0,1.0,0.0,-1.0],[1.0,-1.0,1.0,-2.0,-4.0]]
-}
------------------------------------------------------------------------------
--Question 4
data Exp = RExp Rational   |
           Var String     |
           Sum Exp Exp    |
           Prod Exp Exp   |
           Pow Exp Exp    |
		   D Exp String   |
		   Sin Exp         |
		   Cos Exp		   |
		   Exp Exp		   |
		   Ln  Exp		|
		   Int Exp String   deriving Eq
		   
addParens :: String -> String
addParens x = "(" ++ (x) ++ ")"

showSumContext :: Exp -> String
showSumContext (Prod u v) = showProdContext (Prod u v)
showSumContext (Pow a b)  = (showPowContextLeft a)++"^"++(showPowContextRight b)
showSumContext (Sum a b) = (show a)++"+"++(show b)
showSumContext (D u v)        =	 "D("++(show u)++", "++(v)++")"
showSumContext (Var x)        = x
showSumContext (Sin a) =  "Sin" ++ addParens(showSumContext(a))
showSumContext (Cos a) =  "Cos" ++ addParens(showSumContext(a))
showSumContext (Exp a) =  "e^" ++ addParens(showSumContext(a))
showSumContext (Ln a) =   "Ln"  ++ addParens(showSumContext(a))
showSumContext (Int a b) = "Int("++(show a)++", "++(b)++")"
showSumContext (RExp n) | (denominator n)==1  =if (numerator n)<0 then "(" ++ (show (numerator n)) ++ ")" else (show (numerator n))
                        | otherwise           =(if (numerator n)<0 then "(" ++ (show (numerator n)) ++ ")" else (show (numerator n)) )++ "/"++
						                        (if (denominator n)<0 then "(" ++ (show (denominator n)) ++ ")" else (show (denominator n)))

showProdContext :: Exp -> String												
showProdContext (Prod u v) = (showProdContext u)++"*"++(showProdContext v)
showProdContext (Sum u v) = addParens (showSumContext (Sum u v))
showProdContext (Pow a b)  = addParens(showSumContext (Pow a b))
showProdContext (D u v)        =(showSumContext (D u v))
showProdContext (Var x)        = x
showProdContext (Sin a) =  (showSumContext(Sin a) )
showProdContext (Cos a) =  (showSumContext(Cos a) )
showProdContext (Exp a) =  (showSumContext(Exp a) )
showProdContext (Ln a) =   (showSumContext(Ln a) )
showProdContext (Int a b) =   (showSumContext(Int a b) )
showProdContext (RExp n) | (denominator n)==1  =if (numerator n)<0 then "(" ++ (show (numerator n)) ++ ")" else (show (numerator n))
                         | otherwise           =(if (numerator n)<0 then "(" ++ (show (numerator n)) ++ ")" else (show (numerator n)) )++ "/"++
						                        (if (denominator n)<0 then "(" ++ (show (denominator n)) ++ ")" else (show (denominator n)))

showPowContextLeft :: Exp -> String
showPowContextLeft (Sum a b) = addParens (showSumContext (Sum a b))
showPowContextLeft (Prod a b) = addParens (showProdContext (Prod a b))
showPowContextLeft (Pow a b) = addParens (showSumContext (Pow a b))
showPowContextLeft (D u v)        =	 (showSumContext (D u v))
showPowContextLeft (Var x)        = x
showPowContextLeft (Sin a) =  (showSumContext(Sin a) )
showPowContextLeft (Cos a) =  (showSumContext(Cos a) )
showPowContextLeft (Exp a) =  (showSumContext(Exp a) )
showPowContextLeft (Ln a) =   (showSumContext(Ln a) )
showPowContextLeft (Int a b) =   (showSumContext(Int a b) )
showPowContextLeft (RExp n) | (denominator n)==1  =if (numerator n)<0 then "(" ++ (show (numerator n)) ++ ")" else (show (numerator n))
                        | otherwise           ="(" ++ (show (numerator n)) ++ "/"++ (show (denominator n)) ++ ")"

showPowContextRight :: Exp -> String
showPowContextRight (Sum a b) = addParens (showSumContext (Sum a b))
showPowContextRight (Prod a b) = addParens (showProdContext (Prod a b))
showPowContextRight (Pow a b) = (showSumContext (Pow a b))
showPowContextRight (D u v)        =(showSumContext (D u v))
showPowContextRight (Var x)        = x
showPowContextRight (Sin a) =  (showSumContext(Sin a) )
showPowContextRight (Cos a) =  (showSumContext(Cos a) )
showPowContextRight (Exp a) =  (showSumContext(Exp a) )
showPowContextRight (Ln a) =   (showSumContext(Ln a) )
showPowContextRight (Int a b) =   (showSumContext(Int a b) )
showPowContextRight (RExp n) | (denominator n)==1  =if (numerator n)<0 then "(" ++ (show (numerator n)) ++ ")" else (show (numerator n))
                        | otherwise           =(if (numerator n)<0 then "(" ++ (show (numerator n)) ++ ")" else (show (numerator n)) )++ "/"++
						                        (if (denominator n)<0 then "(" ++ (show (denominator n)) ++ ")" else (show (denominator n)))

instance Show Exp where
  show u              =  showSumContext u
{-
(Sum (RExp 2) (Sum (RExp 3) (RExp 4)))
(Sum (Sum (RExp 2) (RExp 3)) (RExp 4))
(Sum (RExp 2) (Prod (RExp 3) (RExp 4)))
(Prod (Sum (RExp 2) (RExp 3)) (RExp 4))
(Prod (RExp (2%3)) (RExp 3))
(Pow (RExp (2%3)) (RExp 3))
(Pow (Sum (RExp 1) (RExp 3)) (RExp 2))
(Pow (Prod (RExp 4) (RExp 3)) (RExp 2))
(Pow (Prod (RExp 4) (RExp 3)) (Sum (RExp 2) (RExp 4)))
(Pow (RExp (-1)) (RExp 2))
(Pow (Pow (RExp 2) (RExp 3)) (RExp 2))
(Pow (RExp 2) (Pow (RExp 3) (RExp 2)))

-}												
		   

--helper from MPoly.hs from notes
data MPoly = Const Rational | ProdPlus MPoly Kernel MPoly deriving (Show,Eq)

data Kernel = KVar String deriving Eq

instance Show Kernel where
  show (KVar s) = s

instance Ord Kernel where
  compare (KVar x) (KVar y) = compare x y
 

rationalEval :: Exp -> Rational
rationalEval (RExp n) = n
rationalEval (Var x) = error "Variable encountered in rational expression"
rationalEval (Sum u v) = (rationalEval u) + (rationalEval v)
rationalEval (Prod u v) = (rationalEval u) * (rationalEval v)
rationalEval (Pow u v) = let nv = (numerator (rationalEval v))
                   in if nv >= 0 then
                          toRational ((fromRational (rationalEval u)) ^ nv)
                       else error "Fraction encountered in integer expression"
					   

fromExp :: Exp -> MPoly
fromExp (RExp n)   = Const n
fromExp (Var x)    = fromVar x
fromExp (Sum u v)  = (fromExp u) + (fromExp v)
fromExp (Prod u v) = (fromExp u) * (fromExp v)
fromExp (Pow u v)  = let n = numerator (rationalEval v)
                     in if n >=0 then (fromExp u) ^ n
                        else error "Fractional polynomial encountered"
 
fromVar :: String -> MPoly
fromVar x = (ProdPlus (Const 1) (KVar x) (Const 0))

fromConst :: Rational -> MPoly
fromConst a = Const a

scale :: Rational -> MPoly -> MPoly
scale 0 p                  = Const 0
scale a (Const b)          = Const (a*b)
scale a (ProdPlus p1 x p2) = ProdPlus (scale a p1) x (scale a p2)

mulVar :: Kernel -> MPoly -> MPoly -- multiply by x
mulVar x (Const 0)   = (Const 0)
mulVar x p@(Const a) = (ProdPlus p x (Const 0))
mulVar y p@(ProdPlus p1 x p2)
  | x < y            = (ProdPlus (mulVar y p1) x (mulVar y p2))
  | x > y            = (ProdPlus p y (Const 0))
  | otherwise        = (ProdPlus p x (Const 0))

instance Num MPoly where
  (Const a) + (Const b) = Const (a+b)
  (ProdPlus p1 x p2) + (Const a) = ProdPlus p1 x ((Const a) + p2)
  (Const a) + (ProdPlus p1 x p2) = ProdPlus p1 x ((Const a) + p2)
  p@(ProdPlus p1 x p2) + p'@(ProdPlus p1' y p2') 
     | x < y     = ProdPlus p1 x (p2+p')
     | x > y     = ProdPlus p1' y (p+p2')
     | otherwise = normalPoly (p1 + p1') x (p2+p2')

  negate p = scale (-1) p

  (Const a) * p          = scale a p
  (ProdPlus p1 x p2) * p = (p1 * (x `mulVar` p)) + p2*p

  abs _ = error "abs not supported for type MPoly"
  signum _ = error "signum not supported for type MPoly"

  fromInteger  = fromConst . fromInteger
  
normalPoly :: MPoly -> Kernel -> MPoly -> MPoly
normalPoly (Const 0) x p2 = p2
normalPoly p1 x p2        = ProdPlus p1 x p2

value :: [(String, Rational)] -> String-> Rational                        
value env c = case lookup c env of
                Nothing -> toRational (0)
                Just n  -> n


data Eqn = Eqn Exp Exp deriving (Eq,Show)
--helper to convert list of equations to list containing list of tuples with the variables and their coefficients 
convertall :: [Eqn] -> [[(String,Rational)]]
convertall [] = []
convertall (x:xs) = (convert x):(convertall xs)
  where convert (Eqn x y) = traverse (fromExp x) (fromExp y) 
--helper to traverse through the MPoly and find the list of tuples with the variables and their coefficients  
traverse :: MPoly -> MPoly -> [(String,Rational)]
traverse (ProdPlus (Const m) (KVar x) (Const n)) (Const y)=[(x,m)]++[("value",(y-n))]
traverse (ProdPlus (Const m) (KVar x) n) y =[(x,m)]++ (traverse n y) 
--helper to find the list of variables used in the list of equations without duplicates
variablelist :: [[(String,Rational)]]->[String] 
variablelist [] = []
variablelist (x:xs) = sortBy compare (delete "value" (nub ((variablefinder x)++(variablelist xs))))
  where variablefinder [] = []
        variablefinder ((x,_):xs)=x:(variablefinder xs)
--helper to sort the list of rationals with non zero first element		
nonZeroFirst1 :: [[Rational]] -> [[Rational]]
nonZeroFirst1 xxs = if (find1 xxs)==[] then xxs else (find1 xxs):(delete (find1 xxs) xxs)
--helper for finding if there is any element in the list with non zero first element
find1 :: [[Rational]] -> [Rational]
find1 ((x:xs):xxs) = if x/=(toRational 0) then (x:xs) else (if xxs/=[] then (find1 xxs) else [])
		
--helper to convert the list of list of tuples into list required for solvesystem method		
systemcaller ::  [[(String,Rational)]] -> [String] -> [[Rational]]
systemcaller (x:[]) variables = [(systemcaller1 x variables)]
systemcaller (x:xs) variables = [(systemcaller1 x variables)] ++ (systemcaller xs variables)
--helper to look for the values from tuples and insert them in the list
systemcaller1 :: [(String,Rational)] -> [String] -> [Rational]
systemcaller1 tuples []      = []
systemcaller1 tuples (x:xs) =take (length (x:xs) +1) (((value tuples x):(systemcaller1 tuples xs))++[(value tuples "value")])

system :: [Eqn] -> [[Rational]]
system xs = nonZeroFirst1 (systemcaller tuples variables)
  where variables = variablelist (convertall xs)
        tuples    = convertall xs
{-
system [(Eqn (Prod (RExp 2) (Var "y")) (RExp 10))]
system [(Eqn (Var "y") (RExp 5)),(Eqn (Sum (Var "x") (Var "y")) (RExp 2))]
system [(Eqn (Var "y") (RExp 5)),(Eqn (Sum (Var "x") (RExp 1)) (RExp 2))]
system [(Eqn (Sum (Prod (RExp 2) (Var "x"))(Sum (Prod (RExp 3) (Var "z"))(Prod (RExp 3) (Var "y"))))(RExp 8)),(Eqn (Sum (Prod (RExp (-2)) (Var "z"))(Sum (Prod (RExp 3) (Var "y"))(Prod (RExp 2) (Var "x"))))(RExp 3)),(Eqn (Sum (Prod (RExp (-2)) (Var "y"))(Sum (Prod (RExp 4) (Var "x"))(Prod (RExp 2) (Var "z"))))(RExp 4))]
-}

