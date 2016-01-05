-------------------------------------------------------------------------------------------
--Question 1

module Parser4 where
 
import Scanner4
import Data.Ratio
import Data.List
 
data Exp = RExp Rational  |
           Var String     |
           Sum Exp Exp    |
           Diff Exp Exp   |
           Prod Exp Exp   |
           Quo Exp Exp    |
           Neg Exp        |
           Let ExpSeq Exp |
           ParseError String deriving (Eq,Show)
		   

data ExpSeq = Seq [Binding]|ParseEqnSeqError String deriving (Eq, Show)
 
 
data Binding = Bind String Exp | Eqn Exp Exp deriving (Eq, Show)
 
stringFromToken :: Token -> String
stringFromToken (Compound (Id s)) = s
 
integerFromToken :: Token -> Rational
integerFromToken (Compound (Num n)) = fromRational n
 
 
newtype Parser a = Parser ([Token] -> [(a, [Token])])
 
unWrap (Parser f) = f
 
instance Monad Parser where
  return a = Parser(\ts->[(a, ts)])
  p >>= f = Parser(\ts->[(b, ts2) | (a, ts1) <- unWrap p ts, (b, ts2) <- unWrap (f a) ts1])
 
failParser :: Parser a
failParser = Parser(\ts-> [])
 
item :: Parser Token
item = Parser(\ts->case ts of [] -> []; t:ts1 -> [(t,ts1)])
 
parserFilter :: Parser b -> (b -> Bool) -> Parser b
parserFilter parser p = do {a <- parser; if p a then return a else failParser}
 
literal :: Token -> Parser Token
literal t = parserFilter item (==t)
 
variable :: Parser Token
variable =  parserFilter item (\tok->case tok of (Compound (Id _)) -> True; _ -> False)
 
number :: Parser Token
number =  parserFilter item (\tok->case tok of (Compound (Num _)) -> True; _ -> False)
 
(+++) :: Parser a -> Parser a -> Parser a
p1 +++ p2 = Parser(\ts-> (unWrap p1 ts) ++ (unWrap p2 ts))
 

getSeq :: Parser ExpSeq
getSeq = getSeq2 []
 
getSeq2 :: [Binding] -> Parser ExpSeq
getSeq2 revBinds =
  do binding <- getQuo
     ((do tok <- (literal (Simple COMMA))
          getSeq2 (binding:revBinds))
       +++
      (return (Seq (reverse (binding:revBinds)))))
 

getQuo :: Parser Binding
getQuo =
  do vtok <- getExp
     tok <- (literal (Simple EQ1))
     exp <- getExp
     return (Eqn vtok exp)
 

getExp :: Parser Exp
getExp =
  do term <- getTerm
     getExp' term
 
getExp' :: Exp -> Parser Exp
getExp' term =
  (do tok <- (literal (Simple PLUS))
      term2 <- getTerm
      getExp' (Sum term term2))
  +++
  (do tok <- (literal (Simple MINUS))
      term2 <- getTerm
      getExp' (Diff term term2))
  +++
   (return term)
 
getTerm :: Parser Exp
getTerm =
  do factor <- getFactor
     getTerm' factor
 
getTerm' :: Exp -> Parser Exp
getTerm' factor =
  (do tok <- (literal (Simple STAR))
      factor2 <- getFactor
      getTerm' (Prod factor factor2))
  +++
  (do tok <- (literal (Simple SLASH))
      factor2 <- getFactor
      getTerm' (Quo factor factor2))
  +++
   (return factor)
 
getFactor :: Parser Exp
getFactor =
  (do vtok <- variable
      return (Var (stringFromToken vtok)))
  +++
  (do ntok <- number
      return (RExp (integerFromToken ntok)))
  +++
  (do tok <- (literal (Simple MINUS))
      factor <- getFactor
      return (Neg factor))
  +++
  (do tok <- (literal (Simple OP))
      exp <- getExp
      tok <- (literal (Simple CP))
      return exp)
 
parse :: [Token] -> ExpSeq
parse ts =
  case unWrap getSeq ts of
    []            -> ParseEqnSeqError "Bad input"
    (exp, ts1):ps -> if isEmptyTokenStream ts1
                     then exp
                     else ParseEqnSeqError "Unconsumed input"
 
parseString :: String -> ExpSeq
parseString = parse . tokenStreamFromString
{-
parse (tokenStreamFromString "w=1/2*w+20")
parse (tokenStreamFromString "2*y+x=10, 2*x+1=9")
parse (tokenStreamFromString "w=/2*w+20")
-}
--------------------------------------------------------------------------------
--Question 2 (a)
data RegExp sigma = RegEmpty							  |
					RegEpsilon		 					  |
					RegSym sigma						  |
					RegOr (RegExp sigma) (RegExp sigma)   |
					RegSeq (RegExp sigma) (RegExp sigma)  |
					RegStar (RegExp sigma)  		deriving (Show,Eq)
{-
RegEmpty
RegOr (RegSym 'a') (RegSym 'b') 
-}	
-------------------------------------------------------------------------------------------
--Question 2 (b)				
data NFA q sigma = NFA (q->Maybe sigma->[q]) q [q] 
---------------------------------------------------------------------------------------------
--Question 2 (c)
epsilonClosure :: Ord t => NFA t t1 -> [t] -> [t]
epsilonClosure _   []  = []
epsilonClosure (NFA f i fin) xx@(x:xs)= epsilontraker (NFA f i fin) (nub (x:(f x Nothing) ++ (epsilonClosure (NFA f i fin) xs))) xx
 where epsilontraker (NFA f i fin) x y | (sort x)==(sort y) = x
									   | otherwise          = ((epsilonClosure (NFA f i fin) x))
{-
epsilonClosure (nfaFromRegExp (RegOr (RegSym 'a') (RegSym 'b'))) [1..4]
epsilonClosure (nfaFromRegExp (RegSeq (RegSym 'a') (RegSym 'b'))) [1..4]
-}
----------------------------------------------------------------------------------------------									   
--Question 2 (d)
data Str = EStr | ConCat Str Char deriving (Eq,Show)

deltaStar :: Ord t => NFA t Char -> t -> Str -> [t]
deltaStar (NFA f s fin) = helper2 
	where				
	helper2 i EStr = epsilonClosure (NFA f i fin) [i] 
	helper2 i (ConCat a b) =epsilonClosure (NFA f s fin) (nub (concat ([(f r (Just b))|r<-(helper2 i a)])))
					     

{-
deltaStar  (nfaFromRegExp (RegSeq (RegSym 'a') (RegSym 'b'))) 0 EStr
deltaStar  (nfaFromRegExp (RegOr (RegSym 'a') (RegSym 'b'))) 1 (strtoConCat "a")
-}
-----------------------------------------------------------------------------------------------
--Question 2 (e)
strtoConCat::String->Str
strtoConCat  []  	= EStr
strtoConCat  [x] 	= ConCat EStr x
strtoConCat   xxs 	= ConCat (strtoConCat (init xxs)) (last xxs)
{-
(strtoConCat "a")
(strtoConCat "abc")
-}
doesAccept :: Ord a => NFA a Char -> String -> Bool
doesAccept (NFA f i fin) xs =(intersect result fin)/=[] 
 where result=deltaStar (NFA f i fin) i (strtoConCat xs)
{-
doesAccept (nfaFromRegExp RegEmpty) ""
doesAccept (nfaFromRegExp RegEmpty) "a"
doesAccept (nfaFromRegExp RegEpsilon) ""
doesAccept (nfaFromRegExp RegEpsilon) "a"
doesAccept (nfaFromRegExp RegEpsilon) "b"
doesAccept (nfaFromRegExp (RegSym 'a')) ""
doesAccept (nfaFromRegExp (RegSym 'a')) "a"
doesAccept (nfaFromRegExp (RegSym 'a')) "b"
doesAccept (nfaFromRegExp (RegSym 'a')) "aa"
doesAccept (nfaFromRegExp (RegOr (RegSym 'a') (RegSym 'b'))) ""
doesAccept (nfaFromRegExp (RegOr (RegSym 'a') (RegSym 'b'))) "a"
doesAccept (nfaFromRegExp (RegOr (RegSym 'a') (RegSym 'b'))) "b"
doesAccept (nfaFromRegExp (RegOr (RegSym 'a') (RegSym 'b'))) "ab"
doesAccept (nfaFromRegExp (RegSeq (RegSym 'a') (RegSym 'b'))) ""
doesAccept (nfaFromRegExp (RegSeq (RegSym 'a') (RegSym 'b'))) "a"
doesAccept (nfaFromRegExp (RegSeq (RegSym 'a') (RegSym 'b'))) "b"
doesAccept (nfaFromRegExp (RegSeq (RegSym 'a') (RegSym 'b'))) "ba"
doesAccept (nfaFromRegExp (RegSeq (RegSym 'a') (RegSym 'b'))) "ab"
doesAccept (nfaFromRegExp (RegSeq (RegSym 'a') (RegSym 'b'))) "aba"
doesAccept (nfaFromRegExp (RegStar (RegSym 'a'))) ""
doesAccept (nfaFromRegExp (RegStar (RegSym 'a'))) "a"
doesAccept (nfaFromRegExp (RegStar (RegSym 'a'))) "aa"
doesAccept (nfaFromRegExp (RegStar (RegSym 'a'))) "b"
doesAccept (nfaFromRegExp (RegStar (RegSym 'a'))) "ab"
-}
------------------------------------------------------------------------------------------------
--Question 2 (f)

traverse' :: Num t => RegExp a -> t -> ([((t, Maybe a), [t])], t)
traverse' (RegEmpty) i   = ([((i,Nothing),[])],i+1)
traverse' (RegEpsilon) i = ([((i,Nothing),[i])],i)
traverse' (RegSym a) i = ([((i,Just a),[i+1])],i+1)
traverse' (RegOr a b) i = ([((i,Nothing),[i+1,v+1])]++[((v,Nothing),[z+1])]++[((z,Nothing),[z+1])]++u++x,z+1)
	where 
	(u)=fst(traverse' a (i+1))
	(v)=snd(traverse' a (i+1))
	(x)=fst(traverse' b (v+1))	
	(z)=snd(traverse' b (v+1))	
traverse' (RegSeq a b) i = ([((v,Nothing),[v+1])]++u++x,z)
	where
	(u)=fst(traverse' a (i))
	(v)=snd(traverse' a (i))
	(x)=fst(traverse' b (v+1))	
	(z)=snd(traverse' b (v+1))
traverse' (RegStar a) i  = ([((i,Nothing),[v+1,i+1])]++[((v,Nothing),[v+1,i+1])]++u,v+1)
    where
   (u)=fst(traverse' a (i+1))
   (v)=snd(traverse' a (i+1))
		

nfaFromRegExp:: (Num q, Eq sigma, Eq q) => RegExp sigma -> NFA q sigma		
nfaFromRegExp x    = (NFA trans 1 [b])
	where	
	(a)	  = fst (traverse' x 1)
	(b)	  = snd (traverse' x 1)
	trans q c = case lookup (q,c) a  of
				Nothing -> []
				Just s  -> s
{-
(nfaFromRegExp (RegSeq (RegSym ’a’) (RegSym ’b’)))
(nfaFromRegExp (RegStar (RegSym ’a’)))
-}
--------------------------------------------------------------------------------
--Question 3
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

					   

fromExp :: Exp -> MPoly
fromExp (RExp n)   = Const n
fromExp (Var x)    = fromVar x
fromExp (Sum u v)  = (fromExp u) + (fromExp v)
fromExp (Prod u v) = (fromExp u) * (fromExp v)

 
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


--helper to convert list of equations to list containing list of tuples with the variables and their coefficients 
convertall :: [Binding] -> [[(String,Rational)]]
convertall [] = []
convertall (x:xs) = (convert x):(convertall xs)
  where convert (Eqn x y) = traverse (fromExp x) (fromExp y) 
--helper to traverse through the MPoly and find the list of tuples with the variables and their coefficients  
traverse :: MPoly -> MPoly -> [(String,Rational)]
traverse (ProdPlus (Const m) (KVar x) (Const n)) (Const y)=[(x,m)]++[("value",(y-n))]
traverse (ProdPlus (Const n1) (KVar x) (Const n2)) (ProdPlus (Const m1) (KVar y) (Const m2))
										| x==y        = [(x,(n1-m1))]++[("value", n2-m2)]
										| otherwise   = [(x,n1)]++[(y,-m1)]++[("value",(m2-n2))]
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

system :: [Binding] -> [[Rational]]
system xs = nonZeroFirst1 (systemcaller tuples variables)
  where variables = variablelist (convertall xs)
        tuples    = convertall xs
------------------------------------------------------------------------------------
sub :: Num a => [a] -> [a] -> [a]
sub [] []         = []
sub [] (y:ys)     = (-y):(sub [] ys)
sub xs []     	  = xs
sub (x:xs) (y:ys) = (x-y):(sub xs ys)
{-
sub [2,2,3] [2,5,12] 
sub [3,4,5,6] [1,1,1,1]
-}
----------------------------------------------------------------------------

scaleList :: Num t => t -> [t] -> [t]
scaleList a [] = []
scaleList a (x:xs) = (a*x):(scaleList a xs)
{-
scaleList (1/2) [2,5,12]
scaleList (2) [2,5,12]
-}
----------------------------------------------------------------------------

subScale :: [Double] -> [Double] -> [Double]
subScale [] []  = []
subScale [] [a] = []
subScale [a] [] = []
subScale (x:xs) (y:ys) = sub (tail (scaleList (((lcm x y)`div`y)) (y:ys))) (tail (scaleList (((lcm x y)`div`x)) (x:xs)))
{-
subScale [2,2,3,10] [2,5,12,31]
subScale [2,3,3,8] [4,-2,2,4]
-}
----------------------------------------------------------------------------

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
----------------------------------------------------------------------------

instance (Integral Double)  where
 toInteger a = round a
 quotRem a b = (fromIntegral(a/b),a-(b*(fromIntegral(a/b))))
 div a b = a/b

triangulate :: [[Double]] -> [[Double]]
triangulate (xs:[])    = [xs]
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

----------------------------------------------------------------------------

dot :: Num a => [a] -> [a] -> a
dot []     []     =  0
dot xs     []     =  0
dot []     xs     =  0
dot (f:fs) (s:ss) = (f*s) + (dot fs ss)
{-
dot [1,2] [3,4]
dot [1,2,5] [3,4,6]
-}
----------------------------------------------------------------------------

solveLine :: Fractional a => [a] -> [a] -> a
solveLine xs ys =  ((last xs) -(dot (init (tail xs)) ys ))/(head xs)  
{-
solveLine [2,3,3,8] [1,1]
solveLine [-5,-5] []
-}
----------------------------------------------------------------------------

solveTriangular :: Fractional a => [[a]] -> [a]
solveTriangular s2@(s:[]) = (solveLine (head s2) []):[] 
solveTriangular (x:ys) = [(solveLine x ((solveTriangular ys)))]++((solveTriangular ys))
{-
solveTriangular [[2.0,3.0,3.0,8.0],[-8.0,-4.0,-12.0],[-5.0,-5.0]]
solveTriangular [[1.0,2.0,-3.0,4.0,12.0],[-2.0,4.0,-5.0,-14.0],[6.0,-5.0,-16.0],[1.0,2.0]]
-}
----------------------------------------------------------------------------

solveSystem :: [[Double]] -> [Double]
solveSystem xxs = solveTriangular (triangulate xxs)
-------------------------------------------------------------------------------------		
-- helpers for question 3
unsequence :: ExpSeq -> [Binding]
unsequence (Seq a)=a

doublelistmaker :: [[Rational]] -> [[Double]]
doublelistmaker []    = []
doublelistmaker (l:ls)=[doublemaker l] ++ doublelistmaker ls

doublemaker :: [Rational] -> [Double]
doublemaker []     = []
doublemaker (c:cs) = (fromRational c):(doublemaker cs)

equationsolver :: IO [Double]
equationsolver = do  
			putStrLn "Enter a list of linear equations separated by COMMA" 
			b <- getLine    
			putStrLn "The values associated with the variables in order as list is"
			case (parse (tokenStreamFromString b)) of
				(ParseEqnSeqError a) -> error (( a))
				(Seq a) -> return ( (solveSystem (doublelistmaker (system a))))
{-
equationsolver
2*y+x=10, 2*x+1=9
equationsolver
x+2=z,x+z=2
-}

-------------------------------------------------------------------------------------------
