import Data.Char
import Data.List

--Sudoku Solver
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


data SudokuConfig = SudokuConfig [Integer] 
{-
SudokuConfig trivial
SudokuConfig diabolical
-}

sudokuConfigFromList :: [Integer] -> SudokuConfig
sudokuConfigFromList a = SudokuConfig a
{-
sudokuConfigFromList trivial
sudokuConfigFromList diabolical
-}

listFromSudokuConfig :: SudokuConfig -> [Integer]
listFromSudokuConfig (SudokuConfig a)  = a
{-
listFromSudokuConfig (SudokuConfig trivial)
listFromSudokuConfig (SudokuConfig diabolical)
-}

instance Eq SudokuConfig where
   (SudokuConfig list1) == (SudokuConfig list2) = (list1==list2) 
{-
(SudokuConfig trivial)==(SudokuConfig trivial)
(SudokuConfig trivial)/=(SudokuConfig trivial)
-}

instance Show SudokuConfig where
 show (SudokuConfig a) = sudokushow a 0
	where   sudokushow [] _   = [] 
		sudokushow ls 3 = "\n"++(sudokushow ls 0) 
		sudokushow ls n = "\n"++(rowshow (take 9 ls) "" 0)  ++ "\n" ++ sudokushow (drop 9 ls) (n + 1)
 		rowshow [] str _     = str ++ "  "
		rowshow x  str 3     = rowshow x (str ++ "  ") 0
		rowshow (x:xs) str n = (rowshow xs (str ++ " " ++ (if x==0 then "_" else (show x))) (n + 1)) 
{-
Just (sudokuConfigFromList trivial)Just (sudokuConfigFromList diabolical)-}

instance Config SudokuConfig where
  successors (SudokuConfig [] ) = []
  successors (SudokuConfig ds ) = [SudokuConfig (replace (findsuccessor ds) x ds)| x<-(cellpossibilities (ds))]
{-
successors (SudokuConfig trivial)
successors (SudokuConfig diabolical)
-}

--Helper for finding successors
-- function to find next index to be solved
findsuccessor :: [Integer] -> Integer
findsuccessor (ds) = findsuccessor1 ds 0
findsuccessor1 ds 80   = if (getelementByIndex ds 0 80)==0 then 80 else (error "error cant solve")
findsuccessor1 ds x = if (getelementByIndex ds 0 x)==0 then x else (findsuccessor1 ds (x+1))
{-
findsuccessor trivial
findsuccessor diabolical
-}

--function to replace a cell in the SudokuConfig
replace :: (Num a,Eq a) => a -> a1 -> [a1] -> [a1]
replace n newVal (x:xs) = if n == 0  then (newVal:xs) else (x:(replace (n-1) newVal xs))
{-
replace 0 1 trivial
replace 1 2 trivial
-}

--function to find the possible list of values that can be filled in first blank cell
cellpossibilities :: [Integer] -> [Integer]
cellpossibilities ls = [1..9] \\ (getelementByIndexList ls (cellIndices (findsuccessor (ls))))
{-
cellpossibilities trivial
cellpossibilities diabolical
-}

isSudokuGoal::SudokuConfig->Bool
isSudokuGoal (SudokuConfig []) = False
isSudokuGoal (SudokuConfig ls) =  checkForEveryElement ls 0
{-
isSudokuGoal (SudokuConfig trivial)
isSudokuGoal (SudokuConfig diabolical)
-}

--Helper of isSudokuGoal
--function to get element present in the sudoku with index number
getelementByIndex ::[Integer]->Integer->Integer->Integer
getelementByIndex (x:xs) start position = if (start==position) then x else (getelementByIndex xs (start+1) position)
{-
getelementByIndex trivial 0 50
getelementByIndex trivial 0 0
-}

--function to get all elements present in the index list
getelementByIndexList ::[Integer]->[Integer]->[Integer]
getelementByIndexList ls [] = []
getelementByIndexList ls (x:xs) = (getelementByIndex ls 0 x):(getelementByIndexList ls xs)
{-
getelementByIndexList trivial [1,2,3,4]
getelementByIndexList trivial [5,6,7,8,9]
-}

--function to check if the cell has no clashes
checkElement:: Integer->[Integer] ->Bool
checkElement index ls = checkForSameElement (getelementByIndex ls 0 index) (getelementByIndexList ls (cellIndices index)) 0
{-
checkElement 0 trivial
checkElement 2 trivial
-}

--function to check all the indices corresponding to the cell for correctness
checkForEveryElement :: [Integer]->Integer->Bool
checkForEveryElement ls n = if n==80 then (checkElement 80 ls) else (checkElement n ls && (checkForEveryElement ls (n+1)))
{-
checkForEveryElement trivial 6
checkForEveryElement trivial 80
-}

--function to check if the element in the index is present only once in the corresponding row, column and block
checkForSameElement ::Integer->[Integer]->Integer->Bool
checkForSameElement  x [] n = if n==1 then True else False
checkForSameElement  x (y:ys) n = if (x==y||y==0) then (checkForSameElement x ys (n+1)) else (checkForSameElement  x ys n)
{-
checkForSameElement 0 trivial 0
checkForSameElement 80 trivial 0
-}

--function to give column indices
column::Integer -> [Integer]
column n = (adder n 0 9)
{-
column 0
column 1
-}

--function to provide next elements for row and column by increasing
adder :: (Num t,Num a,Eq a) => t -> a -> t -> [t]
adder a 8 x = [a]
adder n m x = [n]++(adder (n+x) (m+1) x)
{-
adder 0 0 1
adder 0 0 9
-}

--function to provide row indices
row :: Integer -> [Integer]
row n = (adder (n*9) 0 1)
{-
row 0
row 1
-}

--function to give nth block of indices
block :: Integer -> [Integer]   
block n = block n (getstart (n)) 0
  where block n ls 0 = (ls):(ls+1):[ls+2]++(block (ls+9) ls 1)
	block n ls 1 = (n):(n+1):[n+2]++(block (n+9) ls 2)
	block n ls 2 = (n):(n+1):[n+2]
{-
block 8
block 5
-}

--funtion to get starting index for nth block
getstart :: (Ord a,Num a) => a -> a
getstart n | n<=2   = n*3
	   | n <= 5 = 27 + (n * 3 - 9)
           | n <= 8 = 54 + (n * 3 - 18)
{-
getstart 8
getstart 5
-}

--function to get the list of row indices corresponding to cell index
rowIndices :: Integer -> Integer -> [Integer]
rowIndices x 8 = if (elem x (row 8)) then  (row 8) else (error "Not present") 
rowIndices x n = if (elem x (row n)) then  (row n) else (rowIndices x (n+1)) 
{-
rowIndices 60 0
rowIndices 80 0
-}

--function to get the list of column indices corresponding to cell index
columnIndices :: Integer -> Integer -> [Integer]
columnIndices x 8 = if (elem x (column 8)) then  (column 8) else (error "Not present") 
columnIndices x n = if (elem x (column n)) then  (column n) else (columnIndices x (n+1)) 
{-
columnIndices 60 0
columnIndices 80 0
-}

--function to get the list of block indices corresponding to cell index
blockIndices :: Integer -> Integer -> [Integer]
blockIndices x 8 = if (elem x (block 8)) then  (block 8) else (error "Not present") 
blockIndices x n = if (elem x (block n)) then  (block n) else (blockIndices x (n+1)) 
{-
blockIndices 60 0
blockIndices 80 0
-}

--function to get the list of indicies to be checked corresponding to cell index
cellIndices :: Integer -> [Integer]
cellIndices n = sort $ nub $ (rowIndices n 0) ++ (columnIndices n 0) ++ (blockIndices n 0)
{-
cellIndices 60 
cellIndices 80 
-}

sudokuSolve:: SudokuConfig->(Maybe SudokuConfig)
sudokuSolve (SudokuConfig ls) = solve isSudokuGoal (SudokuConfig ls)
{-
sudokuSolve (SudokuConfig trivial)
sudokuSolve (SudokuConfig problem)
-}

--related sudoku
trivial =    [ 0, 4, 6,  0, 0, 0,  8, 9, 0,
               0, 7, 0,  4, 0, 9,  0, 1, 0,
               5, 0, 0,  0, 8, 0,  0, 0, 6,

               0, 0, 3,  9, 0, 8,  6, 0, 0,
               9, 0, 0,  0, 0, 0,  0, 0, 2,
               0, 0, 8,  5, 0, 2,  1, 0, 0,

               4, 0, 0,  0, 5, 0,  0, 0, 3,
               0, 2, 0,  1, 0, 6,  0, 7, 0,
               0, 9, 7,  0, 0, 0,  5, 2, 0 ];

profi =      [ 1, 0, 7,  0, 0, 9,  8, 0, 4,
               0, 0, 3,  0, 4, 0,  0, 0, 0,
               8, 0, 2,  0, 0, 5,  0, 0, 6,
            
               0, 0, 0,  0, 8, 0,  3, 0, 0,
               0, 0, 0,  5, 0, 0,  0, 0, 0,
               2, 3, 8,  0, 0, 0,  0, 1, 0,
            
               0, 8, 1,  0, 0, 6,  5, 0, 0,
               0, 2, 0,  0, 0, 4,  0, 0, 0,
               3, 0, 0,  9, 0, 8,  7, 0, 0 ];

diabolical = [ 8, 0, 0,  7, 0, 1,  0, 0, 2,
               0, 0, 6,  0, 0, 0,  7, 0, 0,
               0, 1, 7,  0, 0, 0,  8, 9, 0,
               
               0, 0, 0,  1, 7, 3,  0, 0, 0,
               7, 0, 0,  0, 0, 0,  0, 0, 6,
               0, 0, 0,  9, 5, 6,  0, 0, 0,
               
               0, 9, 5,  0, 0, 0,  4, 1, 0,
               0, 0, 8,  0, 0, 0,  5, 0, 0,
               3, 0, 0,  6, 0, 5,  0, 0, 7 ];

problem =    [ 1, 0, 0,  0, 0, 0,  0, 0, 9,
               0, 5, 0,  0, 0, 0,  0, 2, 0,
               0, 0, 9,  0, 0, 0,  4, 0, 0,

               0, 0, 0,  5, 6, 7,  0, 0, 0,
               0, 0, 0,  8, 9, 1,  0, 0, 0,
               0, 0, 0,  2, 3, 4,  0, 0, 0,

               0, 0, 5,  0, 0, 0,  9, 0, 0,
               0, 7, 0,  0, 0, 0,  0, 4, 0,
               9, 0, 0,  0, 0, 0,  0, 0, 8 ];




