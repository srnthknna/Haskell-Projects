
To compile SatSolver.hs
:l SatSolver.hs

To solve the SAT equations enter in the form as examples given:-
satSolve (Or (And (Not (Var "a")) (Not (Var "b"))) (And (Not (Var "a")) (Not (Var "b"))))
satSolve (Or (And (Not (Var "a")) (Not (Var "b"))) (Or (Not (Var "c")) (Not (Var "d"))))
satSolve (And (And (Not (Var "c")) (Not (Var "a"))) (Or (Not (Var "c")) (Not (Var "c"))))
satSolve (Not (Not (And (Not (Var "a")) (Not (Var "a")))))
satSolve (And (BConst False) (Var "a"))