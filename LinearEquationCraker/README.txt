To execute Linear Equation Cracker run
:l LEC.hs

To solve equations enter the Equations in matrix form enter
solveSystem [[2,3,3,8],[2,3,-2,3],[4,-2,2,4]]
solveSystem [[1.0,2.0,-3.0,4.0,12.0],[2.0,2.0,-2.0,3.0,10.0],[0.0,1.0,1.0,0.0,-1.0],[1.0,-1.0,1.0,-2.0,-4.0]]

To convert the equations in polynomial form to matrix form to solve using above method enter
system [(Eqn (Prod (RExp 2) (Var "y")) (RExp 10))]
system [(Eqn (Var "y") (RExp 5)),(Eqn (Sum (Var "x") (Var "y")) (RExp 2))]
system [(Eqn (Var "y") (RExp 5)),(Eqn (Sum (Var "x") (RExp 1)) (RExp 2))]
system [(Eqn (Sum (Prod (RExp 2) (Var "x"))(Sum (Prod (RExp 3) (Var "z"))(Prod (RExp 3) (Var "y"))))(RExp 8)),(Eqn (Sum (Prod (RExp (-2)) (Var "z"))(Sum (Prod (RExp 3) (Var "y"))(Prod (RExp 2) (Var "x"))))(RExp 3)),(Eqn (Sum (Prod (RExp (-2)) (Var "y"))(Sum (Prod (RExp 4) (Var "x"))(Prod (RExp 2) (Var "z"))))(RExp 4))]
