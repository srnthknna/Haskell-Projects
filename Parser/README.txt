Program for Simple parser using automata and to parse the system of linear equation in polynomial form and solve them
To compile the file run
:l Parser4.hs

The language is 
data Exp = RExp Rational  |
           Var String     |
           Sum Exp Exp    |
           Diff Exp Exp   |
           Prod Exp Exp   |
           Quo Exp Exp    |
           Neg Exp        |
           Let ExpSeq Exp |
           ParseError String 
		   
data ExpSeq = Seq [Binding]|ParseEqnSeqError String deriving (Eq, Show)
 
 
data Binding = Bind String Exp | Eqn Exp Exp deriving (Eq, Show)

Parse the equations using
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

The linear equations can be parsed and solved using
equationsolver
2*y+x=10, 2*x+1=9
equationsolver
x+2=z,x+z=2
