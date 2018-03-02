{-
Program: HW4Part2.fieldsbe.hs
Author: Benjamin Fields
Date: 10-30-2017
Description: This part of the assignment takes the previosly defined abstract syntax
and redifines it to be type safe. 
-}
module HW4 where


-- ******************************************************
{-
Section 1: New Syntax

int	::=	(any integer)	integers
 			
bool	::=	true   |   false	
 			
reg	::=	A   |   B

num ::= int 
|	int + int 
|   reg                **load value at register
 			
expr	::=	num <= num	
|	not bool	
|	bool & bool
 			
stmt	::=	reg := num	***store val
|	if expr	
then prog	
else prog	
end	
 			
prog	::=	ε  |  stmt ; prog	

-}

-- ******************************************************



{-
Section 2: Haskell definition

-}
data Reg = A | B
   deriving(Show)

data MyNum = MyInt Int | Add MyNum MyNum | Load Reg
   deriving(Show)

data Exp =  MyBool Bool |  GTEQ MyNum MyNum | Not Exp | AND Exp Exp
   deriving(Show)

data Stmt = Store Reg MyNum | IF Exp Prog Prog
   deriving(Show)

type Prog = [Stmt]


