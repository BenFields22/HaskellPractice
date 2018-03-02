{-
Program: HW4Part1.fieldsbe.hs
Author: Benjamin Fields
Date: 10-30-2017
Description: This program takes the provided abstract syntax and defines it in terms of 
	haskell data types. The syntax is then used to create to functions. An example function
	is generated and a new function is created.
-}
module HW4 where

{-
	**************HW4Part1*******************
	Section 1:
	Encoding of the abstract syntax
	Description: This section defines the syntax in Haskell data types for the 
	creation of correct programs.
-}

data Reg = A | B
   deriving(Show)

data Exp = MyInt Int | MyBool Bool | Load Reg | Add Exp Exp | GTEQ Exp Exp | Not Exp | AND Exp Exp
   deriving(Show)

data Stmt = Store Reg Exp | IF Exp Prog Prog
   deriving(Show)

type Prog = [Stmt]


{-
	**************HW4Part1*******************
	Section 2:
	Example program defined as a haskell value.
-}
ex::Prog
ex = [Store A (MyInt 3),
   Store B (Add (Load A) (MyInt 2)),
   IF (GTEQ (Load A) (Load B)) [Store A (Add (Load A) (Load A))] [Store B (Add (Load B) (Load B))],
   Store B (Add (Load A) (Load B))]


{-
	**************HW4Part1*******************
	Section 3:
	Description: This function builds the program by iterating through the provided list backwards. This way
	the initialization to 0 can be the first thing done. At each step the prog checks if the current value is
	less than or equal to 5. If so it runs the program to store a new value into register a by 
	loading the value at reg a currently and adding the current integer.
-}
sumFiveOrLess :: [Int] -> Prog
sumFiveOrLess [] = []
sumFiveOrLess (x:[]) = [Store A (MyInt 0), IF (GTEQ (MyInt 5) (MyInt x)) [Store A (Add (Load A) (MyInt x))] []]
sumFiveOrLess (x:xs) = sumFiveOrLess xs ++ [IF (GTEQ (MyInt 5) (MyInt x)) [Store A (Add (Load A) (MyInt x))] []] 


--Testing of the function
myVals::[Int]
myVals = [1,2,3]

test::Prog
test = sumFiveOrLess myVals







