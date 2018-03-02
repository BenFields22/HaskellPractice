module HW5 where

import Prelude hiding (and,or,not,pred,succ,fst,snd,either)

import DeBruijn
import Church




--
-- * Part 1: Church pair update functions
--

-- | 1. A lambda calculus function that replaces the first element in a
--   Church-encoded pair. The first argument to the function is the original
--   pair, the second is the new first element.
--
--   >>> :{
--     eval (app2 pair true (num 3)) ==
--     eval (app2 setFst (app2 pair (num 2) (num 3)) true)
--   :}
--   True
--

{-
Description: After hours and hours of attempts I could not figure this out. I cannot understand how 
we are supposed to correctly swap a value using lambda calculus. I understand that there has to be a way to
order the problem so the proper swaps take place but I could not figure it out with this 
haskell implementation. I tried to create a pair from the passed parameters and also tried to use 
a similar set up as the pair with three abstractions to replace the body with the proper elements when
substituted but it did not work. 
-}
setFst :: Exp
setFst = abs3 (app3 (Ref 0) (app2 (pair) (Ref 1) (Ref 2)) (App snd (Ref 2)) (Ref 1))

-- setFst = abs2 (app2 (sub (fst (Ref 1))) (Ref 0) (Ref 1))

-- | 2. A lambda calculus function that replaces the second element in a
--   Church-encoded pair. The first argument to the function is the original
--   pair, the second is the new second element.
--
--   >>> :{
--     eval (app2 pair (num 2) true) ==
--     eval (app2 setSnd (app2 pair (num 2) (num 3)) true)
--   :}
--   True
--
{-
Description:After hours and hours of attempts I could not figure this out. I cannot understand how 
we are supposed to correctly swap a value using lambda calculus. I understand that there has to be a way to
order the problem so the proper swaps take place but I could not figure it out with this 
haskell implementation. I tried to create a pair from the passed parameters and also tried to use 
a similar set up as the pair with three abstractions to replace the body with the proper elements when
substituted but it did not work. 
-}
setSnd :: Exp
setSnd = abs2 (app2 (app2 (pair) (Ref 0) (Ref 1)) (App snd (Ref 1)) (Ref 0))


--
-- * Part 2: Church encoding a Haskell program
--

-- | Pretend Haskell's Int is restricted to Nats.
type Nat = Int

-- | A simple data type with three cases.
data Foo = N Nat | B Bool | P Nat Bool
  deriving (Eq,Show)

-- | Compute a numeric value from a Foo.
--   (This is just an arbitrary function.)
bar :: Foo -> Nat
bar (N n)     = n * 3
bar (B True)  = 1
bar (B False) = 0
bar (P n b)   = n + if b then 1 else 0

-- | 3. Write a Haskell function that converts a Foo into a
--   lambda calculus term.
{-
Description: After trying this problem for awhile I could not figure out
the correct incoding using lambda terms that were defined in this module.
I know that we need to pattern match on each data type as defined by Foo but the
trouble is in how to use the lambda abstractions. 
-}
encodeFoo :: Foo -> Exp
encodeFoo = undefined

-- encodeFoo N a = \a -> 
-- encodeFoo B b
-- encodeFoo 

-- | 4. Implement the bar function as a lambda calculus term.
{-
Description: This problem stumped me. I was having a difficult time using the 
lambda abstractions in this assignment and could not figure out how to use it in
the context of this problem.
-}
barExp :: Exp
barExp = undefined

-- | Run your lambda-encoded bar function on a lambda-encoded Foo.
runBar :: Foo -> Exp
runBar x = eval (App barExp (encodeFoo x))

-- | A function for testing encodeFoo and barExp. Checks to see if the lambda
--   calculus encoding returns the same number as the given value function.
--
--   >>> test (N 4)
--   True
--
--   >>> test (B True)
--   True
--
--   >>> test (B False)
--   True
--
--   >>> test (P 5 True)
--   True
--
--   >>> test (P 5 False)
--   True
--
test :: Foo -> Bool
test x = num (bar x) == eval (App barExp (encodeFoo x))
