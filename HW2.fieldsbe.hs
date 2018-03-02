{-
	***************HW2******************
	Description:Program that implements many functions for use with
	self defined natural numbers
	Author: Benjamin Fields
-}

module HW2 where

import Prelude hiding (Enum(..), sum)


--
-- * Natural numbers
--

-- | The natural numbers.
data Nat = Zero
         | Succ Nat
         deriving (Eq,Show)

-- | The number 1.
one :: Nat
one = Succ Zero

-- | The number 2.
two :: Nat
two = Succ one

-- | The number 3.
three :: Nat
three = Succ two

-- | The number 4.
four :: Nat
four = Succ three


-- | The predecessor of a natural number.
--   
--   >>> pred Zero
--   Zero
--   
--   >>> pred three
--   Succ (Succ Zero)
--   

{-
	***************HW2******************
	Description: This function returns predecessor
	of a natural number in the format as defined 
	within this program. This function works because 
	of our recursive definitions above. To get the previous
	value we just simply return the value which by definition
	will recursively provide our value to us.
-}
pred::Nat->Nat
pred Zero = Zero
pred (Succ a) = a


-- | True if the given value is zero.
--
--   >>> isZero Zero
--   True
--
--   >>> isZero two
--   False
--

{-
	***************HW2******************
	Description: this function is quite simple when
	pattern matching is used. We only want to return 
	true when zero is provided so if we get Zero as the
	argument we return true and if any other argument is
	provided then we return false.
-}
isZero::Nat->Bool
isZero Zero = True
isZero _ = False


-- | Convert a natural number to an integer. NOTE: We use this function in
--   tests, but you should not use it in your other definitions!
--
--   >>> toInt Zero
--   0
--
--   >>> toInt three
--   3
--

{-
	***************HW2******************
	Description: this function takes uses our recursive defintion
	to continually add one every time we are able to recurse to a 
	lower level thus meaning there is another value. Each time one is added 
	untill we reach zero and zero is provided. The values can then be summed up
	and returned to provide the integer value.
-}
toInt :: Nat -> Int
toInt Zero = 0
toInt a    = 1 + toInt(pred a)


-- | Add two natural numbers.
--
--   >>> add one two
--   Succ (Succ (Succ Zero))
--
--   >>> add Zero one == one
--   True
--
--   >>> add two two == four
--   True
--
--   >>> add two three == add three two
--   True
--  

{-
	***************HW2******************
	Description: for this function we have two easy cases that pattern
	match when there is a zero which just returns the other value. The 
	last case is concerned when we actully have two values. The last case 
	is a bit harder but the function takes two the first value and addes a 
	succ onto it while adding with the b value from the second argument. This
	strips that layer of Succ and then passes it back for pattern matching. 
	The other value is accumulating a succ for every level of recursion 
	thus representing the summed value. Eventually the right side is Zero and 
	the left side is returned.
-} 
add::Nat->Nat->Nat
add Zero a = a
add a Zero = a
add (Succ a) (Succ b) = add (Succ(Succ a)) b


-- | Subtract the second natural number from the first. Return zero
--   if the second number is bigger.
--
--   >>> sub two one
--   Succ Zero
--   
--   >>> sub three one
--   Succ (Succ Zero)
--
--   >>> sub one one
--   Zero
--
--   >>> sub one three
--   Zero
--

{-
	***************HW2******************
	Description: This function uses a similar approach as previously seen
	in that it pattern matches first for the simple cases and then uses
	a recursive call to continuasly strip a succ off of the left value which 
	represents a subtraction of one. Eventually the zero is reached and the final
	value is returned. It also checks to determine if the value is greater and if
	so we simply return zero as we are not accounting for negative values.
-}
sub :: Nat -> Nat -> Nat
sub Zero a = Zero
sub a Zero = a
sub (Succ a) (Succ b) = if(gt b a) then Zero else sub a b



-- | Is the left value greater than the right?
--
--   >>> gt one two
--   False
--
--   >>> gt two one
--   True
--
--   >>> gt two two
--   False
--

{-
	***************HW2******************
	Description: For this function we can leverage the functions previously
	defined to determine if the value is greater than. If it is greater than
	the value then the subtraction will return a positive value but if is it 
	not greater the value will be zero and thus we know that it is in fact not
	greater.
-}
gt :: Nat -> Nat -> Bool
gt a b  | sub a b == Zero = False
        | otherwise = True


-- | Multiply two natural numbers.
--
--   >>> mult two Zero
--   Zero
--
--   >>> mult Zero three
--   Zero
--
--   >>> toInt (mult two three)
--   6
--
--   >>> toInt (mult three three)
--   9
--

{-
	***************HW2******************
	Description: This function uses a recursive add which continues 
	to sum and stripping the Succ to eventually stop the recursion and
	just return the summed value that was the result of summing x times.
	The first two cases are just pattern matching for the simple cases.
-}
mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult _ Zero = Zero
mult (Succ x) y = add y (mult x y)


-- | Compute the sum of a list of natural numbers.
--
--   >>> sum []
--   Zero
--   
--   >>> sum [one,Zero,two]
--   Succ (Succ (Succ Zero))
--
--   >>> toInt (sum [one,two,three])
--   6
--

{-
	***************HW2******************
	Description: This function is rather easy as we have defined it
	in exercises and the same principles apply. While we still have a list 
	recursively call the previously defined sum function and pass the rest 
	of the list to be summed eventually reaching the end (empty list) which returns
	an empty list ending the recursion.
-}
sum :: [Nat] -> Nat
sum [] = Zero
sum (x:xs) = add x (sum xs)


-- | An infinite list of all of the *odd* natural numbers, in order.
--
--   >>> map toInt (take 5 odds)
--   [1,3,5,7,9]
--
--   >>> toInt (sum (take 100 odds))
--   10000
--

{-
	***************HW2******************
	Description:For this function we need to compsose a list of natural odd
	numbers starting with the value 1 which is succ(zero). We then append 
	to this list the values obtained by mapping a composed function that 
	is the successor of the successor, which is a way to 
	describe the value after the next value. Since we are starting on the 
	value one and continually skipping the next value we will always get 
	the odd numbers in the list thus creating our list of odd natural numbers.
-}
odds::[Nat]
odds = Succ(Zero) : map (Succ . Succ) odds
