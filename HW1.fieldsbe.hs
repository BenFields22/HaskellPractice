module HW1 where


--
-- * Part 1: Binary trees
--

-- | Integer-labeled binary trees.
data Tree = Node Int Tree Tree   -- ^ Internal nodes
          | Leaf Int             -- ^ Leaf nodes
  deriving (Eq,Show)


-- | An example binary tree, which will be used in tests.
t1 :: Tree
t1 = Node 1 (Node 2 (Node 3 (Leaf 4) (Leaf 5))
                    (Leaf 6))
            (Node 7 (Leaf 8) (Leaf 9))

-- | Another example binary tree, used in tests.
t2 :: Tree
t2 = Node 6 (Node 2 (Leaf 1) (Node 4 (Leaf 3) (Leaf 5)))
            (Node 8 (Leaf 7) (Leaf 9))


-- | The integer at the left-most node of a binary tree.
--
--   >>> leftmost (Leaf 3)
--   3
--
--   >>> leftmost (Node 5 (Leaf 6) (Leaf 7))
--   6
--   
--   >>> leftmost t1
--   4
--
--   >>> leftmost t2
--   1
--

-- This function was already provided for us 
leftmost :: Tree -> Int
leftmost (Leaf i)     = i
leftmost (Node _ l _) = leftmost l


-- | The integer at the right-most node of a binary tree.
--
--   >>> rightmost (Leaf 3)
--   3
--
--   >>> rightmost (Node 5 (Leaf 6) (Leaf 7))
--   7
--   
--   >>> rightmost t1
--   9
--
--   >>> rightmost t2
--   9
--


{-
	**************HW1*******************
	Description: THis function continues to call
	down the right node until the leaf is reached
	indicating we are at the rightmost position. The
	value of the last leaf is then returned.
-}


rightmost :: Tree -> Int
rightmost (Leaf i)     = i
rightmost (Node _ _ l) = rightmost l


-- | Get the maximum integer from a binary tree.
--
--   >>> maxInt (Leaf 3)
--   3
--
--   >>> maxInt (Node 5 (Leaf 4) (Leaf 2))
--   5
--
--   >>> maxInt (Node 5 (Leaf 7) (Leaf 2))
--   7
--
--   >>> maxInt t1
--   9
--
--   >>> maxInt t2
--   9
--

{-
	**************HW1*******************
	Description: This function recursively compares the 
	value of the current node to the value of the children
	nodes using the max function. In the end the highest value
	is returned.
-}
maxInt :: Tree -> Int
maxInt (Leaf a) = a
maxInt (Node a l r) = max a (max (maxInt l) (maxInt r))

 


-- | Get the minimum integer from a binary tree.
--
--   >>> minInt (Leaf 3)
--   3
--
--   >>> minInt (Node 2 (Leaf 5) (Leaf 4))
--   2
--
--   >>> minInt (Node 5 (Leaf 4) (Leaf 7))
--   4
--
--   >>> minInt t1
--   1
--
--   >>> minInt t2
--   1
--

{-
	**************HW1*******************
	Description: This function takes a similar approach as the previous
	function but uses the min function to recursively return the smallest
	value found at each position.
-}
minInt :: Tree -> Int
minInt (Leaf a) = a
minInt (Node a l r) = min a (min (minInt l) (minInt r))

-- | Get the sum of the integers in a binary tree.
--
--   >>> sumInts (Leaf 3)
--   3
--
--   >>> sumInts (Node 2 (Leaf 5) (Leaf 4))
--   11
--
--   >>> sumInts t1
--   45
--
--   >>> sumInts (Node 10 t1 t2)
--   100
--

{-
	**************HW1*******************
	Description: This function takes the current value
	and recursively adds the values of the children.
-}
sumInts:: Tree -> Int
sumInts (Leaf a) = a
sumInts (Node a l r) = a + (sumInts l) + (sumInts r)


-- | The list of integers encountered by a pre-order traversal of the tree.
--
--   >>> preorder (Leaf 3)
--   [3]
--
--   >>> preorder (Node 5 (Leaf 6) (Leaf 7))
--   [5,6,7]
--
--   >>> preorder t1
--   [1,2,3,4,5,6,7,8,9]
--
--   >>> preorder t2
--   [6,2,1,4,3,5,8,7,9]
--   

{-
	**************HW1*******************
	Description: In order to get a preorder list
	we simple add the value to the list first then 
	recursively call down to the children. This provides the
	preorder list we need.
-}
preorder:: Tree -> [Int]
preorder (Leaf a) = [a]
preorder (Node a l r) = [a] ++ (preorder l) ++ (preorder r)


-- | The list of integers encountered by an in-order traversal of the tree.
--
--   >>> inorder (Leaf 3)
--   [3]
--
--   >>> inorder (Node 5 (Leaf 6) (Leaf 7))
--   [6,5,7]
--
--   >>> inorder t1
--   [4,3,5,2,6,1,8,7,9]
--
--   >>> inorder t2
--   [1,2,3,4,5,6,7,8,9]
--   

{-
	**************HW1*******************
	Description: This function uses an in order traversal
	which requires that we print the value after the recursive call.
	Once the recursion stops after travelling all the way left the value is
	returned after which the parent node value is returned and then the 
	right node is recursively traversed to follow a similar pattern.
-}
inorder:: Tree -> [Int]
inorder (Leaf a) = [a]
inorder (Node a l r) = (inorder l) ++ [a] ++ (inorder r)


-- | Check whether a binary tree is a binary search tree.
--
--   >>> isBST (Leaf 3)
--   True
--
--   >>> isBST (Node 5 (Leaf 6) (Leaf 7))
--   False
--   
--   >>> isBST t1
--   False
--
--   >>> isBST t2
--   True
--   

{-
	**************HW1*******************
	Description: For this function I was able to leverage the properties of
	a binary search tree and the inorder traversal to prove whether the tree
	is in fact a bst. If the tree is a BST than an in order traversal will
	follow an increasing pattern and no left value should be greater than the 
	right value...in the in order list that is... using this we can test for
	a correct BST.
-}
binOrder::[Int]->Bool
binOrder [] = True
binOrder (x:[]) = True
binOrder (x:(y:[])) = (x <= y )
binOrder (x:(y:xs)) =  ((x <= y) && (binOrder xs)) 

isBST:: Tree -> Bool
isBST (Leaf a) = True
isBST (Node a l r) = binOrder (inorder (Node a l r))



-- | Check whether a number is contained in a binary search tree.
--   (You may assume that the given tree is a binary search tree.)
--
--   >>> inBST 2 (Node 5 (Leaf 2) (Leaf 7))
--   True
--
--   >>> inBST 3 (Node 5 (Leaf 2) (Leaf 7))
--   False
--
--   >>> inBST 4 t2
--   True
--
--   >>> inBST 10 t2
--   False
--   

{-
	**************HW1*******************
	Description: For this function we check if the current node has the value
	and if not then recursively check the children ORing the results together
	as we are checking to see if it is present and one TRUE in a chain of OR
	bool checks will confirm the presence of the value.
-}
inBST:: Int -> Tree -> Bool
inBST v (Leaf a) = (v == a) 
inBST v (Node a l r) = if(v==a) then True else inBST v l || inBST v r

--
-- * Part 2: Run-length lists
--


-- | Convert a regular list into a run-length list.
--
--   >>> compress [1,1,1,2,3,3,3,1,2,2,2,2]
--   [(3,1),(1,2),(3,3),(1,1),(4,2)]
-- 
--   >>> compress "Mississippi"
--   [(1,'M'),(1,'i'),(2,'s'),(1,'i'),(2,'s'),(1,'i'),(2,'p'),(1,'i')]
--

{-
	**************HW1*******************
	Description: For this function we are initializing each check of a 
	value with the value 1. This value is passed into the pattern matching
	section to check the structure of the remaining list. If there is no longer
	anymore to the string then we have reached the end and return the tuple
	of the current value. If there is more to the list then we need to check
	if the value is still the same. If the value is the same then we recurse
	the rest of the string and increment the counter. If the value is not
	the same then we create the current tuple and concatenate it with the 
	results of the tuples as created from the rest of the list. 
-}
compress :: Eq a => [a] -> [(Int,a)]
compress []     = []
compress (x:xs) = compress 1 x xs where
    compress n x [] = [(n, x)]
    compress n x (y:ys)
        | x == y    = compress (n + 1) x ys
        | otherwise = (n, x) : compress 1 y ys


-- | Convert a run-length list back into a regular list.
--
--   >>> decompress [(5,'a'),(3,'b'),(4,'c'),(1,'a'),(2,'b')]
--   "aaaaabbbccccabb"
--  

{-
	**************HW1*******************
	Description: This function uses a helper function to continually add 
	the current value to the string based on the current tuple by decrementing
	the value and eventually reaching 0. Once one state is finished the next 
	tuple is processed and so on.
-}

iterChars :: (Int,a) -> [a]
iterChars (0,a) = []
iterChars (b,a) = [a] ++ iterChars (b-1,a)


decompress :: [(Int,a)] -> [a]
decompress [] = []
decompress ((b,a):[])= iterChars (b,a)
decompress ((b,a):xs) = iterChars(b,a) ++ (decompress xs)

