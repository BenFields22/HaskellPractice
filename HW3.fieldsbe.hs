{-
Program: HW3.fieldsbe.hs
Author: Benjamin Fields
Date: 10-16-2017
Description: This program implements more functions for a modified binary tree 
data structure. The first function is valueAt, which provides the value at a given
point in the tree per a provided traversal path. The second function is pathTo, 
which given a value to find returns the path to that value in the tree if it exists.
The third function is mapTree which takes a function and maps it to every value in 
tree providing you with a new tree with updated values.
-}

module HW3 where
import Control.Applicative

-- | One step in a path, indicating whether to follow the left subtree (L)
--   or the right subtree (R).
data Step = L | R
  deriving (Eq,Show)


-- | A path is just a sequence of steps. Each node in a binary tree can be
--   identified by a different path, indicating how to walk down the tree
--   starting from the root. See the examples for `valueAt`.
type Path = [Step]


-- | Binary trees with nodes labeled by an arbitrary type.
data Tree a = Node a (Tree a) (Tree a)
            | End
  deriving (Eq,Show)


-- | Create a leaf node.
leaf :: a -> Tree a
leaf x = Node x End End


-- | An example tree.
ex :: Tree Int
ex = Node 4 (Node 3 (leaf 2) End)
            (Node 7 (Node 5 End (leaf 6))
                    (leaf 8))


-- | Get the value at the node specified by a path.
--
--   >>> valueAt [] ex
--   Just 4
--
--   >>> valueAt [L,L] ex
--   Just 2
--
--   >>> valueAt [L,R] ex
--   Nothing
--
--   >>> valueAt [R,L,R] ex
--   Just 6
--
--   >>> valueAt [L,L,L] ex
--   Nothing
--

{-
	**************HW3*******************
	Description: This is a helper function that allows us to move to 
	either the L or R subtree provided from the current first position
	in the provided path. Thus if the current first slot is L the function
	will return the left subtree for further processing. This process will
	continue until the path is complete. 
-}
moveTo::Step->Tree a-> Tree a
moveTo L (Node a b c) = b
moveTo R (Node a b c) = c

{-
	**************HW3*******************
	Description: This function will take in a Path, which is a list
	of steps and continually traverse the tree based on the path until 
	the path has been complete. Once the path has reached the end the value
	of the current node is returned with a cast to Maybe a. This function utilizes
	a helper function to traverse the correct subtree provided in the current move slot.
-}
valueAt :: Path -> Tree a -> Maybe a
valueAt [] End = Nothing
valueAt [] (Node a _ _ ) = Just a
valueAt (x:[]) (Node a b c) = valueAt [] (moveTo x (Node a b c))
valueAt (x:xs) (Node a b c) = valueAt xs (moveTo x (Node a b c))





-- | Find a path to a node that contains the given value.
--
--   >>> pathTo 3 (leaf 5)
--   Nothing
--
--   >>> pathTo 5 ex
--   Just [R,L]
--
--   >>> pathTo 6 ex
--   Just [R,L,R]
--
--   >>> pathTo 4 ex
--   Just []
--
--   >>> pathTo 10 ex
--   Nothing
--

{-
	**************HW3*******************
	Description: This function was the most difficult for me to implement
	of the three on this assignment. The problem I faced was with how to deal
	with the binary nature of the problem. After doing some reading I found out
	about control applicatives that allow you to contruct function logic for this
	type of problem. The main class that was so useful was the alternative class that
	allows you to take a binary function with potentially two returned values and return
	only one as a result of one or the other being an "empty" result. The result of our
	function is expecting a path so by continually calling the binary function we either
	get a value of nothing returned or a path. The nothing that is returned when not found
	is discarded and the path is taken. To construct our path we are mapping the list 
	contructor with L and R for each left and right traversal of the tree node. As mentioned
	above the nothings will iteratively be discarded thus returning the path to the value
	when it has been found. The list of steps is then casted to the Maybe Path type to 
	present the final results. If both branches return nothing then we know the value does not
	exist and we return nothing per the cast to the Maybe type. 
-}
pathTo :: Eq a => a -> Tree a -> Maybe Path
pathTo val End = Nothing
pathTo val (Node x l r)
    | val == x = Just []
    | otherwise = (fmap (L:) (pathTo val l)) <|> (fmap (R:) (pathTo val r))


-- | Apply a function to the value at every node in the tree.
--   
--   >>> mapTree odd End
--   End
--
--   >>> mapTree even (Node 5 (leaf 2) (leaf 3))
--   Node False (Node True End End) (Node False End End)
--
--   >>> (mapTree not . mapTree even) (Node 5 (leaf 2) (leaf 3))
--   Node True (Node False End End) (Node True End End)
--
--   >>> mapTree (+10) ex
--   Node 14 (Node 13 (Node 12 End End) End) (Node 17 (Node 15 End (Node 16 End End)) (Node 18 End End))
--
--   >>> ex == (mapTree (subtract 27) . mapTree (+27)) ex
--   True
--

{-
	**************HW3*******************
	Description: This function allows a function to be mapped to every value
	in the binary tree. This implementation follows a pattern matching approach
	and takes in a function to which it is applied to a tree with one type and
	returns the tree with the return type of the function. This function lists
	each of the possible argument cases and returns the equivalent tree with the
	function applied at that positions value. 
-}
mapTree:: (a->b) -> Tree a -> Tree b
mapTree f  End = End
mapTree f (Node x End End) = (leaf (f x))
mapTree f (Node x End r) = Node (f x) End (mapTree f r)
mapTree f (Node x l End) = Node (f x) (mapTree f l) End
mapTree f (Node x l r) =  Node (f x) (mapTree f l) (mapTree f r) 
