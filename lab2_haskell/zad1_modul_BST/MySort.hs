module MySort where 
import qualified BST as BST
import qualified Tree as T

sort :: (Ord a) => [a] -> [a]
sort [] = []
sort l = T.toList (BST.fromList l)