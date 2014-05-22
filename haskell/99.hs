
-- 1
-- find last

mylast :: [a] -> a
mylast [x] = x
mylast (x:xs) = last xs


-- 2
-- find second to last
--

myButLast :: [a] -> a
myButLast [x, y] = x
myButLast (x:xs) = myButLast xs

-- 3
-- find kth elem, index starting at 1

elementAt :: [a] -> Int -> a
elementAt x n = last (take n x)

-- 4
-- find length of list
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs


-- 5
-- reverse a list
myReverse :: [a] -> [a]
myReverse [] = []
myReverse [x] = [x]
myReverse (x:xs) = myReverse xs ++ [x]

-- 6
-- find out whether a list is a palindrome
palindromeP :: (Eq a) => [a] -> Bool
palindromeP [] = True
palindromeP [x] = True
palindromeP [x, y] = if x == y then True else False
palindromeP (x:xs) = if x /= last xs then False
    else palindromeP (init xs)

-- 7
-- flatten a nested list structure
-- requires a new data type because Haskell lists are homogeneous
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List [x]) = flatten x
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

-- 8
-- Eliminate consecutive duplicates of a list
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:xs) = if x == head xs then compress (x:tail xs) else x:(compress xs)


-- 9
-- Pack consecutive duplicates of list elements into sublists.  Separate sublists for unequal elements
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x:xs) = if x /= head xs then [x]:r else (x:head r):tail r
    where r = pack xs

-- 10
-- Run-length encoding of a list.  Use solution from #9.
-- Encoding is [(N1 E2), (N2 E2), ...] where N is the number of the element, then E is the element
encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = []
encode x = enc (pack x) where
    enc :: (Eq a) => [[a]] -> [(Int, a)]
    enc [x] = [(length x, head x)]
    enc (x:xs) = (length x, head x):enc xs
-- alternate, shorter version of 10
encode2 :: (Eq a) => [a] -> [(Int, a)]
encode2 xs = map (\ x -> (length x, head x)) (pack xs)

-- 11
-- Modified run-length encoding - if length is 1, put the element in the list directly
-- This one needs a data type to get around haskell's homogeneous lists
data RlElem a = Single a | Multiple Int a deriving (Show)
encodeModified :: (Eq a) => [a] -> [RlElem a]
encodeModified [] = []
encodeModified xs = map transform (encode2 xs) where
    transform (n, elem) = if n == 1 then Single elem else Multiple n elem

