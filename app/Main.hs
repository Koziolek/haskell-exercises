module Main where

import Data.Char (ord, chr)

main :: IO ()
main = do
  putStrLn "Hello World!"
  putStrLn "bye"

max' :: Ord a => a -> a -> a
max' a b = if a > b
            then a
            else b

maximum' :: Ord a => [a] -> a
maximum' []     = error "why?"
maximum' [x]    = x
maximum' (x:xs) = do
                  let mxs = maximum' xs
                  max' x mxs

maximumTotal :: Ord a => [a] -> Maybe a
maximumTotal []     = Nothing
maximumTotal [x]    = Just x
maximumTotal (x:xs)
  | Just x > tailMax = Just x
  | otherwise = tailMax
  where tailMax = maximumTotal xs

data Tree = Leaf
    | Node Tree Tree
    deriving (Show, Eq)

longestBranch :: Tree -> Int
longestBranch Leaf         = 0
longestBranch (Node b1 b2) = 1 + max (longestBranch b1) (longestBranch b2)

shortestBranch :: Tree -> Int
shortestBranch Leaf         = 0
shortestBranch (Node b1 b2) = 1 + min (longestBranch b1) (longestBranch b2)

lsBranch :: (Int -> Int -> Int) -> Tree -> Int
lsBranch _ Leaf         = 0
lsBranch f (Node b1 b2) = 1 + f (lsBranch f b1) (lsBranch f b2)

-- how many pattern matches to use?
isSubtree :: Tree -> Tree -> Bool
isSubtree Leaf Leaf = True
isSubtree (Node _ _) Leaf = False
-- @ alias lokalny
isSubtree t1 t2@(Node b1 b2) = t1 == t2 || isSubtree t1 b1 || isSubtree t1 b2


factorial :: Integer -> Integer
factorial n = undefined

isPalindrome :: String -> Bool
isPalindrome s = undefined

isPalindromeInt :: Int -> Bool
isPalindromeInt n = undefined

-- try pointfree notation
sumOdd :: [Int] -> Int
sumOdd = undefined

-- use ord, chr, `div`, `mod`
rot13 :: String -> String
rot13 = undefined

-- use list comprehension
rightTriangles :: Int -> [(Int, Int, Int)]
rightTriangles n = undefined

safeHead :: [a] -> Maybe a
safeHead _ = undefined

isHeadEq5 :: [Int] -> Maybe Bool
isHeadEq5 l =
  case safeHead l of
    Just 5 -> Just True
    Just _ -> Just False
    _      -> Nothing

-- write isHeadEq5 using monadic Maybe
isHeadEq5M :: [Int] -> Maybe Bool
isHeadEq5M l = do
  return undefined
