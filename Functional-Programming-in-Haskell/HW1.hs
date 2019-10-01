-- CptS 355 - Fall 2019 Assignment 1
-- Please include your name and the names of the students with whom you discussed any of the problems in this homework

module HW1
     where

import Data.Char (toUpper)

-- 1. exists
exists :: Eq t => t -> [t] -> Bool
exists a [] = False
exists a (x:xs)
          | a == x = True
          | otherwise = exists a xs


-- 2. listUnion
listUnion :: Eq a => [a] -> [a] -> [a]
listUnion [] [] = []
listUnion x y = let 
                 z = (x ++ y)
                 distinctList = []
                 rmdup [] distinctList = []
                 rmdup (z:zs) distinctList = if (exists z distinctList) then (rmdup zs distinctList)  --if z exists still in the list, dont add it to the return yet
                                                                      else z : (rmdup zs (distinctList ++ [z]))
                 in  rmdup z []


-- 3. replace
replace :: (Eq t1, Num t1) => t1 -> t2 -> [t2] -> [t2]
replace index newValue [] = []
replace 0 newValue (x:xs) = newValue:xs
replace index newValue (x:xs)  = [x] ++ (replace (index -1) newValue xs)


-- 4. prereqFor
prereqFor :: Eq t => [(a, [t])] -> t -> [a]

prereqFor [] prereq = []

prereqFor (x:xs) prereq | exists prereq (snd (x)) = [(fst (x))] ++ prereqFor xs prereq
                        | otherwise = prereqFor xs prereq




-- 5. isPalindrome
isPalindrome :: [Char] -> Bool

isPalindrome [] = True
isPalindrome x = let

                  -- Helper function
                  upperCase [] = []
                  upperCase (y:ys) = (toUpper y):upperCase ys
 
                  upperCaseList = upperCase x  -- must change the list to uppercase for comparisons
                  
                  -- Helper function
                  checkPalindrome :: [Char] -> Bool
                  checkPalindrome a = a == a

                  in checkPalindrome upperCaseList



-- 6. groupSumtoN

groupSumtoN :: (Ord a, Num a) => a -> [a] -> [[a]]

groupSumtoN 0 list = [[]]
groupSumtoN num [] = [[]]

groupSumtoN n list = let 
                      sum [] = 0
                      sum (x:xs) = x + sum xs

                     
                      --variables
                      returnList = []
                      acc = []
                      i = list 

                      helper n [] acc returnList = [reverse acc] ++ returnList --base case
                      helper n (i:is) acc returnList | ((sum acc) + i) > n = (reverse acc):(helper n is [i] returnList) 
                                                     | otherwise = (helper n is (i:acc) returnList)
                      
                      
                      in helper n list acc returnList 

