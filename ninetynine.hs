-- Problem 1: Find the last Element of a list:

myLast :: [a] -> a
myLast [x] = x
myLast (x:xs) = myLast xs


-- Problem 2: Find all but the last element of a list:

myButLast :: [a] -> [a]
myButLast [x] = []
myButLast (x:xs) = x : myButLast xs


-- Problem 3: Find the kth element of a list:

elementAt :: [a] -> Int -> a
elementAt (x:_) 1 = x
elementAt (_:xs) n = elementAt xs (n-1)


-- Problem 4: Find the number of elements in a list:

len :: [a] -> Int
len [] = 0
len [a] = 1
len (a:as) = 1 + len as


-- Problem 5: Reverse a list:

rev :: [a] -> [a]
rev [] = []
rev (x:xs) = (rev xs) ++ [x]


-- Problem 6: Find out whether a list is a palindrome:

palindrome [] = True
palindrome [_] = True
palindrome xs = (head xs) == (last xs) && (palindrome $ init $ tail xs)
