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
