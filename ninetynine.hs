-- Problem 1: Find the last Element of a list:

myLast :: [a] -> a
myLast [x] = x
myLast (x:xs) = myLast xs
