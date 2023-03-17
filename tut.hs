import Data.List

{- Missing topics to be written:
	-> List Comprehension
	-> More on let and while
	-> Folds
	-> Dollar implementation
	-> Composite Functions
	-> Point-Free Functions
-}

--Pattern Matching

lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER RECEIVED!"
lucky x = "Sorry!"

--Recursive Factorial

factorial :: (Integral a) => a -> a 
factorial 0 = 1
factorial n = n * factorial (n-1)

--List Pattern Matching

head' :: [a] -> a
head' [] = error "Not used on empty lists you dumb sh*t!"
head' (x:_) = x

--Own length function in recursion

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

--Sum implementation

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

--Guards implementation

max' :: (Ord a) => a -> a-> a
max' a b
    | a > b = a
    | otherwise = b

{-Guards vs Patterns is that pattern check that input form, whereas guards work depending on a true or false returned by the expression in the code -}

compare' :: (Ord a) => a -> a -> Ordering
compare' a b
    | a > b = LT
    | a == b = EQ
    | otherwise = GT

--Where can be used to reduce repetitions in a block of code

bmiTeller :: (RealFloat a) => a -> a -> String
bmiTeller weight height
    | bmi <= 18.5 = "Underweight!"
    | bmi <= 25.0 = "Normal"
    | bmi <= 30.0 = "Fat!"
    | otherwise = "You are a whale! congratulations!"
    where bmi = weight / height ^ 2

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f : _) = firstname
          (l : _) = lastname

calcBmis :: ( RealFloat a ) => [(a , a )] -> [a]
calcBmis xs = [ bmi w h | (w , h) <- xs ]
    where bmi weight height = weight / height ^ 2

--Using let bindings

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h = 
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^2
    in sideArea + 2 * topArea

--Elegant QuickSort algorithm in Haskell

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted

--Recursion in Haskell!
--Function to produce the maximum element in a List

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Not on an empty list you fool!"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

--Recursive solution for replicate function implementation

replicate' :: Int -> a -> [a]
replicate' n x
    | n <= 0 = []
    | otherwise = x : replicate' (n-1) x

--Recursive solution for take function implementation

take' :: Int -> [a] -> [a]
take' n _
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

--Recursive solution of reversing a list

--Yay! my first self thought recursion in haskell!

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

--Recursive solution for zip function implementation

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y):zip' xs ys

--recursive implementation of elem function

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs)
    | a == x = True
    | otherwise = elem' a xs



-----------HIGHER ORDER FUNCTIONS - SHIT'S GONNA GET MORE REAL!--------

-- Higher order functions usually take functions as parameters and or or return functions as return types

--Every Haskell function only takes one parameter. 

--Whenever there are more than one parameter involved, curried functions are called to accomodate multiple parameters

{-----------------
Partially applied functions:

	Whenever a function in which more than one parameters are needed, the main funtion first takes only the first input and then creates a partial function that again takes the next parameter and does the function and then sends the new partial fraction until all the parameters are used and a useful answer is generated by the function.

--------------------}

--Example of higher order functions and partial function

compareWithHundred :: Int -> Ordering
compareWithHundred = compare 100 

-- In the above example, compare 100 will take 100 and produce a partial fraction that again takes an integer and compares with 100 and returns the valid ordering. 

--Example of sectioning a regular function

divideByTen :: (Floating a) => a -> a
divideByTen = (/10) 

--More on Partial functions:

foo :: (Num a) => a -> a -> a
foo x y = x * y

-- If i now define a new function called foobar like follows:

foobar = foo 5

-- It means that it is a partial function of foo, and a single parameter can be passed to foobar that will multiply with 5 and produce a result

--Applications of partial functions in infix functions format using sections

--sectioning is done by bounding the function in parenthesis where an operand is missing and when we supply the operand at the side by a space, the compiler adds this operand in the missing place and produces the desired result.

divide :: (Floating a) => a -> a -> a
divide = (/)

--Now we have seen a function getting returned, now let us see a function going in!

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

--Lets make a higher order programming implementation of standard zipWith Function

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

--Collatz Sequences:

chain :: Integer -> [Integer]
chain 1 = [1]
chain n
    | even n = n:chain(div n 2)
    | odd n = n:chain(n*3 + 1)

--Lambda expressions: indicated in parenthesis by \

-- map (+3) [1..100]
-- map (\x -> x + 3) [1..100]

--The above both functions are similar


addThree :: Int -> Int -> Int -> Int
addThree = \x -> \y -> \z -> x + y + z

sum'' :: (Num a) => [a] -> a
sum'' xs = foldl (+) 0 xs

--sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

--Different kinds of fibonacci production code

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

list_till_number :: Int -> [Int]
list_till_number 0 = []
list_till_number x = list_till_number (x-1) ++ [x]

fib_series :: Int -> [Int]
fib_series x = map fibonacci (list_till_number x)

--foldl function implementation:

sum_fold :: (Num a) => [a] -> a
sum_fold xs = foldl (+) 0 xs

-- Compositions of Functions in haskell

size_nub :: (Eq a) => [a] -> Int
size_nub xs = length (nub xs)
 
