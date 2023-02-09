-- Examples from Week 3 Lecture Videos

-- Polymorphism --
lengthString:: String -> Int
lengthString [] = 0
lengthString (x:xs) = 1 + lengthString xs

lengthInt:: [Int] -> Int
lengthInt [] = 0
lengthInt (x:xs) = 1 + lengthInt xs

lengthFloat:: [Float] -> Int
lengthFloat [] = 0
lengthFloat (x:xs) = 1 + lengthFloat xs

length1::[a] -> Int
length1 [] = 0
length1 (x:xs) = 1 + (length1 xs)

countChar :: Eq a => a -> [a] -> Int
countChar _ [] =0
countChar c (x:xs)  | (x == c) = 1 + countChar c xs
                    | otherwise = countChar c xs

butLast:: [a] -> a
butLast [x,y] = x
butLast (x:xs) = butLast xs

myRev:: [a] -> [a]
myRev [] = []
myRev (x:xs) = (myRev xs) ++ [x]

isPalin:: Eq a => [a] -> Bool
isPalin xs = xs == (myRev xs)

flatten:: [[a]] -> [a]
flatten [] = []
flatten (xs:xss) = xs ++ flatten xss

-- Type Classes --

equals:: Eq a => a -> a -> Bool
equals x y = x == y

add:: Num a => a -> a -> a
add x y = x + y

greaterThan:: Ord a => a -> a -> Bool
greaterThan x y = x > y

generateList:: Enum a => a -> a -> [a]
generateList x y = [x..y]
