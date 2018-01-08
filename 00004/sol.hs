import Text.Printf

split :: Int -> [Int]
split 0 = []
split x = (split (div x 10)) ++ [mod x 10]

isPalindrome :: Int -> Bool
isPalindrome x = (a == reverse a)
  where a = split x

largestPalidrome :: Int -> Int
largestPalidrome 0 = -1
largestPalidrome x
  | (isPalindrome x) = x
  | otherwise        = (largestPalidrome (x - 1))

factorLst :: Int -> Int -> [[Int]]
factorLst x y
  | (y * y > x)      = []
  | ((mod x y) == 0) = [[y, (div x y)]] ++ (factorLst x (y + 1))
  | otherwise        = factorLst x (y + 1)

factorize :: Int -> [[Int]]
factorize x = (factorLst x 1)

factorSize :: [[Int]] -> [[Int]]
factorSize [] = []
factorSize x = [[length a, length b]] ++ factorSize q
  where p = head x
        q = tail x
        a = split (p !! 0)
        b = split (p !! 1)

isProdSize :: Int -> Int -> Bool
isProdSize x y = (elem [y, y] a)
  where a = (factorSize (factorize x))

largestPalidromeProduct :: Int -> Int -> Int
largestPalidromeProduct x y
  | (isProdSize x y) = x
  | otherwise        = largestPalidromeProduct (largestPalidrome (x - 1)) y

result = largestPalidromeProduct 1000000 3

main = do
  printf "Result: %d\n" (result::Int)
