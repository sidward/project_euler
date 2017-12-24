import Data.List
import Text.Printf

fib :: Int -> Int
fibInfLst :: [Int]
fib 0 = 1
fib 1 = 1
fib x = (fibInfLst !! (x - 1) + fibInfLst !! (x - 2))
fibInfLst = [(fib x) | x <- [0, 1 ..]]

fibEven = [fibInfLst !! x | x <- [2, 5 ..]]
fibBound = takeWhile (< 4000000) fibEven
result = sum(fibBound)

main = do
  printf "Result: %d\n" (result::Int)
