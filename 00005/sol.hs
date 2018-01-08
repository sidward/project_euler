import Text.Printf

coprimeReduce :: Int -> Int -> Int
coprimeReduce x y
  | (g == 1) = x * y
  | otherwise = (coprimeReduce (div x g) (div y g))
  where g = (gcd x y)

evenDivisible x n
  | (n <= 1)         = x
  | ((gcd x n) == 1) = evenDivisible (n * x) (n - 1)
  | otherwise        = evenDivisible (x * (div n (gcd x n))) (n - 1)

result = evenDivisible 1 20

main = do
  printf "Result: %d\n" (result::Int)
