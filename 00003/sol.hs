import Data.List
import Text.Printf

primereduce :: Int -> Int -> Int

primereduce 1 n = n
primereduce x n = if ((mod x n) == 0)
  then (primereduce (div x n) n)
  else (primereduce x (n + 1))

maxprime :: Int -> Int

maxprime n = (primereduce n 2)

result = maxprime 600851475143

main = do
  printf "Result: %d\n" (result::Int)
