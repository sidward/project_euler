import Data.List
import Text.Printf

result = sum [x | x <- [1, 2 .. 999], (mod x 3) == 0 || (mod x 5) == 0]

main = do
  printf "Result: %d\n" (result::Int)
