import Text.Printf

fn :: Double -> Double
fn n = abs((n * (n + 1)/2)**2 - (n * (n + 1) * (2 * n + 1) / 6))

result = fn 100

main = do
  printf "Result: %d\n" ((floor result)::Int)
