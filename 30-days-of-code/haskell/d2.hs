-- Enter your code here. Read input from STDIN. Print output to STDOUT
import Text.Printf

calPrice :: (RealFrac a, Integral b) => a -> a -> a ->b
calPrice price tip tax =
  let tp = (price * tip) / 100
      tx = price * tax / 100
  in round (price + tp + tx)

main :: IO()
main = do
  price <- readLn :: IO Double
  tip <- readLn :: IO Double
  tax <- readLn :: IO Double
  let final = calPrice price tip tax
  printf "The final price of the meal is %d.\n" (final :: Int)
