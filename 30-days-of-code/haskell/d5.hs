import Control.Monad

loopSeries :: Int -> Int -> Int -> [Int]
loopSeries a b n =
  tail $ reverse $
  foldl (\acc x ->
           ((head acc) + 2 ^ x * b) : acc) [a] [0..n-1]
 
main :: IO ()
main = do
  t_tmp <- getLine
  let t = read t_tmp :: Int
  forM_ [1..t] $ \_ -> do
    x <- getLine
    let (a:b:n:_) = map read $ words x :: [Int]
    putStrLn $  unwords [show i |i <- loopSeries a b n]
