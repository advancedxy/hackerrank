import System.IO

weirdNess :: Int -> String
weirdNess n
  | n `mod` 2 /= 0 = "Weird"
  | n `mod` 2 == 0 && n >= 6 && n <= 20 = "Weird"
  | otherwise = "Not Weird"

main :: IO ()
main = do
  n_temp <- getLine
  let n = read n_temp :: Int
  putStrLn $ weirdNess n
