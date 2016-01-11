main :: IO ()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    putStrLn $ unlines $
      map (\x -> (replicate (n-x) ' ') ++ (replicate x '#')) [1..n]
