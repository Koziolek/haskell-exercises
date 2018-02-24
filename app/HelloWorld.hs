module HelloWorld where

main :: IO ()

main = do
      putStrLn "Hello World"
      putStrLn "Bye!"


quicksort :: Ord a => [a] -> [a]
quicksort [] =[]
quicksort (h:t) =
  let smaller = quicksort [ a | a <- t, a <= h  ]
      lager   = quicksort [ a | a <- t, a > h  ]
  in smaller ++ [h] ++ lager