parse :: String -> [Int]
parse str = []

main :: IO ()
main = do
  input <- getContents
  let parsed = parse input
  print parsed
