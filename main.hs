parse :: String -> (String, String)
parse str = (str, "parsed")

main :: IO ()
main = do
  input <- getContents
  let parsed = parse input
  print parsed
