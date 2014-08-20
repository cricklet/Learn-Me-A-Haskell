main = do
  putStrLn "What's your name?" -- IO ()
  name <- getLine -- IO String
  putStrLn $ "Hi, " ++ name
  putStrLn "What's your age?"
  ageStr <- getLine
  let age = read ageStr
  putStrLn $ "Your age times 2 is: " ++ show (age * 2)
