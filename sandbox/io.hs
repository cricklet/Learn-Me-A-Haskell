maybeRead :: Read a => String -> Maybe a
maybeRead s = case reads s of
                  [(x,"")] -> Just x
                  _        -> Nothing

doubleAge = do
  putStrLn "What's your age?"
  ageStr <- getLine
  let age = maybeRead ageStr in
    case age of
      Just a  -> putStrLn $ "Your age times 2 is: " ++ show (a * 2)
      Nothing -> doubleAge

main = do
  doubleAge
