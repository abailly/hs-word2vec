module Steps where


instance Progress IO where
  progress =  BS8.putStrLn . encode

