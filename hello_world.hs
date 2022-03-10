main = interact czesc

czesc :: String -> String 
czesc s = czesc2 (lines s)

czesc2 :: [String] -> String 
czesc2 ss = "jak masz na imie?\n" ++ go s
    where
        go (ss:_) = "Czesc " ++ ss "!\n"
        go [] = "nie ma z kim gadac :("