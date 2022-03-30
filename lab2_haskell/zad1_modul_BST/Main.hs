module Main where 
import MySort

cat :: String -> String
cat str = let l = (read str)::[Int] in
        show (sort l)
main = interact cat
