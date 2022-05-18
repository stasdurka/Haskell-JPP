-- Stwórz moduł ReadInts
module ReadInts where
import Data.List.Split
import Data.Char

-- a.
readInts :: String -> [Integer]
readInts str = fmap read (filter (all isDigit) (splitOn " " str)) :: [Integer]

-- b.
readEither :: String -> Either String Int
readEither str = if all isDigit str 
                then Right (read str :: Int) 
                else Left $ "Error: " ++ str ++ " is not a number"

-- readInts2 :: String -> Either String [Int]
-- readInts2 str = ri $ splitOn " " str where
--     ri :: [String] -> Either String [Int]
--     ri (x:xs) = readEither x >>= \n -> n:(ri xs)
readInts2 str = mapM readEither (splitOn " " str)

sumInts :: String -> String
sumInts str = case readInts2 str of
                Left error -> error
                Right l -> show $ sum l

