-- Stwórz moduł ReadInts
module ReadInts where
import Data.List.Split
import Data.Char

-- a.
readInts :: String -> [Integer]
readInts str = fmap read (filter (all isDigit) (splitOn " " str)) :: [Integer]

-- b.
readEither :: String -> Either String Int
readEither str = case (all isDigit str) of 
                    True -> Right (read str :: Int)
                    False -> Left $ "Error: " ++ str ++ " is not a number"

-- readInts2 :: String -> Either String [Int]
-- readInts2 str = ri $ splitOn " " str where
--     ri :: [String] -> Either String [Int]
--     ri (x:xs) = readEither x >>= \n -> n:(ri xs)
readInts2 str = mapM readEither (splitOn " " str)

sumInts :: String -> String
sumInts str = case (readInts2 str) of
                Left error -> error
                Right l -> show $ foldr (+) 0 l

