import Data.Char(isHexDigit, digitToInt)
-- isHexDigit :: Char -> Bool
-- digitToInt :: Char -> Int

import Control.Monad.Error.Class
data ParseError = Err {location::Int, reason::String} deriving Show

-- type ParseMonad a = Either ParseError a 
type ParseMonad = Either ParseError
toString :: Integer -> ParseMonad String
toString = undefined

parseHexDigit :: Char -> Int -> ParseMonad Integer
parseHexDigit c loc = if (isHexDigit c) 
                    then Right $ toInteger $ digitToInt c
                    else Left Err {location=loc, reason="Invalid character"}

parseHex :: String -> ParseMonad Integer
parseHex str = parseHex' 0 (reverse str)

parseHex' :: Int -> String -> ParseMonad Integer
parseHex' _ [] = return 0
parseHex' i (x:xs) = do
                        m <- parseHexDigit x (length xs)
                        n <- parseHex' (i+1) xs
                        return (n * 16 + m)

-- convert zamienia  napis z liczba szesnastkowa 
--   na napis z liczba dziesietna
convert :: String -> String
convert s = str where
    (Right str) = tryParse s `catchError` printError
    tryParse s = do {n <- parseHex s; return $ show n}
    -- printError e = undefined
    printError :: ParseError -> ParseMonad String
    printError (Err loc msg) = return $ concat ["At index ",show loc,": ",msg]
    