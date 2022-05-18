import System.Environment
import System.IO

interact' :: (String -> String) -> IO ()
interact' f = do 
    s <- getContents
    putStr (f s)

-- favLanguage = getLine >>= (\str -> if str /= "Haskell" then return ())
    -- getArgs >>= print 

    --(b) Napisz program, który będzie pytał użytkownika o ulubiony język programowania, tak długo aż odpowiedzią będzie 'Haskell'

ask :: IO ()
ask = do
    line <- getLine
    if line == "Haskell"
        then return ()
        else do
            putStrLn "NIE"
            ask

-- main = ask

-- c. Napisz uproszczoną wersję programu wc (wypisującą ilość linii, słów i znaków w pliku o nazwie podanej jako argument, bądź stdin jeśli bez argumentu).

-- import System.Environment
-- import System.IO

processFile :: Handle -> IO ()
processFile h = do
    contents <- hGetContents h
    putStrLn "linie:"
    print (length $ lines contents)
    putStrLn "znaki:"
    print (length contents)
    putStrLn "słowa:"
    print (length $ words contents)

main = do
    args <- getArgs
    if not (null args)
        then 
            withFile (head args) ReadMode processFile
        else do
            input <- getLine
            putStrLn input

