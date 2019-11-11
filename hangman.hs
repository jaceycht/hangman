import System.IO

hangman :: IO ()
hangman  =
    do putStrLn "Think of a word: "
       word <- sgetLine
       putStrLn "Try to guess it:"
       play word

getCh :: IO Char
getCh  = 
    do hSetEcho stdin False
       x <- getChar
       hSetEcho stdin True
       return x

sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n' then
                do putChar '\n'
                   return ""
              else
                do putChar '-'
                   xs <- sgetLine
                   return (x:xs)

play :: String -> IO () 
play word = 
    do putStr "> "
       guess <- getLine
       let r = match word guess -- not r <- match word guess
       if (elem '-' r) then
          do putStrLn r
             play word
       else 
          putStrLn "You won!"

match :: String -> String -> String 
match xs ys = [if c `elem` ys then c else '-' | c <- xs]