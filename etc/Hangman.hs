

wordFile = ""
guessLimit = 10

main :: IO()
main = do
  w <- randomWord
  gameLoop w ""

randomWord :: IO String
randomWord = do
  wlist <- wordFile
  words wlist


gameLoop :: String -> String -> IO()
gameLoop w g | win = showWin
             | lose = showLose
             | otherwise = 
               do displayStatus
                  guesses <- getLine
                  gameLoop w (g `union` take lives guesses)
                
                where 

  win = and [c `elem` g | c <- w ] -- all (`elem` g) w
  lose = lives <= 0
  lives = guessLimit - length ( g \\ w)
  showWin = putStr $ "Win! " ++ w
  showLose = putStr $ "Lose " ++ w
  displayStatus = do
    putStrLn [if c `elem` g then c else '_' | c <- w]
    putStrLn ("Type your guess (" ++ show lives ++ " remaining)")