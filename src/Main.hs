module Main where
import           Data.Char          (isSpace)
import           Data.Time          (diffUTCTime, getCurrentTime)
import           System.Environment (getArgs)
import           System.IO          (hFlush, stdin, stdout)
import           Xmas               (getDay)

main :: IO ()
main = do
  args <- getArgs
  inp <- if null args then do
    putStr "Which millisecond to remember?\n=> "
    _ <- hFlush stdout
    readLn :: IO Int
  else
    (return . read . head) args
  millisChristimas inp

millisChristimas :: Int -> IO ()
millisChristimas x = do
  fileStr <- readFile $ "xmas" ++ show x ++ ".txt"
  let inp = reverse $ dropWhile isSpace $ reverse fileStr
  let (d1, d2) = getDay x
  putStrLn $ "The answer for the first and second part, are, respectivily:\n"
    ++ show (d1 inp) ++ "\n" ++ show (d2 inp)
