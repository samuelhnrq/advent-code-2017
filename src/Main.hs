module Main where
import           Control.Monad         (when)
import           Data.Char             (isSpace, toLower)
import           Data.List
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           System.Environment    (getArgs)
import           System.IO             (hFlush, stdin, stdout)
import           Xmas                  (getDay)

main :: IO ()
main = do
  args <- getArgs
  inp <- read <$> (if null args then do
    putStr "Which millisecond to remember?\n=> "
    _ <- hFlush stdout
    getLine
  else
    (return . head) args)
  fileStr <- dropWhileEnd isSpace
    <$> readFile ("input/xmas" ++ show inp ++ ".txt")
  let part = toLower (if length args >= 2 then head (args !! 1) else 'b')
      (d1, d2) = getDay inp
      answers = [d1 fileStr, d2 fileStr]
      partIs a = part == l || part == 'b' where l = toLower a
  iTime <- getPOSIXTime
  let showDuration = subtract iTime <$> getPOSIXTime >>= print
      showRes x = do
        putStrLn $"The answer of the " ++ show (x + 1) ++ "# part is:"
        print (answers !! x)
        putStr "It took "
        showDuration
  when (partIs 'f') $ showRes 0
  when (partIs 's') $ showRes 1
