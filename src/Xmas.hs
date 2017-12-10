{-# LANGUAGE FlexibleContexts #-}
module Xmas
(
  getDay,
  countSteps
) where
import           Control.Monad.ST
import qualified Data.Array.Base  as B
import qualified Data.Array.ST    as S
import           Data.List        (sort)
--import           Debug.Trace         (trace)

dummy :: String -> Int
dummy _ = -1

getDay :: Int -> (String -> Int, String -> Int)
getDay 1 = (globalSolve 1, \inp -> globalSolve (length inp `div` 2) inp)
getDay 2 = (calcSpreadSheet, calcSheetDiv)
getDay 3 = (calcMoves, dummy)
getDay 4 = (checkPass, checkPass2)
getDay 5 = (stepsOff 1, stepsOff 2)
getDay _ = (dummy, dummy)


-- ================= FIRST DAY OF CHRISTIMAS! =================
globalSolve :: Int -> String -> Int
globalSolve off xs = sum $ zipEq xs $ drop off inf
  where inf = cycle xs
        zipEq zs ys = [read [x] | (x, y) <- zip zs ys, x == y]


-- ================= SECOND DAY OF CHRISTIMAS! =================
calcSpreadSheet :: String -> Int
calcSpreadSheet x = sum $ map rowDiff $ lines x
  where rowDiff k = let numbs = map read (words k) in
                    maximum numbs - minimum numbs

calcSheetDiv :: String -> Int
calcSheetDiv xs = sum
  $ concat [[x `div` y | let numbs = map read (words row),
                         x <- numbs,
                         y <- numbs,
                         x `mod` y == 0,
                         x /= y] | row <- lines xs]


-- ================= THIRD DAY OF CHRISTIMAS! =================
type Point = (Int, Int)
calcMoves :: String -> Int
calcMoves x = let (p1, p2) = getCoords (read x) in
  (abs (fst p1 - fst p2) + abs (snd p1 - snd p2))


getCoords :: Int -> (Point, Point)
getCoords x
  | x > (end - side) = ((side - (end - x), 0), center)
  | x > (end - side * 2) = ((0, (end - side) - x), center)
  | x > (end - side * 3) = (((end - (side * 2)) - x, side), center)
  | x > (end - side * 4) = ((side, (end - (side * 3)) - x), center)
  | otherwise = (center, center)
  where
    toDouble = realToFrac :: Int -> Double
    end = head $ dropWhile (< x) [y ^ (2 :: Int) |
      let n = (ceiling . sqrt . toDouble) x,
      y <- [n..], odd y]
    side = round $ sqrt (toDouble end) - 1
    center = let half = floor (toDouble side / 2) in (half, half)


-- ================= FOURTH DAY OF CHRISTIMAS! =================
checkPass :: String -> Int
-- Length of filtered array of the lines, each line subdivide the list of
-- chars into list of strings by space and check if no dup with validRow
checkPass = length . filter (validRow . words) . lines

checkPass2 :: String -> Int
-- Same as before but sort each 'word' alphabetically before checking the row
-- Two aplhabetically sorted anagrams are the same thing
checkPass2 = length . filter (validRow . map sort . words) . lines

validRow :: (Eq a) => [a] -> Bool
validRow [] = True
validRow (x : xs)
  | x `elem` xs = False
  | otherwise = validRow xs


-- ================= FIFTH DAY OF CHRISTIMAS! =================
stepsOff :: Int -> String -> Int
stepsOff n xs = case n of
    2 -> countSteps (\z -> z + (if z >= 3 then -1 else 1)) inpt
    1 -> countSteps (+1) inpt
    _ -> 0
  where
    inpt = map read $ lines xs

-- This function was quite the monadic adventure, I didn't literally
-- spent the entirity of the 3 days making this function but  inderectly
-- by learning how monads worked, and what were the best collections,
-- by benchmarking a lot of them, I've made a 15sec function into a 580ms one
countSteps :: (Int -> Int) -> [Int] -> Int
countSteps rule xs = runST $ do
  let siz = (0, length xs)
  arr <- S.newListArray siz xs :: ST s (S.STUArray s Int Int)
  let manySteps ind ttl
        | S.inRange siz ind = do
          curr <- B.unsafeRead arr ind
          _ <- B.unsafeWrite arr ind $! rule curr
          manySteps (ind + curr) $! (ttl + 1)
        | otherwise = return ttl
  subtract 2 <$> manySteps 0 0


-- ================= SIXTH DAY OF CHRISTIMAS! =================
-- evenBanks :: String -> Int
-- evenBanks xs = runST $ do
--   let origBanks = (map read . words) xs
--       siz = (0, length origBanks)
--   banks <- S.newListArray siz origBanks :: ST s (S.STUArray s Int Int)
--   return 1
