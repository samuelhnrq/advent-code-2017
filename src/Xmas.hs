module Xmas
(
  getDay,
  getCoords
) where
import           Data.List   (nub, sort)
import           Debug.Trace (trace)

dummy :: String -> Int
dummy _ = -1

getDay :: Int -> (String -> Int, String -> Int)
getDay 1 = (globalSolve 1, \inp -> globalSolve (length inp `div` 2) inp)
getDay 2 = (calcSpreadSheet, calcSheetDiv)
getDay 3 = (calcMoves, dummy)
getDay 4 = (checkPass, checkPass2)
getDay _ = (dummy, dummy)


-- ================= FIRST DAY OF CHRISTIMAS! =================
globalSolve :: Int -> String -> Int
globalSolve off xs = sum $ zipEq xs $ drop off inf
  where inf = cycle xs
        zipEq xs ys = [read [x] | (x, y) <- zip xs ys, x == y]


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
  where
    end = head $ dropWhile (< x) [y ^ 2 |
      let n = (ceiling . sqrt . realToFrac) x, y <- [n..], odd y]
    side = round $ sqrt (realToFrac end) - 1
    center = let half = floor (realToFrac side / 2) in (half, half)


-- ================= FOURTH DAY OF CHRISTIMAS! =================
checkPass :: String -> Int
-- Length of filtered array of the lines, each line subdivide the list of
-- chars into list of strings by space and check if no dup with validRow
checkPass = length . filter (validRow . words) . lines

checkPass2 :: String -> Int
-- Same as before but sort each 'word' alphabetically before checking the row
-- Two aplhabetically sorted anagrams are the same thing
checkPass2 = length . filter (validRow. map sort . words) . lines

validRow :: (Eq a) => [a] -> Bool
validRow [] = True
validRow (x : xs)
  | x `elem` xs = False
  | otherwise = validRow xs
