module Solver (solve) where

import Board (Board(MkBoard), Group, Matrix, Cell(..), Line, Row)

import Control.Monad (guard, mfilter)
import Data.List (transpose)
import Control.Applicative (Alternative(..))

-- TODO: find all solutions in case of a grid with more than one solution
solve :: Board -> Board
solve (MkBoard rgs cgs grid) = MkBoard rgs cgs $ fix solveAll grid
  where
    solveAll = transpose . solveLines cgs . transpose . solveLines rgs

    solveLines :: [[Group]] -> Matrix (Maybe Cell) -> Matrix (Maybe Cell)
    solveLines = zipWith solveLine

fix :: Eq a => (a -> a) -> a -> a
fix f x
  | f x' == x = x'
  | otherwise = fix f x'
  where x' = f x

solveLine :: [Group] -> Line -> Line
solveLine gs line = zipWith (<|>) line $ mergeLines $ filter (matches line) $ lineCombinations gs $ length line
  where
    matches :: Line -> Row Cell -> Bool
    matches xs = and . zipWith (\x y -> maybe True (==y) x) xs

mergeLines :: [Row Cell] -> Line
mergeLines [] = []
mergeLines xs | any null xs = []
mergeLines xs =
  let heads = map head xs
      tails = map tail xs
      this = mfilter (const $ allEq heads) $ Just (head heads)
   in this : mergeLines tails

allEq :: Eq a => [a] -> Bool
allEq = and . (zipWith (==) <$> init <*> tail)

-- INFO: might return a row longer than the given length, but that's
--       fine because in the solveLine it's selected based on the original row
lineCombinations :: [Group] -> Int -> [Row Cell]
lineCombinations _ len | len <= 0 = return []
lineCombinations [] len = return $ replicate len Remove
lineCombinations (g:gs) len = do
  guard (sum (g:gs) + length gs <= len)
  let ls = (Remove:) <$> lineCombinations (g:gs) (pred len)
  let rs = (\rest -> replicate g Fill ++ Remove : rest) <$> lineCombinations gs (len - g - 1)
  ls ++ rs
