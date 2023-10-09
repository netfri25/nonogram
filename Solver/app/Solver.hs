-- TODO: optimize the solver (BY A LOT)
module Solver (solve) where

import Board (BoardGroups(..), Group, Matrix, Cell(..), Line, Row, Grid)

import Control.Monad (guard, mfilter)
import Data.List (transpose)
import Control.Applicative (Alternative(..))

-- TODO: find all solutions in case of a grid with more than one solution
solve :: BoardGroups -> Grid
solve (MkBoardGroups rgs cgs) = solve' rows_combs cols_combs empty_grid
  where
    rows = length cgs
    cols = length rgs

    empty_grid = replicate rows (replicate cols Nothing)

    rows_combs = map (`lineCombinations` length cgs) rgs
    cols_combs = map (`lineCombinations` length rgs) cgs

    solve' :: [[Row Cell]] -> [[Row Cell]] -> Matrix (Maybe Cell) -> Matrix (Maybe Cell)
    solve' rcombs ccombs grid =
      let (grid',  rcombs') = solveLines rcombs grid
          (grid'', ccombs') = solveLines ccombs (transpose grid')
          grid''' = transpose grid''
          changed = grid /= grid''' || rcombs /= rcombs' || ccombs/= ccombs'
       in if changed
          then solve' rcombs' ccombs' grid'''
          else grid'''

    solveLines :: [[Row Cell]] -> Matrix (Maybe Cell) -> (Matrix (Maybe Cell), [[Row Cell]])
    solveLines combs grid =
      let combs' = zipWith (filter . matches) grid combs
          grid' = zipWith solveLine combs grid
       in (grid', combs')

solveLine :: [Row Cell] -> Line -> Line
solveLine combs line = zipWith (<|>) line $ mergeLines $ filter (matches line) combs

matches :: Line -> Row Cell -> Bool
matches xs = and . zipWith (\m v -> maybe True (==v) m) xs

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
lineCombinations gs 0 = guard (null gs) >> return []
lineCombinations _ len | len < 0 = return []
lineCombinations [] len = return $ replicate len Remove
lineCombinations (g:gs) len = do
  guard (sum (g:gs) + length gs <= len)
  let pad_remove = (Remove:) <$> lineCombinations (g:gs) (pred len)
  let create_group = (\rest -> replicate g Fill ++ Remove : rest) <$> lineCombinations gs (len - g - 1)
  create_group ++ pad_remove
