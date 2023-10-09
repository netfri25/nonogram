-- TODO: optimize the solver (BY A LOT)
module Solver (solve) where

import Board (BoardGroups(..), Group, Matrix, Cell(..), Line, Row, Grid)
import Memo (Memo, evalMemo)
import qualified Memo as M

import Control.Monad (mfilter)
import Data.List (transpose)
import Control.Applicative (Alternative(..))

-- TODO: find all solutions in case of a grid with more than one solution
solve :: BoardGroups -> Grid
solve (MkBoardGroups rgs cgs) = solve' rows_combs cols_combs empty_grid
  where
    rows = length rgs
    cols = length cgs

    empty_grid = replicate rows (replicate cols Nothing)

    rows_combs = map (flip evalMemo mempty . flip lineCombinations cols . zip [0..]) rgs
    cols_combs = map (flip evalMemo mempty . flip lineCombinations rows . zip [0..]) cgs

    solve' :: [[Row Cell]] -> [[Row Cell]] -> Matrix (Maybe Cell) -> Matrix (Maybe Cell)
    solve' rcombs ccombs grid =
      let grid'   = zipWith solveLine rcombs grid
          grid''  = zipWith solveLine ccombs (transpose grid')
          ccombs' = zipWith (filter . matches) grid'' ccombs
          grid''' = transpose grid''
          rcombs' = zipWith (filter . matches) grid''' rcombs
          changed = grid /= grid'''
       in if changed
          then solve' rcombs' ccombs' grid'''
          else grid'''

solveLine :: [Row Cell] -> Line -> Line
solveLine combs line = zipWith (<|>) line (mergeLines combs)

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

type Indexed a = (Int, a)

-- INFO: might return a row longer than the given length, but that's
--       fine because in the solveLine it's selected based on the original row
lineCombinations :: [Indexed Group] -> Int -> Memo (Indexed Int) [Row Cell] [Row Cell]
lineCombinations [] len = return $ return $ replicate len Remove
lineCombinations (g:gs) len = do
  let key = (fst g, len)
  -- accumulates the length and the sum at the same time
  let no_space = any (>len) $ tail $ scanl (\acc -> succ . (acc+)) (-1) (map snd (g:gs))
  if no_space
  then return mempty
  else M.lookup key >>= maybe (calcNext key) return
  where
    calcNext :: Indexed Int -> Memo (Indexed Int) [Row Cell] [Row Cell]
    calcNext key = do
      let create_group = fmap (\rest -> replicate (snd g) Fill ++ Remove : rest) <$> lineCombinations gs (len - snd g - 1)
      let pad_remove = fmap (Remove:) <$> lineCombinations (g:gs) (pred len)
      value <- (++) <$> create_group <*> pad_remove
      M.insert key value
      return value
