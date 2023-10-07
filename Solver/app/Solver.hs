-- TODO: make it find all the possible solutions, instead of only one solution
module Solver (solve) where

import Board (Board(MkBoard), Group, Matrix, Cell(..), Line, Row)

import Control.Monad (guard, mfilter)
import Data.List (transpose, groupBy)
import Data.Function (on)
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
solveLine groups = finishGroups groups . mergeAllCombs groups
  where
    mergeAllCombs :: [Group] -> Line -> Line
    mergeAllCombs gs xs = zipWith (<|>) xs $ mergeLines $ filter (matches xs) $ lineCombinations gs $ length xs

    matches :: Line -> Row Cell -> Bool
    matches xs ys = and $ zipWith (\x y -> maybe True (==y) x) xs ys

-- WARNING: didn't check it really good, so I hope that it works as expected
-- INFO: this function marks X's for groups that are already finished
finishGroups :: [Group] -> Line -> Line
finishGroups groups line = intersectLines finishGroupRegular finishGroupReverse
  where
    finishGroupRegular = concat $ finishGroups' groups $ groupBy ((==) `on` (== Just Fill)) line
    finishGroupReverse = reverse $ concat $ finishGroups' (reverse groups) $ groupBy ((==) `on` (== Just Fill)) $ reverse line

    intersectLines :: Line -> Line -> Line
    intersectLines = zipWith (\x -> (>>= \y -> mfilter (==y) x))
    -- intersectLines _ [] = []
    -- intersectLines [] _ = []
    -- intersectLines (x:xs) (y:ys)
    --   | x == y = x : intersectLines xs ys
      -- | otherwise = Nothing : intersectLines xs ys

    finishGroups' :: [Group] -> [[Maybe Cell]] -> [[Maybe Cell]]
    finishGroups' _ [] = []
    finishGroups' [] xs = xs
    finishGroups' (g:gs) (chunk:not_group:chunks) -- fill after chunk
      | head chunk == Just Fill =
        if isLength g chunk
        then chunk : [Just Remove] : tail not_group : finishGroups' gs chunks
        else chunk : not_group : finishGroups' gs chunks
    finishGroups' (g:gs) (not_group:chunk:chunks) -- fill before chunk
      | head chunk == Just Fill =
        if isLength g chunk
        then init not_group : [Just Remove] : finishGroups' gs (chunk:chunks)
        else not_group : chunk : finishGroups' gs chunks
    finishGroups' gs (not_group:chunks) = not_group : finishGroups' gs chunks

isLength :: Int -> [a] -> Bool
isLength 0 [] = True
isLength 0 _ = False
isLength _ [] = False
isLength l (_:xs) = isLength (pred l) xs

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
