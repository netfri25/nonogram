module Board
  ( Matrix
  , Row
  , Cell(..)
  , Group
  , Board(..)
  , Line
  ) where

type Matrix a = [Row a]
type Row a = [a]

data Cell = Fill | Remove
  deriving Eq

instance Show Cell where
  show Fill   = "@"
  show Remove = "x"

type Group = Int

data Board = MkBoard
  { rowsGroups :: [[Group]]
  , colsGroups :: [[Group]]
  , boardGrid :: Matrix (Maybe Cell)
  } deriving Show

type Line = Row (Maybe Cell)

-- testBoard :: Board
-- testBoard = MkBoard
--   [[5], [1, 4], [2, 1], [1, 2], [1, 1, 2], [1, 4], [1, 1, 2]]
--   [[6], [1], [1, 1], [2, 2], [2, 2], [7], [2, 1, 2]]
--   (replicate 7 (replicate 7 Nothing))
