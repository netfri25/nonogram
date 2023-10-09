module Board
  ( Matrix
  , Row
  , Cell(..)
  , Group
  , BoardGroups(..)
  , Line
  , Grid
  , parseBoardGroups
  ) where

type Matrix a = [Row a]
type Row a = [a]

data Cell = Fill | Remove
  deriving Eq

instance Show Cell where
  show Fill   = "@"
  show Remove = "x"

type Group = Int

data BoardGroups = MkBoardGroups
  { groupsRows :: [[Group]]
  , groupsCols :: [[Group]]
  } deriving Show

type Line = Row (Maybe Cell)

type Grid = Matrix (Maybe Cell)

-- the first line contains the list of rows groups, and the second line is for the columns
parseBoardGroups :: String -> Maybe BoardGroups
parseBoardGroups input =
  case map read (lines input) of
    [rows, cols] -> Just $ MkBoardGroups rows cols
    _ -> Nothing
