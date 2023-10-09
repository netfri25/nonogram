module Memo (Memo, evalMemo, Memo.lookup, insert) where

import qualified Data.Map.Strict as M
import Control.Monad.State

type Memo k v = State (M.Map k v)

evalMemo :: Memo k v a -> M.Map k v -> a
evalMemo = evalState

lookup :: Ord k => k -> Memo k v (Maybe v)
lookup = gets . M.lookup

insert :: Ord k => k -> v -> Memo k v ()
insert k = modify . M.insert k
