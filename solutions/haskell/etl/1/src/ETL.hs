module ETL (transform) where

import qualified Data.Map as M
import qualified Data.Text as T

transform :: M.Map a T.Text -> M.Map Char a
transform = M.foldlWithKey' step M.empty
  where
    step :: M.Map Char a -> a -> T.Text -> M.Map Char a
    step mp k txt = T.foldl' (\mp' ch -> M.insert ch k mp') mp (T.toLower txt)
