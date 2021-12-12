module Utils
    ( readChars
    ) where

import qualified Data.Text as T

readChars :: String -> [Int]
readChars inputs = map (read . T.unpack) $ T.chunksOf 1 (T.pack inputs)