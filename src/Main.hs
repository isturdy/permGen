module Main where

import           GHC.Word
import           System.Environment

import qualified Array              as A
import qualified Map                as M
import qualified Trie               as T

handlers :: [(String, Word8 -> String)]
handlers = [
    ("map",   M.showLongest)
  , ("array", A.showLongest)
  , ("trie",  T.showLongest)
  ]

main :: IO ()
main = do
  [pn,ns] <- getArgs
  let p = lookup pn handlers
  let n = read ns :: Word8
  case p of
    Nothing -> error "Unsupported handler"
    Just f  -> putStrLn . show $ f n
