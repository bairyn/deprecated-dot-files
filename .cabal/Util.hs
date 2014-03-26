--{-# OPTIONS_GHC -interactive-print=myPrint #-}  -- This doesn't work; added
--it to alias in .zshrc.
{-# LANGUAGE Arrows #-}
module Util
    ( scramble
    , scrambleIO
    , shuffleWords
    , myPrint

    , module Util.Misc0
    ) where

import Prelude hiding (id, (.))
import Control.Arrow
import Control.Category
import Control.Monad
import System.Random
-- http://stackoverflow.com/questions/5535512/how-to-hack-ghci-or-hugs-so-that-it-prints-unicode-chars-unescaped
import System.IO
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import Util.Misc0

-- Very inefficiently scramble a list.
scramble :: [a] -> StdGen -> ([a], StdGen)
scramble xs s = case length xs of
    0 -> (xs, s)
    n -> let (currentElemIndex, s') = randomR (0, pred n) s
             (left, right) = splitAt currentElemIndex xs
         in  first (head right :) $ scramble (left ++ tail right) s'

scrambleIO :: [a] -> IO [a]
scrambleIO xs = do
    s <- getStdGen
    let (xs', s') = scramble xs s
    setStdGen s'
    return xs'

-- | Shuffle the words in a string and print the result.
shuffleWords :: String -> IO ()
shuffleWords = putStrLn . unwords <=< scrambleIO . words

-- http://stackoverflow.com/questions/5535512/how-to-hack-ghci-or-hugs-so-that-it-prints-unicode-chars-unescaped
myPrint :: (PP.Pretty a) => a -> IO ()
myPrint a = (PP.hPutDoc stdout . PP.pretty) a >> putStrLn ""
