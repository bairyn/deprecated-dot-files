module Util.Misc0 where

import Prelude hiding (id, (.))

-- | Calculate the difference between adjacent numbers.
dif :: (Num a) => [a] -> [a]
dif xs = zipWith subtract xs (tail xs)

-- | Apply 'dif' @n@ times.
difn :: (Integral n, Num a) => n -> [a] -> [a]
difn 0 xs = xs
difn n xs = difn (n - 1) $ dif xs
