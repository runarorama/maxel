module Data.Maxel where

import Data.MultiSet

data Pixel a = Pixel { pixelX :: a, pixelY :: a }

newtype Maxel a = Maxel { unMaxel :: MultiSet (Pixel a) }


