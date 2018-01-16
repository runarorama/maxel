module Data.Maxel where

import Control.Applicative
import Data.MultiSet

data Pixel a = Pixel { row :: a, column :: a }
  deriving (Eq, Ord, Show)

newtype Maxel a = Maxel { unMaxel :: MultiSet (Pixel a) }
  deriving (Eq, Ord, Show)

diagonal :: a -> Pixel a
diagonal a = Pixel a a

transpose :: Pixel a -> Pixel a
transpose (Pixel m n) = Pixel n m

columnCollinear :: (Eq a) => Pixel a -> Pixel a -> Bool
columnCollinear (Pixel _ c1) (Pixel _ c2) = c1 == c2

rowCollinear :: (Eq a) => Pixel a -> Pixel a -> Bool
rowCollinear (Pixel r1 _) (Pixel r2 _) = r1 == r2

collinear :: (Eq a) => Pixel a -> Pixel a -> Bool
collinear = liftA2 (||) <$> columnCollinear <*> rowCollinear
