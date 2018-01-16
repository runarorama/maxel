module Data.Pixel where

import Control.Applicative

data Pixel a = Pixel { row :: a, column :: a }
  deriving (Eq, Ord, Show)

-- | row . diagonal == column . diagonal
diagonal :: a -> Pixel a
diagonal a = Pixel a a

-- | row . transpose == column && column . transpose == row
transpose :: Pixel a -> Pixel a
transpose (Pixel m n) = Pixel n m

-- | A pixel p is diagonal exactly when transposePixel p == p
isDiagonal :: Eq a => Pixel a -> Bool
isDiagonal (Pixel m n) = m == n

-- | columnCollinear x y == column x == column y
columnCollinear :: (Eq a) => Pixel a -> Pixel a -> Bool
columnCollinear x y = column x == column y

-- | rowCollinear x y == column x == column y
rowCollinear :: (Eq a) => Pixel a -> Pixel a -> Bool
rowCollinear x y = row x == row y

-- | rowCollinear x y == (column x == column y || row x == row y)
collinear :: (Eq a) => Pixel a -> Pixel a -> Bool
collinear = liftA2 (||) <$> columnCollinear <*> rowCollinear

(*?) :: Eq a => Pixel a -> Pixel a -> Maybe (Pixel a)
Pixel k l *? Pixel m n | l == m = Just (Pixel k m)
                       | otherwise = Nothing
