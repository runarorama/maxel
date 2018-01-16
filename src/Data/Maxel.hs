module Data.Maxel where

import           Data.Foldable
import           Data.Maybe
import qualified Data.MultiSet as S
import qualified Data.Pixel as P
import           Data.Pixel (Pixel(..))
import           Data.Realm
import           Data.Set (Set)
import           Numeric.Natural

newtype Maxel a = Maxel { unMaxel :: S.MultiSet (Pixel a) }

newtype Frame a = Frame { unFrame :: Set (Pixel a) }

instance Show a => Show (Maxel a) where
  show (Maxel s) = "fromList " ++ show (S.elems s)

fromList :: Ord a => [Pixel a] -> Maxel a
fromList ps = Maxel (S.fromList ps)

-- | The number of pixels in the maxel, repetitions included
size :: Maxel a -> Natural
size (Maxel s) = fromIntegral $ S.size s

-- | The extent of a maxel m is the pixel (r,c) where r and c are the largest row
-- and column of the pixels in m, respectively.
-- This operation is undefined for the empty maxel.
extent :: Ord a => Maxel a -> Maybe (Pixel a)
extent (Maxel s)
  | S.null s = Nothing
  | otherwise = Just $ Pixel (S.findMax $ S.map row s)
                             (S.findMax $ S.map column s)

-- | A maxel is diagonal exactly when all of its pixels are diagonal.
isDiagonal :: Eq a => Maxel a -> Bool
isDiagonal (Maxel s) = all P.isDiagonal $ S.elems s

-- | The transpose of a maxel is the maxel of the transposes of its pixels.
transpose :: Ord a => Maxel a -> Maxel a
transpose (Maxel s) = Maxel (S.map P.transpose s)

-- | A maxel is symmetric when its transpose is itself.
isSymmetric :: Ord a => Maxel a -> Bool
isSymmetric m = transpose m == m

-- | The empty maxel has no pixels.
empty :: Ord a => Maxel a
empty = fromList []

-- | Test whether a given maxel is the empty maxel.
null :: Maxel a -> Bool
null (Maxel s) = S.null s

-- | Get the pixels in a maxel as a list
pixels :: Maxel a -> [Pixel a]
pixels (Maxel s) = S.elems s

-- | Scale a maxel by a natural number n, by scaling the multiplicities.
scale :: Ord a => Natural -> Maxel a -> Maxel a
scale = (fold .) . replicate . fromIntegral

-- | A single-pixel maxel
singleton :: Ord a => a -> a -> Maxel a
singleton m n = fromList [Pixel m n]

instance Ord a => Realm (Maxel a) where
  (Maxel p) \/ (Maxel q) = Maxel (p `S.maxUnion` q)
  (Maxel p) /\ (Maxel q) = Maxel (p `S.intersection` q)

instance Ord a => Ord (Maxel a) where
  m <= n = m \/ n == n

instance Eq a => Eq (Maxel a) where
  Maxel m == Maxel n = m == n

instance Ord a => Monoid (Maxel a) where
  mappend (Maxel p) (Maxel q) = Maxel (p `S.union` q)
  mempty = empty

-- | Maxel multiplication
(<.>) :: Ord a => Maxel a -> Maxel a -> Maxel a
Maxel a <.> Maxel b = Maxel $
  S.bind a $ \x ->
  S.bind b $ \y ->
    maybe S.empty S.singleton (x P.*? y)

-- | The support of a maxel is a frame
support :: Maxel a -> Frame a
support (Maxel a) = Frame $ S.toSet a

