{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Bifunctor
import           Data.Monoid
import           Control.Monad (void)
import           Test.QuickCheck

import Data.Realm
import qualified Data.Maxel as M
import qualified Data.Pixel as P
import Data.Maxel (Maxel(..))
import Data.Pixel (Pixel(..))

instance Arbitrary a => Arbitrary (Pixel a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Pixel x y

genDiagonal :: Arbitrary a => Gen (Pixel a)
genDiagonal = P.diagonal <$> arbitrary

instance (Ord a, Arbitrary a) => Arbitrary (Maxel a) where
  arbitrary = M.fromList <$> listOf arbitrary

-- A pixel 'p' is a diagonal exactly when 'transpose p = p'
prop_transpose_pixel_diagonal :: Property
prop_transpose_pixel_diagonal =
  forAll (genDiagonal @Int) $ \a -> P.transpose a === a

-- transpose . transpose = id
prop_transpose_pixel_identity :: Property
prop_transpose_pixel_identity =
  forAll (arbitrary @(Pixel Int)) $ \a -> P.transpose (P.transpose a) === a

-- For a pixel 'a = (m,n)', 'row a = m' and 'col a = n'
prop_row_col :: Property
prop_row_col =
  forAll (arbitrary @(Pixel Int)) $ \p ->
    let Pixel m n = p in P.row p === m .&&. P.column p === n

-- The empty maxel is empty
prop_empty_null :: Property
prop_empty_null = property $ M.null @Int M.empty

-- The empty maxel has size 0
prop_empty_zero :: Property
prop_empty_zero = property . (== 0) . M.size $ M.empty @Int

prop_maxel_realm :: Property
prop_maxel_realm =
  label "Maxel realm laws" $
  forAll (arbitrary @(Maxel Int, Maxel Int, Maxel Int)) $ \(k, m, n) ->
    label "commutative law"
      (m <> n === n <> m .&&. m \/ n === n \/ m .&&. m /\ n === n /\ m) .&&.
    label "associative law"
      (k <> (m <> n) === (k <> m) <> n .&&.
       k \/ (m \/ n) === (k \/ m) \/ n .&&.
       k /\ (m /\ n) === (k /\ m) /\ n) .&&.
    label "distributive law"
      (k <> (m \/ n) === (k <> m) \/ (k <> n) .&&.
       k <> (m /\ n) === (k <> m) /\ (k <> n) .&&.
       k /\ (m \/ n) === (k /\ m) \/ (k /\ n) .&&.
       k \/ (m /\ n) === (k \/ m) /\ (k \/ n)) .&&.
    label "identity law"
      (mempty <> m === m .&&.
       mempty \/ m === m .&&.
       mempty /\ m === mempty) .&&.
    label "absorption law"
      (m \/ (m /\ n) === m .&&.
      m /\ (m \/ n) === m) .&&.
    label "idempotent law"
      (m /\ m === m .&&.
       m \/ m === m) .&&.
    label "summation law"
      ((m \/ n) <> (m /\ n) === m <> n) .&&.
    label "cancellation law"
      ((k <> n == m <> n) === (k == m))

return []

tests :: IO Bool
tests = $quickCheckAll

main :: IO ()
main = void tests
