{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Bifunctor
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as Set
import           Control.Applicative
import           Control.Monad (void, join)
import           Test.QuickCheck

import Data.Realm
import qualified Data.Maxel as M
import qualified Data.Pixel as P
import Data.Maxel (Maxel(..), Frame(..))
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

-- | Generate a maxel whose support is in the frame f
genFromFrame :: Ord a => Frame a -> Gen (Maxel a)
genFromFrame f = M.fromList <$> (listOf . elements . Set.toList $ unFrame f)

prop_transpose_pixel_diagonal :: Property
prop_transpose_pixel_diagonal =
  label "isDiagonal p <=> transpose p = p" .
    forAll (genDiagonal @Int) $ \a -> P.transpose a === a

prop_transpose_pixel_identity :: Property
prop_transpose_pixel_identity =
  label "transpose . transpose = id" .
    forAll (arbitrary @(Pixel Int)) $ \a -> P.transpose (P.transpose a) === a

prop_row_col :: Property
prop_row_col =
  label "(row (m, n), col (m, n)) = (m, n)" .
    forAll (arbitrary @(Pixel Int)) $ \p ->
      let Pixel m n = p in P.row p === m .&&. P.column p === n

prop_empty_null :: Property
prop_empty_null = label "The empty maxel is empty" .
  property $ M.null @Int M.empty

prop_empty_zero :: Property
prop_empty_zero =
  label "The empty maxel has size 0" .
    property . (== 0) . M.size $ M.empty @Int

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
      (property ((k <> n /= m <> n) || k == m))

prop_pixel_mul_transpose :: Property
prop_pixel_mul_transpose =
  label "(a b)^t = a^t b^t" .
    forAll (arbitrary @(Pixel Int, Pixel Int)) $ \(a, b) ->
      (P.transpose <$> a P.*? b) === P.transpose b P.*? P.transpose a

prop_pixel_mul_associativity :: Property
prop_pixel_mul_associativity =
  label "a (b c) = (a b) c" .
    forAll (arbitrary @(Pixel Int, Pixel Int, Pixel Int)) $ \(a, b, c) ->
      (a P.*? b >>= (P.*? c)) === ((a P.*?) =<< b P.*? c)

prop_maxel_multiplication :: Property
prop_maxel_multiplication =
  label "associative law" .
    forAll (arbitrary @(Maxel Int, Maxel Int, Maxel Int)) $ \(a, b, c) ->
      a M.<.> (b M.<.> c) === (a M.<.> b) M.<.> c

prop_diagonal_idempotent :: Property
prop_diagonal_idempotent =
  label "singleton diagonals are idempotent" .
    forAll (arbitrary @Int) $ \n ->
      let x = M.singleIdempotent n in
        x M.<.> x === x

prop_partial_identity_idempotent :: Property
prop_partial_identity_idempotent =
  label "partial identities are idempotent" .
    forAll (arbitrary @(Set Int)) $ \j ->
      let x = M.partialIdentity j in
        x M.<.> x === x

prop_partial_identity_interscect :: Property
prop_partial_identity_interscect =
  label "partial identity multiplication is set intersection" .
    forAll (arbitrary @(Set Int, Set Int)) $ \(j, j') ->
      let x = M.partialIdentity j
          y = M.partialIdentity j'
      in
        x M.<.> y === M.partialIdentity (Set.intersection j j')

cartesianFrame :: Ord a => Set a -> Frame a
cartesianFrame j = M.Frame . Set.fromList . join (liftA2 Pixel) $ Set.elems j

prop_partial_identity_support :: Property
prop_partial_identity_support =
  label "partial identity on a maxel supported by its frame is identity" .
    forAll (arbitrary @(Set Int, Maxel Int)) $ \(j, m) ->
      let ej = M.partialIdentity j
      in (ej M.<.> m == m && m M.<.> ej == m) ===
         (M.support m <= cartesianFrame j)

prop_support_product_closure :: Property
prop_support_product_closure =
  label "maxel product is closed under frames" $
    forAll (arbitrary @(Set Int)) $ \j ->
      let f = cartesianFrame j
      in forAll (if Set.null j then return (M.empty, M.empty)
                 else do
                   let f = cartesianFrame j
                   m <- genFromFrame f
                   n <- genFromFrame f
                   return (m,n)) $ \(m, n) ->
                M.support (m M.<.> n) <= f

return []

tests :: IO Bool
tests = $quickCheckAll

main :: IO ()
main = void tests

