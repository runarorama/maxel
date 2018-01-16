{-# LANGUAGE TemplateHaskell  #-}

module Main where

import           Control.Monad (void)
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Data.Maxel

genPixel :: Gen (Pixel Int)
genPixel = do
  x <- Gen.int Range.linearBounded
  y <- Gen.int Range.linearBounded
  return $ Pixel x y

genDiagonal :: Gen (Pixel Int)
genDiagonal =
  diagonal <$> Gen.int Range.linearBounded

-- A pixel 'p' is a diagonal exactly when 'transpose p = p'
prop_transpose_diagonal :: Property
prop_transpose_diagonal =
  property $ do
    a <- forAll genDiagonal
    transpose a === a

-- transpose . transpose = id
prop_transpose_identity :: Property
prop_transpose_identity =
  property $ do
    a <- forAll genPixel
    transpose (transpose a) === a

-- For a pixel 'a = (m,n)', 'row a = m' and 'col a = n'
prop_row_col :: Property
prop_row_col =
  property $ do
    p <- forAll genPixel
    let Pixel m n = p
    row p === m
    column p === n

tests :: IO Bool
tests = checkParallel $$(discover)

main :: IO ()
main = void tests
