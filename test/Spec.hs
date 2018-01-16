{-# LANGUAGE TemplateHaskell  #-}

module Main where

import           Control.Monad (void)
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Data.Maxel as M
import qualified Data.Pixel as P
import Data.Maxel (Maxel(..))
import Data.Pixel (Pixel(..))

genPixel :: Gen (Pixel Int)
genPixel = do
  x <- Gen.int Range.linearBounded
  y <- Gen.int Range.linearBounded
  return $ Pixel x y

genDiagonal :: Gen (Pixel Int)
genDiagonal =
  P.diagonal <$> Gen.int Range.linearBounded

-- A pixel 'p' is a diagonal exactly when 'transpose p = p'
prop_transpose_pixel_diagonal :: Property
prop_transpose_pixel_diagonal =
  property $ do
    a <- forAll genDiagonal
    P.transpose a === a

-- transpose . transpose = id
prop_transpose_pixel_identity :: Property
prop_transpose_pixel_identity =
  property $ do
    a <- forAll genPixel
    P.transpose (P.transpose a) === a

-- For a pixel 'a = (m,n)', 'row a = m' and 'col a = n'
prop_row_col :: Property
prop_row_col =
  property $ do
    p <- forAll genPixel
    let Pixel m n = p
    P.row p === m
    P.column p === n

tests :: IO Bool
tests = checkParallel $$(discover)

main :: IO ()
main = void tests
