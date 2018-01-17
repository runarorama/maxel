{-# LANGUAGE FlexibleInstances #-}

module Data.Realm where

import Control.Applicative
import Data.Monoid (Product(..), Sum, Any(..))
import Numeric.Natural

-- | A realm is a monoid and a distributive lattice, satisfying the following:
--
-- Commutative laws
--
-- @
-- m <> n ≡ n <> m
-- m \/ n ≡ n \/ m
-- m /\ n ≡ n /\ m
-- @
--
-- Associative laws
--
-- @
-- k <> (m <> n) ≡ (k <> m) <> n
-- k \/ (m \/ n) ≡ (k \/ m) \/ n
-- k /\ (m /\ n) ≡ (k /\ m) /\ n
-- @
--
-- Distributive laws
--
-- @
-- k <> (m \/ n) ≡ (k <> m) \/ (k <> n)
-- k <> (m /\ n) ≡ (k <> m) /\ (k <> n)
-- k /\ (m \/ n) ≡ (k /\ m) \/ (k /\ n)
-- k \/ (m /\ n) ≡ (k \/ m) /\ (k \/ n)
-- @
--
-- Identity laws
--
-- @
-- mempty <> m ≡ m
-- mempty \/ m ≡ m
-- mempty /\ m ≡ mempty
-- @
--
-- Absorption laws
--
-- @
-- m \/ (m /\ n) ≡ m
-- m /\ (m \/ n) ≡ m
-- @
--
-- Idempotent laws
--
-- @
-- m \/ m ≡ m
-- m /\ m ≡ m
-- @
--
-- Summation law
--
-- @
-- (m \/ n) <> (m /\ n) ≡ m <> n
-- @
--
-- Cancellation law
--
-- @
-- (k <> n = m <> n) => (k = m)
-- @
--
-- This is a subclass of `Ord` because any join semilattice defines an order
--
-- @
-- m <= n = m \/ n == n
-- @
class (Ord a, Monoid a) => Realm a where
  (\/) :: a -> a -> a
  (/\) :: a -> a -> a

-- | Numbers with addition form a realm
instance (Ord a, Num a) => Realm (Sum a) where
  a \/ b = max a b
  a /\ b = min a b

-- | Booleans with disjunction form a realm
instance Realm Any where
  Any a \/ Any b = Any (a || b)
  Any a /\ Any b = Any (a && b)

-- | The () type is a trivial realm
instance Realm () where
  () \/ () = ()
  () /\ () = ()

-- | The product of two realms is a realm
instance (Realm a, Realm b) => Realm (a,b) where
  (a,b) \/ (c,d) = (a \/ c, b \/ d)
  (a,b) /\ (c,d) = (a /\ c, b /\ d)

-- | The natural numbers under multiplication form a realm with gcd and lcm
-- as meet and join, respectively
instance Realm (Product Natural) where
  Product m \/ Product n = Product (lcm n m)
  Product m /\ Product n = Product (gcd n m)

instance Realm a => Realm (Maybe a) where
  n \/ m = ((\/) <$> n <*> m) <|> n <|> m
  n /\ m = ((/\) <$> n <*> m) <|> n <|> m

