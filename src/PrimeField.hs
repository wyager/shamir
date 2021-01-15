-- So we can write out 2^127 - 1 as a type
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module PrimeField where

import SSSS (ByteSize, byteSize)
import GHC.TypeLits (type (^), type (-), KnownNat)
import Data.FiniteField.PrimeField as PrimeField (PrimeField, toInteger) -- from finite-field

type Secret128 = PrimeField (2^128 - 159)
type Secret256 = PrimeField (2^256 - 189)
type Secret384 = PrimeField (2^384 - 317)

data WhichPrime = P128 | P256 | P384 deriving (Show, Eq)

instance KnownNat n => ByteSize (PrimeField n) where
    byteSize _ = (bits + 7) `div` 8
        where 
            highest = PrimeField.toInteger (maxBound :: PrimeField n)
            go n 0 = n
            go n m = go (n+1) (m `div` 2)
            bits = go 0 highest