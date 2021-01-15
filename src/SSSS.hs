-- To use:
-- Install GHC. Install the libraries mentioned below.
-- $ cabal install finite-field matrix entropy # or stack install
-- $ ghci SSSS.hs
-- > secret <- randomCoeff -- or secret = 0x12345...
-- > shards <- encodeSecret secret 2 3 -- 2-of-3 recovery
-- > test_recovery 2 3 secret shards
-- > recoverSecret (take 2 shards)
-- Max secret size is determined by choice of prime number.

{-# LANGUAGE ScopedTypeVariables #-}
module SSSS where

import Data.Matrix (rref, fromLists, (!)) -- from matrix
import System.Entropy (getEntropy) -- from entropy
import Data.ByteString as ByteString (foldl) -- from bytestring


newtype Polynomial f = Polynomial {coeffs :: [f]} deriving Show 
data Shard f = Shard {shardX :: f, shardY :: f} deriving (Show, Functor)

class ByteSize f where
    byteSize :: proxy f -> Int

degree :: Polynomial f -> Int
degree = length . coeffs

evaluate :: Num f => Polynomial f -> f -> f
evaluate poly x = sum $ zipWith (*) (coeffs poly) powers
    where powers = iterate (* x) 1

randomCoeff :: forall f . (Num f, ByteSize f) => IO f
randomCoeff = do
    bytes <- getEntropy ((byteSize :: Maybe f -> Int) Nothing) -- In case someone wants to use 256 bit
    let convert byte = fromIntegral (fromEnum byte)
        coeff = ByteString.foldl (\total byte -> total * 256 + convert byte) 0 bytes
    return coeff

makeRandomPolynomial :: (Num f, ByteSize f) => f -> Int -> IO (Polynomial f)
makeRandomPolynomial secret requiredForRecovery = do
    randomCoeffs <- sequence $ replicate (requiredForRecovery - 1) randomCoeff
    return $ Polynomial (secret : randomCoeffs)

mkShard :: (Num f) => Polynomial f -> f -> Shard f
mkShard poly x = Shard {shardX = x, shardY = evaluate poly x}

mkShards :: (Num f, Enum f) => Polynomial f -> Int -> [Shard f]
mkShards poly extra = take (degree poly + extra) $ map (mkShard poly) (iterate (+1) 1)

recoverSecret :: (Fractional f, Eq f) => [Shard f] -> Either String f
recoverSecret shards = do
        let rows = [take (length shards) (iterate (* x) 1) ++ [y] | Shard x y <- shards] 
        solved <- rref $ fromLists rows
        return $ solved ! (1, length shards + 1)

encodeSecret :: (Num f, ByteSize f, Enum f) => f -> Int -> Int -> IO [Shard f]
encodeSecret secret m n 
    | m > n = fail "m must not exceed n"
    | otherwise = do
        poly <- makeRandomPolynomial secret m
        return (mkShards poly (n - m))

-- Testing

sublists :: [a] -> [[a]]
sublists [] = [[]]
sublists (x:xs) = do
    sublist <- sublists xs
    [x:sublist, sublist]

test_recovery :: (Fractional f, Eq f, Show f) => Int -> Int -> f -> [Shard f] -> IO ()
test_recovery m n secret encoded = do
    let recovery_sets = filter (\l -> length l == m) (sublists encoded)
        validate recovery_set = if 
            recoverSecret recovery_set == Right secret 
            then return () 
            else fail $ "Could not recover " ++ show (recovery_set, secret, m, n, recoverSecret recovery_set)
    mapM_ validate recovery_sets

test_m_of_n :: forall proxy f . (Fractional f, Eq f, ByteSize f, Enum f, Show f) => proxy f -> Int -> Int -> IO ()
test_m_of_n _ m n = do
    secret <- randomCoeff :: IO f
    encoded <- encodeSecret secret m n 
    test_recovery m n secret encoded
    
test :: forall proxy f . (Fractional f, Eq f, ByteSize f, Enum f, Show f) => proxy f -> IO ()
test p = sequence_ [test_m_of_n p m (m+5) | m <- [1,2,3,4,5,6,7]]
