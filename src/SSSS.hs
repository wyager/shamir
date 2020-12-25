-- To use:
-- Install GHC. Install the libraries mentioned below.
-- $ cabal install finite-field matrix entropy # or stack install
-- $ ghci SSSS.hs
-- > secret <- randomCoeff -- or secret = 0x12345...
-- > shards <- encodeSecret secret 2 3 -- 2-of-3 recovery
-- > test_recovery 2 3 secret shards
-- > recoverSecret (take 2 shards)
-- Max secret size is determined by choice of prime number.

-- So we can write out 2^127 - 1 as a type
{-# LANGUAGE DataKinds, TypeOperators #-}
module SSSS where
import GHC.TypeLits (type (^), type (-))
import Data.FiniteField.PrimeField as PrimeField (PrimeField) -- from finite-field
import Data.Matrix (rref, fromLists, (!)) -- from matrix
import System.Entropy (getEntropy) -- from entropy
import Data.ByteString as ByteString (foldl) -- from bytestring

type Prime = (2^127) - 1 -- can also use (2^256 - 189)
type Field = PrimeField Prime 
newtype Polynomial = Polynomial {coeffs :: [Field]} deriving Show
data Shard = Shard {shardX :: Field, shardY :: Field} deriving Show

degree :: Polynomial -> Int
degree = length . coeffs

evaluate :: Polynomial -> Field -> Field
evaluate poly x = sum $ zipWith (*) (coeffs poly) powers
    where powers = iterate (* x) 1

randomCoeff :: IO Field
randomCoeff = do
    bytes <- getEntropy 32 -- In case someone wants to use 256 bit
    let convert byte = fromIntegral (fromEnum byte)
        coeff = ByteString.foldl (\total byte -> total * 256 + convert byte) 0 bytes
    return coeff

makeRandomPolynomial :: Field -> Int -> IO Polynomial
makeRandomPolynomial secret requiredForRecovery = do
    randomCoeffs <- sequence $ replicate (requiredForRecovery - 1) randomCoeff
    return $ Polynomial (secret : randomCoeffs)

generateShard :: Polynomial -> IO Shard
generateShard poly = do
    x <- randomCoeff
    return $ Shard {shardX = x, shardY = evaluate poly x}

generateShards :: Polynomial -> Int -> IO [Shard]
generateShards poly extra = sequence $ replicate (degree poly + extra) (generateShard poly)

recoverSecret :: [Shard] -> Either String Field
recoverSecret shards = do
        let rows = [take (length shards) (iterate (* x) 1) ++ [y] | Shard x y <- shards] 
        solved <- rref $ fromLists rows
        return $ solved ! (1, length shards + 1)

encodeSecret :: Field -> Int -> Int -> IO [Shard]
encodeSecret secret m n 
    | m > n = fail "m must not exceed n"
    | otherwise = do
        poly <- makeRandomPolynomial secret m
        shards <- generateShards poly (n - m) 
        return shards

-- Testing

sublists :: [a] -> [[a]]
sublists [] = [[]]
sublists (x:xs) = do
    sublist <- sublists xs
    [x:sublist, sublist]

test_recovery :: Int -> Int -> Field -> [Shard] -> IO ()
test_recovery m n secret encoded = do
    let recovery_sets = filter (\l -> length l == m) (sublists encoded)
        validate recovery_set = if 
            recoverSecret recovery_set == Right secret 
            then return () 
            else fail $ "Could not recover " ++ show (recovery_set, secret, m, n, recoverSecret recovery_set)
    mapM_ validate recovery_sets

test_m_of_n :: Int -> Int -> IO ()
test_m_of_n m n = do
    secret <- randomCoeff
    encoded <- encodeSecret secret m n 
    test_recovery m n secret encoded
    
main :: IO ()
main = sequence_ [test_m_of_n m n | m <- [1..7], n <- [m..m+5]]
