module Formats where

import qualified Haskoin.Keys as Keys
import PrimeField (Secret128, Secret256, Secret384, WhichPrime(P128,P256,P384))
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.FiniteField.PrimeField as PF
import GHC.TypeLits (KnownNat)
import qualified SSSS

bsToInteger :: BS.ByteString -> Integer
bsToInteger = BS.foldl' (\acc next -> acc * 256 + toInteger next) 0 

textToInteger :: T.Text -> Integer
textToInteger = bsToInteger . TE.encodeUtf8

integerToBytes :: Int -> Integer -> Either String BS.ByteString
integerToBytes len = go [] len 
    where
    go acc 0 0 = Right (BS.pack acc)
    go acc 0 x = Left $ "Could not fit result in only " ++ show len ++ " bytes"
    go acc n x = let (remainder, lowest) = x `divMod` 256 in go (fromIntegral lowest : acc) (n - 1) remainder



bytesFitIn :: MonadFail m => BS.ByteString -> m WhichPrime
bytesFitIn bs | SSSS.byteSize (Nothing :: Maybe Secret128) == BS.length bs = return P128
              | SSSS.byteSize (Nothing :: Maybe Secret256) == BS.length bs = return P256
              | SSSS.byteSize (Nothing :: Maybe Secret384) == BS.length bs = return P384
              | otherwise = fail "These bytes have a length that doesn't match any of the primes in this program"


fitsIn :: MonadFail m => Integer -> m WhichPrime
fitsIn i | PF.toInteger (maxBound :: Secret128) >= i = return P128
         | PF.toInteger (maxBound :: Secret256) >= i = return P256
         | PF.toInteger (maxBound :: Secret384) >= i = return P384
         | otherwise = fail "This doesn't fit in any prime known to this program"

choosePrime :: MonadFail m => (forall n . KnownNat n => PF.PrimeField n -> a) -> Integer -> m a
choosePrime f i = do
    which <- fitsIn i
    return $ case which of 
        P128 -> f (fromInteger i :: Secret128)
        P256 -> f (fromInteger i :: Secret256)
        P384 -> f (fromInteger i :: Secret384)

analyzeRecoveredShard :: forall n . KnownNat n => PF.PrimeField n -> (Integer, Either String T.Text, Either String T.Text)
analyzeRecoveredShard y = (int, mnem, text)
    where
    int = PF.toInteger y
    bytes = integerToBytes (SSSS.byteSize (Nothing :: Maybe (PF.PrimeField n))) int
    mnem = bytes >>= Keys.toMnemonic
    stripLeadingZeros = BS.dropWhile (== 0)
    text = bytes >>= (either (Left . show) Right . TE.decodeUtf8' . stripLeadingZeros)

