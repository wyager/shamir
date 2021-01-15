module Command where

import Data.Text (Text, unpack, pack)
import qualified Options.Applicative as Opt
import qualified Data.Attoparsec.Text as A
import qualified Data.ByteString as BS
import qualified Haskoin.Keys as Keys
import Control.Applicative (some, (<|>))
import qualified SSSS
import qualified Formats
import GHC.TypeLits (KnownNat)
import qualified Data.FiniteField.PrimeField as PF
import Data.Proxy (Proxy(..))
import Control.Monad (unless)
import PrimeField (WhichPrime(P128,P256,P384), Secret128, Secret256, Secret384)


data SplitCommand 
    = SplitHex Int Int Integer
    | SplitMnemonic Int Int BS.ByteString
    | SplitText Int Int Text
    deriving Show

data EncodedShard = EncodedShard Integer Text 
instance Show EncodedShard where
    show (EncodedShard x y) = "\"" ++ show x ++ ":" ++ unpack y ++ "\""

split :: SplitCommand -> IO ()
split cmd = case cmd of
        SplitHex m n i -> go m n i
        SplitMnemonic m n bs -> go m n (Formats.bsToInteger bs)
        SplitText m n txt -> go m n (Formats.textToInteger txt)
    where
    go m n i = do
        genShards <- case Formats.choosePrime split' i of
            Nothing -> fail "I don't have any primes large enough to fit your data"
            Just genShards -> return genShards
        shards <- genShards m n 
        mapM_ print shards
    split' :: forall n . KnownNat n => PF.PrimeField n -> Int -> Int -> IO [EncodedShard]
    split' i m n = do
        shards <- SSSS.encodeSecret i m n
        let untyped = fmap (fmap PF.toInteger) shards
            byteSize = SSSS.byteSize (Proxy :: Proxy (PF.PrimeField n))
            encode integer = do
                bytes <- Formats.integerToBytes byteSize integer
                Keys.toMnemonic bytes
        mnemonics <- either fail return $ mapM (encode . SSSS.shardY) untyped
        let output = zipWith EncodedShard (map SSSS.shardX untyped) mnemonics
        -- TODO: Add validation that any combination of keys can recover
        return output

validate :: MonadFail m => ((Integer, Either String Text, Either String Text) -> Bool) -> [EncodedShard] -> Int -> m ()
validate check allShards m = mapM_ restoreFrom (sublists m allShards)
    where
    sublists 0 _ = [[]]
    sublists n [] = []
    sublists n (x:xs) = sublists n xs ++ do
        subs <- sublists (n-1) xs
        return (x : subs)
    restoreFrom :: [EncodedShard] -> m ()
    restoreFrom = undefined
    decode :: EncodedShard -> Either String ShardArg
    decode = A.parseOnly shardArgParser . pack . show
    -- restoreFrom :: [EncodedShard] -> Either String ()
    -- restoreFrom shards = do
    --     decodedShards <- mapM decode shards
    --     secret <- recover

splitParser :: Opt.Parser SplitCommand
splitParser = Opt.hsubparser (int <> mnem <> text)
    where    
    int :: Opt.Mod Opt.CommandFields SplitCommand
    int = Opt.command "integer" (Opt.info (SplitHex <$> m <*> n <*> hexParser) (Opt.progDesc "Split a hex integer"))
    hexParser :: Opt.Parser Integer
    hexParser = Opt.argument readInt (Opt.help "A hex-encoded integer, like ABCDEF01 or 0xdeadbeef")
    readInt :: Opt.ReadM Integer
    readInt = Opt.str >>= either fail return . A.parseOnly (A.hexadecimal) 
    mnem :: Opt.Mod Opt.CommandFields SplitCommand
    mnem = Opt.command "mnemonic" (Opt.info (SplitMnemonic <$> m <*> n <*> mnemParser) (Opt.progDesc "Split a BIP39 mnemonic"))
    mnemParser :: Opt.Parser BS.ByteString
    mnemParser = Opt.argument readMnem (Opt.help "A BIP39 mnemonic, like \"act crew input ostritch group\"")
    readMnem :: Opt.ReadM BS.ByteString
    readMnem = Opt.str >>= either fail return . A.parseOnly mnemonicParser
    text :: Opt.Mod Opt.CommandFields SplitCommand
    text = Opt.command "text" (Opt.info (SplitText <$> m <*> n <*> textParser) (Opt.progDesc "Split a BIP39 mnemonic"))
    textParser :: Opt.Parser Text
    textParser = Opt.argument readText (Opt.help "Arbitrary utf-8 text (in quotes)")
    readText :: Opt.ReadM Text
    readText = Opt.str
    m :: Opt.Parser Int
    m = Opt.argument Opt.auto (Opt.help "m: the number of shards required to restore the secret")
    n :: Opt.Parser Int
    n = Opt.argument Opt.auto (Opt.help "n: the number of shards required to restore the secret")

data Command 
    = Split SplitCommand
    | Recover [ShardArg]
    deriving Show

data ShardArg = ShardArg {shardArgX :: Integer, shardArgY :: BS.ByteString}
    deriving Show



mnemonicParser :: A.Parser BS.ByteString 
mnemonicParser = do
    mnemonic <- A.takeWhile (A.inClass $ ['a'..'z'] ++ ['A'..'Z'] ++ " ")
    case Keys.fromMnemonic mnemonic of
        Left err -> fail $ "Could not parse shard words: " ++ err
        Right bytes -> return bytes

shardArgParser :: A.Parser ShardArg
shardArgParser = ShardArg <$> A.decimal <* ":" <*> mnemonicParser <* A.endOfInput

parseShardArg :: Opt.ReadM ShardArg
parseShardArg = do
    rawText <- Opt.str
    case A.parseOnly shardArgParser rawText of
        Left err -> fail err
        Right shard -> return shard

shardArg :: Opt.Parser ShardArg 
shardArg = Opt.argument parseShardArg (Opt.help "A shard, of the format \"4:act crew input ostritch group\"")

shardsArg :: Opt.Parser [ShardArg]
shardsArg = some shardArg

withValidatedShards :: MonadFail m => [ShardArg] -> (forall n . KnownNat n => [SSSS.Shard (PF.PrimeField n)] -> a) -> m a
withValidatedShards [] f = fail "Need at least one shard"
withValidatedShards shards@((ShardArg x yBytes):otherShards) f = do
    fitsIn <- Formats.bytesFitIn yBytes
    otherFitsIn <- mapM (Formats.bytesFitIn . shardArgY) otherShards
    unless (all (== fitsIn) otherFitsIn) $ fail "Error: Input shards have different sizes"
    return $ case fitsIn of
        PrimeField.P128 -> f $ map (format @Secret128) shards
        PrimeField.P256 -> f $ map (format @Secret256) shards
        PrimeField.P384 -> f $ map (format @Secret384) shards
    where
    format :: forall n . Num n => ShardArg -> SSSS.Shard n
    format (ShardArg x y)= SSSS.Shard (fromIntegral x :: n) (fromInteger (Formats.bsToInteger y) :: n)



run :: IO ()
run = do
    command <- Opt.execParser cmdParser
    case command of
        Split sc -> split sc
        Recover shards -> do
            let recover :: forall n . KnownNat n => [SSSS.Shard (PF.PrimeField n)] -> Either String (Integer, Either String Text, Either String Text)
                recover shards = Formats.analyzeRecoveredShard <$> SSSS.recoverSecret shards 
            (int, mnem, text) <- either fail return =<< withValidatedShards shards recover
            print int
            print mnem
            print text
            return ()
            -- print recovered
    where
    cmdParser :: Opt.ParserInfo Command
    cmdParser = Opt.info (Opt.helper <*> programOptions) 
        (Opt.fullDesc <> Opt.progDesc "SSSS tool")
    programOptions :: Opt.Parser Command
    programOptions = Opt.hsubparser (split <> recover)
        where
        split :: Opt.Mod Opt.CommandFields Command
        split = Opt.command "split" (Opt.info (Split <$> splitParser) (Opt.progDesc "Split data"))
        recover :: Opt.Mod Opt.CommandFields Command
        recover = Opt.command "recover" (Opt.info (Recover <$> shardsArg) (Opt.progDesc "Recover from shards"))




