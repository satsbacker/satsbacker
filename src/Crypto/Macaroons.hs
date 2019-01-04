{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Crypto.Macaroons where

import "cryptonite" Crypto.Hash.Algorithms
import "cryptonite" Crypto.MAC.HMAC
import Control.Applicative
import Data.Attoparsec.ByteString (Parser, parseOnly, endOfInput)
import Data.ByteArray (ByteArrayAccess)
import Data.ByteString (ByteString)
import Data.Char
import Data.List (intercalate, groupBy, foldl', sort, nub)
import Data.Serialize
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Traversable (for)
import Data.Word

import qualified Data.Attoparsec.ByteString.Char8 as A8
import qualified Data.ByteArray as Memory
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Hex
import qualified Data.ByteString.Base64.URL as B64
import qualified Data.ByteString.Char8 as B8

newtype Secret = Secret { getSecret :: ByteString }
    deriving Show

newtype CId = CId { getCId :: ByteString }
    deriving Show

newtype VId = VId { getVId :: ByteString }
    deriving Show

newtype Location = Location { getLocation :: ByteString }
    deriving Show

newtype Sig = Sig { getSig :: ByteString }
    deriving Show

-- | Main structure of a macaroon
data Macaroon = Macaroon
    { macaroonLocation   :: Location
    -- ^ Target location
    , macaroonIdentifier :: CId
    -- ^ Macaroon Identifier
    , macaroonCaveats    :: [Caveat]
    -- ^ List of caveats
    , macaroonSignature  :: Sig
    -- ^ Macaroon HMAC signature
    }

data Caveat = FirstParty CId
            | ThirdParty CId VId Location

cid :: Caveat -> CId
cid (FirstParty i) = i
cid (ThirdParty i _ _) = i

vid :: Caveat -> Maybe VId
vid FirstParty{}       = Nothing
vid (ThirdParty _ i _) = Just i

-- | show instance conforming to the @inspect@ "specification"
instance Show Macaroon where
    -- We use intercalate because unlines would add a trailing newline
    show (Macaroon (Location l) (CId i) c (Sig s)) =
        intercalate "\n" [ "location " ++ B8.unpack l
                         , "identifier " ++ B8.unpack i
                         , intercalate "\n" (map show c)
                         , "signature " ++ B8.unpack (Hex.encode s)
                         ]

instance Show Caveat where
    show (ThirdParty (CId c) (VId v) (Location l)) =
        unlines [ "cid " ++ B8.unpack c
                , "vid " ++ B8.unpack v
                , "cl " ++ B8.unpack l
                ]

    show (FirstParty (CId c)) =
        "cid " ++ B8.unpack c


toBytes :: ByteArrayAccess a => a -> ByteString
toBytes = Memory.convert

magicKey :: ByteString
magicKey = "macaroons-key-generator"

-- | Create a Macaroon from its key, identifier and location
create :: Secret -> CId -> Location -> Macaroon
create secret ident loc =
  Macaroon loc ident [] sig
  where
    sig        = Sig $ toBytes (hmac derivedKey (getCId ident) :: HMAC SHA256)
    derivedKey = toBytes (hmac magicKey (getSecret secret) :: HMAC SHA256)


caveatBytes :: Caveat -> ByteString
caveatBytes (FirstParty cid_) = getCId cid_
caveatBytes (ThirdParty cid_ vid_ _cl) =
    getVId vid_ `BS.append` getCId cid_


addCaveat :: Caveat -> Macaroon -> Macaroon
addCaveat c m =
    m { macaroonCaveats   = cavs ++ [c]
      , macaroonSignature = sig
      }
  where
    cavs    = macaroonCaveats m
    prevSig = getSig (macaroonSignature m)
    sig     = Sig $ toBytes (hmac prevSig (caveatBytes c) :: HMAC SHA256)




serializeCaveat :: Caveat -> Put
serializeCaveat c = do
    packetize "cid" (getCId (cid c))
    case c of
      ThirdParty _cid vid_ cl -> do packetize "vid" (getVId vid_)
                                    packetize "cl" (getLocation cl)
      _ -> mempty



-- | Serialize a macaroon in an URL-safe Base64 encoding
serialize :: Macaroon -> BS.ByteString
serialize Macaroon{..} = B8.filter (/= '=') . B64.encode . runPut $ do
    packetize "location" (getLocation macaroonLocation)
    packetize "identifier" (getCId macaroonIdentifier)
    mapM_ serializeCaveat macaroonCaveats
    packetize "signature" (getSig macaroonSignature)


packetize :: BS.ByteString -> BS.ByteString -> Put
packetize key dat = do
    let size = 4 + 2 + BS.length key + BS.length dat
    putByteString $ B8.map toLower . Hex.encode . encode $ (fromIntegral size :: Word16)
    putByteString key
    putByteString " "
    putByteString dat
    putByteString "\n"


-- | Deserialize a macaroon from a base64url-encoded ByteString
deserialize :: BS.ByteString -> Either String Macaroon
deserialize = parseOnly macaroonParser . B64.decodeLenient


macaroonParser :: Parser Macaroon
macaroonParser = do
    ps <- many packet <* endOfInput
    let (header,ps') = splitAt 2 ps

    (l, i) <- case header of
      [("location",l),("identifier", i)] -> pure (l, i)
      _                                  -> fail "missing macaroon header"

    let (cs,sig) = splitAt (length ps' - 1) ps'
        grouped  = groupBy splitCavs cs
        caveats  = map (mkCaveat l) grouped

    s <- case sig of
        [("signature", s)] -> pure s
        _                  -> fail "missing macaroon signature"

    return $ Macaroon (Location l) (CId i) caveats (Sig s)
  where
    mkCaveat _ [("cid",c),("vid",v),("cl",l)] =
        ThirdParty (CId c) (VId v) (Location l)
    mkCaveat _l [("cid",c)] = FirstParty (CId c)
    mkCaveat _ _ = error "Malformed caveat"
    splitCavs _ ("cid",_) = False
    splitCavs _ _ = True


packet :: Parser (BS.ByteString, BS.ByteString)
packet = do
    size <- A8.take 4
    case A8.parseOnly (A8.hexadecimal :: Parser Word16) size of
        Left e  -> fail e
        Right s -> do
            bs <- A8.take (fromIntegral (s - 4))
            let (key, dat) = B8.break (== ' ') bs
            return (key, B8.tail $ B8.init dat)


isValidSig :: Secret -> Macaroon -> Bool
isValidSig k Macaroon{..} =
  getSig macaroonSignature == foldl' hash sig macaroonCaveats
  where
    sig        = toBytes (hmac derivedKey (getCId macaroonIdentifier) :: HMAC SHA256)
    hash s c   = toBytes (hmac s (caveatBytes c) :: HMAC SHA256)
    derivedKey = toBytes (hmac magicKey (getSecret k) :: HMAC SHA256)

data VerificationResult = Verified
                        | Refused
                        | Unrelated
                        deriving (Show, Eq, Ord)

type Verifier f = Macaroon -> Caveat -> f VerificationResult


verify :: Applicative f => Secret -> [Verifier f] -> Macaroon -> f Bool
verify s verifiers m@Macaroon{..} =
  fmap (isValidSig s m &&) allVerifiersPass
  where
    indexed        = zip [(0 :: Int)..]
    indexedCaveats = indexed macaroonCaveats
    validResult    = take (length macaroonCaveats) (indexed (repeat Verified))

    verifierResults =
      fmap concat $
      for indexedCaveats $ \(i, c) ->
      for verifiers $ \verifier ->
        (,) <$> pure i <*> verifier m c
    validResults = nub . sort . filter ((==Verified) . snd)
    allVerifiersPass =
      flip fmap verifierResults $ \rs ->
        not (any ((==Refused) . snd) rs) && validResults rs == validResult


newCaveat :: ByteString -> Caveat
newCaveat cid_ = FirstParty (CId cid_)


expiresCaveat :: Int -> Caveat
expiresCaveat t = newCaveat ("time < " `BS.append` B8.pack (show t))


unixExpiresCaveat :: POSIXTime -> Caveat
unixExpiresCaveat = expiresCaveat . round


utcExpiresCaveat :: UTCTime -> Caveat
utcExpiresCaveat = unixExpiresCaveat . utcTimeToPOSIXSeconds


