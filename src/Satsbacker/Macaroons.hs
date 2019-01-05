{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}


module Satsbacker.Macaroons where

import Crypto.Macaroons
import Crypto.Macaroons.Verifiers
import Data.Attoparsec.ByteString.Char8

import Data.Time.Clock.POSIX
import Data.Monoid (First(..))
import Data.Text.Encoding (encodeUtf8)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8

import Satsbacker.Data.Email (Email(..))
import Satsbacker.Data.User (UserId(..))
import Satsbacker.Entropy


-- structural keys themselves are always valid
structuralVerifier :: [CId] -> Caveat -> VerificationResult
structuralVerifier items c =
  case c of
    ThirdParty{} -> Unrelated
    FirstParty (CId cid_)
      | matches cid_ -> Verified
      | otherwise    -> Unrelated
  where
    matches cid_ = any ((`BS.isPrefixOf` cid_) . getCId) items



verifyProvidedData :: Caveat -> VerificationResult
verifyProvidedData = structuralVerifier [CId "signature "]


satsVerifiers :: UserId -> [Macaroon -> Caveat -> IO VerificationResult]
satsVerifiers (UserId userId) =
  [ const unixTimeVerifier
  , const (userVerifier userId)
  ]



daysToSeconds :: Int -> Int
daysToSeconds = (86400*)


expiringMacaroon :: Secret -> IO Macaroon
expiringMacaroon secret = do
  t      <- fmap round getPOSIXTime
  rnonce <- randInt
  let nonce    = CId (B8.pack (show rnonce))
      macaroon = create secret nonce (Location "satsbacker.com")
      expireAt = t + daysToSeconds 14
  return (macaroon `addCaveat` expiresCaveat expireAt)


userCaveat :: UserId -> Caveat
userCaveat (UserId userId) =
    newCaveat ("user = " `BS.append` B8.pack (show userId))


sessionMacaroon :: Secret -> UserId -> IO Macaroon
sessionMacaroon secret userId = do
  m <- expiringMacaroon secret
  return (m `addCaveat` userCaveat userId)


emailCaveat :: Email -> Caveat
emailCaveat (Email email) = newCaveat ("email = " `BS.append` encodeUtf8 email)


mintEmailMacaroon :: Secret -> Email -> UserId -> IO Macaroon
mintEmailMacaroon secret email userId = do
  m <- expiringMacaroon secret
  return (m `addCaveat` emailCaveat email
            `addCaveat` userCaveat userId)


emailVerifier :: Applicative f => Email -> Caveat -> f VerificationResult
emailVerifier (Email email) =
    equalVerifier "email" (pure (encodeUtf8 email)) takeByteString


verifyEmailMacaroon :: Secret -> Email -> UserId -> Macaroon -> IO Bool
verifyEmailMacaroon secret email userId m =
    let
        verifiers = const (emailVerifier email) : satsVerifiers userId
    in verify secret verifiers m


userVerifier :: (Applicative f)
             => Int -> Caveat -> f VerificationResult
userVerifier uid = equalVerifier "user" (pure uid) decimal


parseUserId :: Caveat -> Maybe UserId
parseUserId caveat =
  case equalParser "user" decimal caveat of
    Left  err -> Nothing
    Right uid -> Just (UserId uid)


first :: Foldable t => (a -> Maybe b) -> t a -> Maybe b
first fn = getFirst . foldMap (First . fn)


macaroonUserId :: Macaroon -> Maybe UserId
macaroonUserId Macaroon{..} =
    first parseUserId macaroonCaveats


test :: IO Macaroon
test = mintEmailMacaroon (Secret "secret") (Email "jb55@jb55.com") (UserId 1)
