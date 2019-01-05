{-# LANGUAGE OverloadedStrings #-}


module Satsbacker.Macaroons where

import Crypto.Macaroons
import Crypto.Macaroons.Verifiers
import Data.Attoparsec.ByteString.Char8 (decimal)

import Data.Time.Clock.POSIX

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8

import Satsbacker.Data.User (UserId(..))


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
  , const (accountVerifier userId)
  ]



test :: Macaroon
Right test =
  deserialize "MDAxMWxvY2F0aW9uIGxvYwowMDE1aWRlbnRpZmllciBpZGVudAowMDJmc2lnbmF0dXJlIHN_aUgJ2oheHBdPFOpj6Q2odn-0ce4D7YGdZObe3srTCg"


daysToSeconds :: Int -> Int
daysToSeconds = (86400*)


expiringMacaroon :: Secret -> IO Macaroon
expiringMacaroon secret = do
  t <- fmap round getPOSIXTime
  let macaroon = create secret (CId "0") (Location "satsbacker.com")
      expireAt = t + daysToSeconds 14
  return (addCaveat (expiresCaveat expireAt) macaroon)


accountCaveat :: UserId -> Caveat
accountCaveat (UserId userId) =
    newCaveat ("account = " `BS.append` B8.pack (show userId))


sessionMacaroon :: Secret -> UserId -> IO Macaroon
sessionMacaroon secret userId = do
  m <- expiringMacaroon secret
  return $ addCaveat (accountCaveat userId) m


satsVerify :: UserId -> Macaroon -> IO Bool
satsVerify userId = verify (Secret "secret") (satsVerifiers userId)


accountVerifier :: (Applicative f)
                => Int -> Caveat -> f VerificationResult
accountVerifier uid = equalVerifier "account" (pure uid) decimal
