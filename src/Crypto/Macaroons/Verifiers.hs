{-# LANGUAGE OverloadedStrings #-}

module Crypto.Macaroons.Verifiers
    ( compareVerifier
    , unixTimeVerifier
    , equalVerifier
    , equalParser
    ) where

import Data.ByteString (ByteString)
import Data.Bool (bool)
import Data.Functor ((<&>))
import Data.Time.Clock.POSIX
import Data.Attoparsec.ByteString.Char8


import Crypto.Macaroons

orderingParser :: Parser Ordering
orderingParser = do
  c <- satisfy (`elem` ("<>=" :: String))
  return $ case c of
             '<' -> LT
             '>' -> GT
             '=' -> EQ
             _   -> error "impossible result in orderingParser"

-- | parses expressions of the kind:
--     "key compare_op value"
opParser :: ByteString -> Parser b -> Parser a -> Parser (a, b)
opParser key parseOp value = do
  _   <- string key
  _   <- char ' '
  op  <- parseOp
  _   <- char ' '
  val <- value
  return (val, op)


-- key < val
-- key > val
-- key = val
ordParser :: ByteString -> Parser a -> Parser (a, Ordering)
ordParser key = opParser key orderingParser


-- key = val
eqParser :: ByteString -> Parser b -> Parser b
eqParser key val = fmap fst (opParser key (char '=') val)

firstPartyV :: Applicative f
            => Caveat -> (CId -> f VerificationResult) -> f VerificationResult
firstPartyV c = firstParty c (pure Unrelated)


firstParty :: Caveat -> p -> (CId -> p) -> p
firstParty c g f =
    case c of
      ThirdParty{}    -> g
      FirstParty cid_ -> f cid_


compareVerifier :: (Applicative f, Ord a)
                => ByteString -> f a -> Parser a -> Caveat -> f VerificationResult
compareVerifier key lval valParser c  =
  firstPartyV c $ \(CId cid_) ->
    case parseOnly (ordParser key valParser) cid_ of
      Left _err -> pure Unrelated
      Right (rval, ord) -> fmap toVerified $
        case ord of
          LT -> lval <&> (< rval)
          GT -> lval <&> (> rval)
          EQ -> lval <&> (== rval)


equalParser
  :: ByteString
     -> Parser a
     -> Caveat
     -> Either String a
equalParser key valParser c =
  firstParty c (Left "no parse") $ \(CId cid_) ->
    parseOnly (eqParser key valParser) cid_


equalVerifier :: (Applicative f, Eq a)
              => ByteString
              -> f a
              -> Parser a
              -> Caveat
              -> f VerificationResult
equalVerifier key lval valParser c =
  case equalParser key valParser c of
      Left _err  -> pure Unrelated
      Right rval -> lval <&> toVerified . (==rval)


toVerified :: Bool -> VerificationResult
toVerified = bool Refused Verified


unixTime :: IO Int
unixTime = fmap round getPOSIXTime


unixTimeVerifier :: Caveat -> IO VerificationResult
unixTimeVerifier = compareVerifier "time" unixTime decimal


