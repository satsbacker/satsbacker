{-# LANGUAGE OverloadedStrings #-}

module Network.RPC.PeerState
    ( parsePeerState
    , PeerState(..)
    ) where

import Data.Maybe (maybe)
import Data.Text (Text)
import Data.Aeson

data PeerState = Uninitialized
               | AwaitingLockin
               | Cheated            -- ^ cheated onchain
               | Complete
               | Mutual             -- ^ mutual onchain close
               | Normal
               | Opening
               | OurUnilateral      -- ^ our onchain unilateral close
               | ShuttingDown
               | SignatureExchange
               | SpendSeen
               | TheirUnilateral
               deriving (Show, Eq, Ord)


parsePeerState :: Text -> Maybe PeerState
parsePeerState txt =
  case txt of
    "UNINITIALIZED"             -> Just Uninitialized
    "OPENINGD"                  -> Just Opening
    "CHANNELD_AWAITING_LOCKIN"  -> Just AwaitingLockin
    "CHANNELD_NORMAL"           -> Just Normal
    "CHANNELD_SHUTTING_DOWN"    -> Just ShuttingDown
    "CLOSINGD_SIGEXCHANGE"      -> Just SignatureExchange
    "CLOSINGD_COMPLETE"         -> Just Complete
    "FUNDING_SPEND_SEEN"        -> Just SpendSeen
    "ONCHAIND_CHEATED"          -> Just Cheated
    "ONCHAIND_THEIR_UNILATERAL" -> Just TheirUnilateral
    "ONCHAIND_OUR_UNILATERAL"   -> Just OurUnilateral
    "ONCHAIND_MUTUAL"           -> Just Mutual
    _                           -> Nothing


instance FromJSON PeerState where
  parseJSON (String s) =
    maybe (fail ("Could not parse PeerState " ++ show s))
          return (parsePeerState s)

  parseJSON v =
    fail ("PeerState not a string: " ++ show v)
