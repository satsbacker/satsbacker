
module Bitsbacker.UniqueId where

import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Base58
import Data.ByteString (ByteString)

newtype UniqueId = UniqueId { getUniqueId :: UUID }

instance Show UniqueId where
  show invoiceId = BS.unpack (encodeUniqueId invoiceId)

encodeUniqueId :: UniqueId -> ByteString
encodeUniqueId (UniqueId uuid) =
  encodeBase58 bitcoinAlphabet uuidBytes
  where
    uuidBytes = LBS.toStrict (UUID.toByteString uuid)

newUniqueId :: IO UniqueId
newUniqueId = fmap UniqueId UUID.nextRandom

-- λ> replicateM 5 newInvoiceId >>= mapM_ print
-- CuLhGEF2ChmDS5mW2WG7qB
-- UpgygGB5GecUcuYmqRKki9
-- LrHtTnwZra3Udr6uzBXJTC
-- 42f2g5Rw5NBty2KrHmzq4n
-- VnFPcHXKMRzaLrfuSp9CQm

decodeUniqueId :: ByteString -> Maybe UUID
decodeUniqueId bs =
  UUID.fromByteString =<< fmap LBS.fromStrict (decodeBase58 bitcoinAlphabet bs)
