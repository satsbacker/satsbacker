
module Bitcoin.Address where

import qualified Data.Text as T

data AddrType = Bech32
              | P2SHSegwit
              deriving (Eq)

data Address = Address { address :: Text, addrType :: AddrType }

instance Show AddrType where
  show Bech32 = "bech32"
  show P2SHSegwit = "p2sh-segwit"
