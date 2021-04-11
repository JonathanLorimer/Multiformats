module Multiaddr.Core where

import Data.Text (Text, length)
import Relude hiding (length)

type Multiaddr = Set AddrPart

type IPv4Addr = (Word8, Word8, Word8, Word8)

type IPv6Addr =
  ( Word16,
    Word16,
    Word16,
    Word16,
    Word16,
    Word16,
    Word16,
    Word16
  )

newtype Text16 = Text16 {unText16 :: Text} deriving (Eq, Ord, Show)

text16 :: Text -> Maybe Text16
text16 t = if length t == 16 then Just (Text16 t) else Nothing

newtype Text52 = Text52 {unText52 :: Text} deriving (Eq, Ord, Show)

text52 :: Text -> Maybe Text52
text52 t = if length t == 52 then Just (Text52 t) else Nothing

data OnionAddr
  = OnionAddr
      { onionPath :: Text16,
        onionPort :: Port
      }
  deriving (Eq, Ord, Show)

data Onion3Addr
  = Onion3Addr
      { onion3Path :: Text52,
        onion3Port :: Port
      }
  deriving (Eq, Ord, Show)

type Port = Word16

type Zone = Text

type UnixPath = [Text]

data AddrPart where
  -- Protocol based addresses
  IPv4 :: IPv4Addr -> AddrPart
  IPv6 :: IPv6Addr -> AddrPart
  IPv6Zone :: (IPv6Addr, Zone) -> AddrPart
  -- Port number addresses
  TCP :: Port -> AddrPart
  UDP :: Port -> AddrPart
  DCCP :: Port -> AddrPart
  SCTP :: Port -> AddrPart
  -- Variable length addresses
  Onion :: OnionAddr -> AddrPart
  Onion3 :: Onion3Addr -> AddrPart -- Need Word296 in haskell to make this happen
  Unix :: UnixPath -> AddrPart
  DNS :: Text -> AddrPart
  DNS4 :: Text -> AddrPart
  DNS6 :: Text -> AddrPart
  DNSAddr :: Text -> AddrPart
  P2P :: Text -> AddrPart
  Garlic64 :: Text -> AddrPart
  Garlic32 :: Text -> AddrPart
  Memory :: Text -> AddrPart
  -- Addressless Parts
  UDT :: AddrPart
  UTP :: AddrPart
  QUIC :: AddrPart
  HTTP :: AddrPart
  HTTPS :: AddrPart
  WS :: AddrPart
  WSS :: AddrPart
  P2PWebsocketStar :: AddrPart
  P2PStardust :: AddrPart
  P2PWebRTCStar :: AddrPart
  P2PWebRTCDirect :: AddrPart
  P2PCircuit :: AddrPart
  deriving (Eq, Ord)

newtype Code = Code {unCode :: Word16}
  deriving (Show)

toCode :: AddrPart -> Code
toCode (IPv4 _) = Code 4
toCode (TCP _) = Code 6
toCode (UDP _) = Code 273
toCode (DCCP _) = Code 33
toCode (IPv6 _) = Code 41
toCode (IPv6Zone _) = Code 42
toCode (DNS _) = Code 53
toCode (DNS4 _) = Code 54
toCode (DNS6 _) = Code 55
toCode (DNSAddr _) = Code 56
toCode (SCTP _) = Code 132
toCode UDT = Code 301
toCode UTP = Code 302
toCode (Unix _) = Code 400
toCode (P2P _) = Code 421
toCode (Onion _) = Code 444
toCode (Onion3 _) = Code 445
toCode (Garlic32 _) = Code 446
toCode (Garlic64 _) = Code 447
toCode QUIC = Code 460
toCode HTTP = Code 480
toCode HTTPS = Code 443
toCode WS = Code 477
toCode WSS = Code 478
toCode P2PWebsocketStar = Code 479
toCode P2PStardust = Code 277
toCode P2PWebRTCStar = Code 275
toCode P2PWebRTCDirect = Code 276
toCode P2PCircuit = Code 290
toCode (Memory _) = Code 777
