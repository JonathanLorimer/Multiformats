module Multiaddr.Core where

import GHC.Word
import Data.DoubleWord
import Data.ByteString (ByteString)
import Data.Set
import Data.Text (Text)

type Multiaddr = Set AddrPart

type IPv4Addr = (Word8, Word8, Word8, Word8)
type IPv6Addr = ( Word16, Word16, Word16, Word16
                , Word16, Word16, Word16, Word16
                )

type Zone = Text
type UnixPath = [Text]

data AddrPart where
  -- Protocol based addresses
  IPv4 :: IPv4Addr -> AddrPart
  IPv6 ::  IPv6Addr -> AddrPart
  IPv6Zone ::  (IPv6Addr, Zone)  -> AddrPart

  -- Port number addresses
  TCP :: Word16 -> AddrPart
  UDP ::  Word16 -> AddrPart
  DCCP ::  Word16 -> AddrPart
  SCTP ::  Word16 -> AddrPart
  Onion ::  Word96 -> AddrPart
  -- Onion3 ::  Word256 -> AddrPart -- Need Word296 in haskell to make this happen

  -- Variable length addresses
  Unix ::  UnixPath -> AddrPart
  DNS ::  ByteString -> AddrPart
  DNS4 ::  ByteString -> AddrPart
  DNS6 ::  ByteString -> AddrPart
  DNSAddr ::  ByteString -> AddrPart
  P2P ::  ByteString -> AddrPart
  Garlic64 ::  ByteString -> AddrPart
  Garlic32 ::  ByteString -> AddrPart
  Memory ::  ByteString -> AddrPart

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

newtype Code = Code { unCode ::  Word16 }
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
toCode (DNSAddr _)= Code 56
toCode (SCTP _)= Code 132
toCode UDT = Code 301
toCode UTP = Code 302
toCode (Unix _) = Code 400
toCode (P2P _) = Code 421
toCode (Onion _) = Code 444
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
