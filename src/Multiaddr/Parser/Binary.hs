{-# LANGUAGE BangPatterns #-}

module Multiaddr.Parser.Binary where

import Data.Bits
import Data.Serialize.Get
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Multiaddr.Core
import Relude hiding (many)

-- import GHC.Word (Word8, Word16)

newtype VariableLength = VariableLength {unVarLength :: Int}

getVarLength :: Get VariableLength
getVarLength = VariableLength <$> getVarInt

getVarInt :: (Integral a, Bits a) => Get a
getVarInt = go 0 0
  where
    go n !val = do
      b <- getWord8
      if testBit b 7
        then go (n + 7) (val .|. (fromIntegral (b .&. 0x7F) `shiftL` n))
        else return $! val .|. (fromIntegral b `shiftL` n)

ipv4Addr :: Get IPv4Addr
ipv4Addr =
  (,,,)
    <$> getWord8
    <*> getWord8
    <*> getWord8
    <*> getWord8

ipv6Addr :: Get IPv6Addr
ipv6Addr =
  (,,,,,,,)
    <$> getWord16be
    <*> getWord16be
    <*> getWord16be
    <*> getWord16be
    <*> getWord16be
    <*> getWord16be
    <*> getWord16be
    <*> getWord16be

ipv6ZoneAddr :: VariableLength -> Get (IPv6Addr, Zone)
ipv6ZoneAddr VariableLength {..} =
  let zoneLength = unVarLength - 128
   in liftA2 (,) ipv6Addr (E.decodeUtf8 <$> getByteString zoneLength)

unixPathAddr :: VariableLength -> Get UnixPath
unixPathAddr VariableLength {..} = T.splitOn "/" . E.decodeUtf8 <$> getByteString unVarLength

onionAddr :: Get OnionAddr
onionAddr = E.decodeUtf8 <$> getByteString 96

-- $> import Data.Serialize.Get

-- $> runGet (getByteString 16) "12345678901234567"

-- onion3Addr :: Get Onion3Addr
-- onion3Addr = undefined

-- mkPart :: Get (a -> AddrPart) -> Get a -> Get AddrPart
-- mkPart ctorP addrP = undefined

-- ipv4Part :: Get AddrPart
-- ipv4Part = undefined

-- ipv6Part :: Get AddrPart
-- ipv6Part = undefined

-- ipv6ZonePart :: Get AddrPart
-- ipv6ZonePart = undefined

-- tcpPart :: Get AddrPart
-- tcpPart = undefined

-- udpPart :: Get AddrPart
-- udpPart = undefined

-- dccpPart :: Get AddrPart
-- dccpPart = undefined

-- sctpPart :: Get AddrPart
-- sctpPart = undefined

-- onionPart :: Get AddrPart
-- onionPart = undefined

-- onion3Part :: Get AddrPart
-- onion3Part = undefined

-- unixPart :: Get AddrPart
-- unixPart = undefined

-- dnsPart :: Get AddrPart
-- dnsPart = undefined

-- dns4Part :: Get AddrPart
-- dns4Part = undefined

-- dns6Part :: Get AddrPart
-- dns6Part = undefined

-- dnsAddrPart :: Get AddrPart
-- dnsAddrPart = undefined

-- p2pPart :: Get AddrPart
-- p2pPart = undefined

-- garlic64Part :: Get AddrPart
-- garlic64Part = undefined

-- garlic32Part :: Get AddrPart
-- garlic32Part = undefined

-- memoryPart :: Get AddrPart
-- memoryPart = undefined

-- udtPart :: Get AddrPart
-- udtPart = undefined

-- utpPart :: Get AddrPart
-- utpPart = undefined

-- quicPart :: Get AddrPart
-- quicPart = undefined

-- httpPart :: Get AddrPart
-- httpPart = undefined

-- httpsPart :: Get AddrPart
-- httpsPart = undefined

-- wsPart :: Get AddrPart
-- wsPart = undefined

-- wssPart :: Get AddrPart
-- wssPart = undefined

-- p2pWebsocketStarPart :: Get AddrPart
-- p2pWebsocketStarPart = undefined

-- p2pStardustPart :: Get AddrPart
-- p2pStardustPart = undefined

-- p2pWebRTCStarPart :: Get AddrPart
-- p2pWebRTCStarPart = undefined

-- p2pWebRTCDirectPart :: Get AddrPart
-- p2pWebRTCDirectPart = undefined

-- p2pCircuitPart :: Get AddrPart
-- p2pCircuitPart = undefined
