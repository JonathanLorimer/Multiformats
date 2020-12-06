module Multiaddr.Parser where

import Text.Megaparsec
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as C
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Control.Monad (void, guard)
import Multiaddr.Core
import GHC.Word (Word8, Word16)

type MultiaddrHumanParser = Parsec Void Text

delim :: MultiaddrHumanParser Char
delim = C.char '/'

ip4 :: MultiaddrHumanParser (IPv4Addr -> AddrPart)
ip4 = IPv4 <$ C.string "ip4"

ip6 :: MultiaddrHumanParser (IPv6Addr -> AddrPart)
ip6 = IPv6 <$ C.string "ip6"

word :: Num a => Integer -> MultiaddrHumanParser Integer -> MultiaddrHumanParser a
word limit subParser = do
  num :: Integer <- subParser
  guard (0 <= num && num <= limit)
  pure $ fromInteger num

word8 :: MultiaddrHumanParser Integer -> MultiaddrHumanParser Word8
word8 = word 255

decWord8 :: MultiaddrHumanParser Word8
decWord8 = word8 C.decimal

word16 :: MultiaddrHumanParser Integer -> MultiaddrHumanParser Word16
word16 = word 65535

decWord16 :: MultiaddrHumanParser Word16
decWord16 = word16 C.decimal

hexWord16 :: MultiaddrHumanParser Word16
hexWord16 = word16 C.hexadecimal

ipv4Addr :: MultiaddrHumanParser IPv4Addr
ipv4Addr =
  (,,,)
    <$> decWord8
    <*> (ipDelim >> decWord8)
    <*> (ipDelim >> decWord8)
    <*> (ipDelim >> decWord8)
    where
      ipDelim = C.char '.'

ipv6Addr :: MultiaddrHumanParser IPv6Addr
ipv6Addr = do
  beginning <- sepEndBy hexWord16 ipDelim
  -- Handles double colon scenario
  void $ optional ipDelim
  end <- sepBy hexWord16 ipDelim

  let len = length beginning + length end
  guard (len <= 8)

  case beginning <> replicate (8 - len) 0 <> end of
    [a,b,c,d,e,f,g,h] -> pure (a,b,c,d,e,f,g,h)
    _ -> fail "ipv6 address of incorrect length"

  where
    ipDelim :: MultiaddrHumanParser Char
    ipDelim = C.char ':'

ipv6ZoneAddr :: MultiaddrHumanParser (IPv6Addr, Zone)
ipv6ZoneAddr = do
  ipv6Address <- ipv6Addr
  void $ C.char '%'
  zone <- many C.alphaNumChar
  pure (ipv6Address, T.pack zone)

unixPathAddr :: MultiaddrHumanParser UnixPath
unixPathAddr = fmap T.pack <$> (pathDelim >> sepBy (many (anySingleBut '/')) pathDelim)
    where
      pathDelim :: MultiaddrHumanParser Char
      pathDelim = C.char '/'

-- t1 = "2001:0db8:1111:000a:00b0:0000:0000:0200" :: Text
-- t2 = "2001:db8:1111:a:b0::200" :: Text
-- t3 = "2001:0db8:0000:0000:abcd:0000:0000:1234" :: Text
-- t4 = "2001:db8::abcd:0:0:1234" :: Text
-- t5 = "2001:0db8:aaaa:0001:0000:0000:0000:0100" :: Text
-- t6 = "2001:db8:aaaa:1::100" :: Text
-- t7 = "2001:0db8:aaaa:0001:0000:0000:0000:0200" :: Text
-- t8 = "2001:db8:aaaa:1::200%eth2" :: Text
--
--

---- $> import Text.Megaparsec
--
---- $> runParser unixPathAddr "hello" "/path/to/my/dirs/file.hs"

mkPart :: MultiaddrHumanParser (a -> AddrPart) -> MultiaddrHumanParser a -> MultiaddrHumanParser AddrPart
mkPart ctorP addrP = do
  void delim
  constructor <- ctorP
  void $ optional delim
  address <- addrP
  pure $ constructor address

mkAddresslessPart :: AddrPart -> Text -> MultiaddrHumanParser AddrPart
mkAddresslessPart ctor str = mkPart (const ctor <$ C.string str) (pure ())

ipv4Part :: MultiaddrHumanParser AddrPart
ipv4Part = mkPart ip4 ipv4Addr

ipv6Part :: MultiaddrHumanParser AddrPart
ipv6Part = mkPart ip6 ipv6Addr

ipv6ZonePart :: MultiaddrHumanParser AddrPart
ipv6ZonePart = mkPart (IPv6Zone <$ C.string "ip6zone") ipv6ZoneAddr

tcpPart :: MultiaddrHumanParser AddrPart
tcpPart = mkPart (TCP <$ C.string "tcp") decWord16

udpPart :: MultiaddrHumanParser AddrPart
udpPart = mkPart (UDP <$ C.string "udp") decWord16

dccpPart :: MultiaddrHumanParser AddrPart
dccpPart = mkPart (DCCP <$ C.string "dccp") decWord16

sctpPart :: MultiaddrHumanParser AddrPart
sctpPart = mkPart (SCTP <$ C.string "sctp") decWord16

-- TODO(jonathan): look into how onion addresses are encoded and implement
onionPart :: MultiaddrHumanParser AddrPart
onionPart = undefined

unixPart :: MultiaddrHumanParser AddrPart
unixPart = mkPart (Unix <$ C.string "unix") unixPathAddr

dnsPart :: MultiaddrHumanParser AddrPart
dnsPart = undefined

dns4Part :: MultiaddrHumanParser AddrPart
dns4Part = undefined

dns6Part :: MultiaddrHumanParser AddrPart
dns6Part = undefined

dns6AddrPart :: MultiaddrHumanParser AddrPart
dns6AddrPart = undefined

p2pPart :: MultiaddrHumanParser AddrPart
p2pPart = mkPart (P2P <$ (C.string "p2p" <|> C.string "ipfs")) (pure ("" :: ByteString))

garlic64Part :: MultiaddrHumanParser AddrPart
garlic64Part = undefined

garlic32Part :: MultiaddrHumanParser AddrPart
garlic32Part = undefined

memoryPart :: MultiaddrHumanParser AddrPart
memoryPart = undefined

udtPart :: MultiaddrHumanParser AddrPart
udtPart = mkAddresslessPart UDT "udt"

utpPart :: MultiaddrHumanParser AddrPart
utpPart = mkAddresslessPart UTP "utp"

quicPart :: MultiaddrHumanParser AddrPart
quicPart = mkAddresslessPart QUIC "quic"

httpPart :: MultiaddrHumanParser AddrPart
httpPart = mkAddresslessPart HTTP "http"

httpsPart :: MultiaddrHumanParser AddrPart
httpsPart = mkAddresslessPart HTTPS "https"

wsPart :: MultiaddrHumanParser AddrPart
wsPart = mkAddresslessPart WS "ws"

wssPart :: MultiaddrHumanParser AddrPart
wssPart = mkAddresslessPart WSS "wss"

p2pWebsocketStarPart :: MultiaddrHumanParser AddrPart
p2pWebsocketStarPart = mkAddresslessPart P2PWebsocketStar "p2p-websocket-star"

p2pStardustPart :: MultiaddrHumanParser AddrPart
p2pStardustPart = mkAddresslessPart P2PStardust "p2p-stardust"

p2pWebRTCStarPart :: MultiaddrHumanParser AddrPart
p2pWebRTCStarPart = mkAddresslessPart P2PStardust "p2p-webrtc-star"

p2pWebRTCDirectPart :: MultiaddrHumanParser AddrPart
p2pWebRTCDirectPart = mkAddresslessPart P2PStardust "p2p-webrtc-direct"

p2pCircuitPart :: MultiaddrHumanParser AddrPart
p2pCircuitPart = mkAddresslessPart P2PStardust "p2p-circuit"
