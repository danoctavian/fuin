{-# LANGUAGE ForeignFunctionInterface #-} 
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Socks5Proxy where
  
import Control.Monad.IO.Class
import Data.ByteString.Char8 as Ch8
import Data.ByteString as DatB
import System.FilePath.Posix
import System.Environment
import System.Directory
import Network hiding (accept)
import Network.Socket
import Network.Socket.ByteString.Lazy as NBSL
import Control.Concurrent
import Data.List as L
import Control.Monad.Maybe
import Control.Monad.Error as CME
import Control.Monad.Error.Class
import Control.Monad as CM
import qualified Data.ByteString.Lazy as BSL
import Data.Attoparsec 
import Data.Attoparsec.Char8 as DAC
import Data.Attoparsec.Binary
import Network.Socket.ByteString as NBS
import Data.Char
import Data.String
import Data.Maybe
import System.IO
import System.IO.Strict as SysIOStrict
import Network.Socket.Splice
import Data.Word
import Prelude as P
import Data.Conduit.Network
import Data.Conduit
import Data.Conduit.List as DCL
import Control.Monad.Trans.Resource
import System.Log.Logger
import System.Log.Handler.Syslog
import System.Log.Handler.Simple
import Data.List.Split as DLS

--import WireProtocol

import Utils

data PacketHandlers = PacketHandlers {incoming :: PacketHandler, outgoing :: PacketHandler}
type PacketHandler = (MonadIO m) => ByteString -> m (ByteString)

-- client sock -> remote server sock -> handlers
type InitHook = (MonadIO m) => SockAddr -> SockAddr -> m PacketHandlers
type GetConn = (Data.String.IsString e, MonadIO m, MonadError e m, Functor m) => Socket -> m Connection
data Config = Config {getPort :: PortID, initHook :: InitHook, getConn :: GetConn}

type Method = Char
data ClientHandshake = CH Char [Method]
  deriving Show

data CMD = CONNECT | BIND | UDPASSOCIATE -- command
  deriving Show
data ATYP = IPV4 | DOMAINNAME | IPV6 -- address type
  deriving (Show, Eq)
data Connection = Connection CMD ATYP SockAddr -- cmd, atyp, address, port
  deriving Show

msgSize = 1024 -- magical? may need more
-- for bittorrent 20 + 1024 * 1024
packSize = 20 + 1024 * 1024

cmdCodes = ['\1', '\2', '\3']
atypCodes = ['\1', '\3', '\4']
toCMD c = lookup c $ L.zip cmdCodes [CONNECT, BIND, UDPASSOCIATE]
toATYP a = lookup a $ L.zip atypCodes [IPV4, DOMAINNAME, IPV6]


runServer :: (MonadIO io) => Config -> io ()
runServer config = liftIO $ withSocketsDo $  do
  liftIO $ P.putStrLn "Starting server"
  sock <- listenOn $ (Socks5Proxy.getPort config)
  loop config sock


justPrint :: (Show s, MonadIO m) => String -> s -> m s 
justPrint  flag bs = do
  liftIO $ P.putStrLn flag
  liftIO $ P.putStrLn $ show bs
  return bs

printerInit _ _ = return $ PacketHandlers (justPrint "INCOMING!!!") (justPrint "OUTGOING!!!") 
noOpInit _ _ = return $ PacketHandlers return return



loop config serverSock = do
  (clientSock, addr) <- accept serverSock
  forkIO $ handleReq config (clientSock, addr)
  loop config serverSock


doSocksHandshake :: GetConn
doSocksHandshake conn = do
  hs <- getMessage conn parseHandshake (isValidHandshake, "Invalid handshake")
  liftIO $ NBS.sendAll conn "\5\0"
  connRequest <- getMessage conn parseConnectionReq (isValidConnectionReq, "Invalid connection request")
  liftIO $ NBS.send conn "\5\0\0\1\0\0 01c" --this is magical as fuck...
  return connRequest

handleReq config (sock, addr) = do
  eitherConnRequest <- runErrorT $ (getConn config $ sock)
  case eitherConnRequest of
    Left err -> P.putStrLn err 
    Right c -> handleConnection c (sock, addr) config
  -- TODO: add code for catching exceptions when connections is broken
  P.putStrLn "done showing"
  Network.Socket.sClose sock

handleConnection (Connection cmd atyp serverSockAddr) (clientSock, clientAddr) config = do
  P.putStrLn "handling connection"
  {-}
  let addr = getStrAddress atyp bsAddr
  addrInfos <- getAddrInfo Nothing (Just addr) (Just $ show $ sockAddrPort ) 
  let serverAddr = L.head addrInfos -- TODO: issue here might be no head
  -}
  serverSock <- socket (toAddressFamily atyp) Stream defaultProtocol --
  {- fuck me if i understand why they did this faggotry with reversing the endianness 
    of whatever i write in port number; but here it goes but whatever gets here is endianness adjusted
    essentially the PortNumber thing assumes what is given to it is big endian (network byte order)
    and turns it into little endian (host byte order) -}
  P.putStrLn $ "address is " ++ (show serverSockAddr)
  connect serverSock serverSockAddr
  handlers <- (initHook config) clientAddr serverSockAddr 
{-
  -- fast proxying solution. problem is it does not allow place a hook
  -- and tamper with the packets
  -- using conduit atm -it's prolly good nough
  clientH <- socketToHandle clientSock ReadWriteMode
  serverH <- socketToHandle serverSock ReadWriteMode
  let serverSide = (serverSock, Just serverH)
  let clientSide = (clientSock, Just clientH)
  void . forkIO   $ splice msgSize serverSide clientSide 
  splice msgSize clientSide serverSide 
-}
  forkIO $ forwardPackets clientSock serverSock (outgoing handlers)
  forwardPackets serverSock clientSock (incoming handlers)
  return ()

getMessage conn parse (validate, errMsg) = do
  tcpMsg <- liftIO $ NBS.recv conn msgSize
  handshakeParse <- return (parseOnly parse tcpMsg)
  hs <- case handshakeParse of
          Left err -> CME.throwError "failed parse "
          Right hs -> if (validate hs) then return hs else CME.throwError errMsg 
  return hs

-- this is wrong - when connection is broken what happens to this loop?
-- now it just goes on forever...


safeConduitSock :: (MonadResource m) => (Socket -> ConduitM i o m r) -> Socket -> ConduitM i o m r
safeConduitSock conduit socket  = bracketP ((return socket) :: IO (Socket)) Network.Socket.sClose conduit

-- TODO: implement below code using safe resource closing using above; now ihnfc what happens to the socket
forwardPackets :: (MonadIO io) => Socket ->
                  Socket -> (ByteString -> io ByteString)
                  -> io ()
forwardPackets src dst trans = do
  r <- (sourceSocket src) =$ (DCL.mapM trans) $$ (sinkSocket dst)
  return ()

-- only support TCP connect. fail otherwise
parseConnectionReq :: Parser Connection
parseConnectionReq = do
  verB <- anyChar
  cmd <- fmap fromJust $ satisfyWith (toCMD . word8ToChar) toBool
  anyChar
  atyp <- fmap fromJust $ satisfyWith (toATYP . word8ToChar) toBool
  address <- case atyp of
    IPV4 -> DAC.take 4
    DOMAINNAME -> do {size <- fmap ord $ anyChar ; DAC.take size}
    IPV6 -> DAC.take 16
  port <- anyWord16be
  return (Connection CONNECT atyp $ fromJust $ toSockAddress atyp address $ toggleEndianW16 port)

isValidConnectionReq :: Connection -> Bool
isValidConnectionReq _ = True
  
parseHandshake :: Parser ClientHandshake
parseHandshake = do
  verB <- anyChar
  methCount <- fmap ord anyChar
  methods <- replicateM methCount anyChar
  return $ CH verB methods

isValidHandshake :: ClientHandshake -> Bool
isValidHandshake (CH ver methods) = ver == (chr 5) && L.elem '\0' methods

-- data type convertors

toStrAddress :: ATYP -> ByteString -> String
toStrAddress DOMAINNAME = Ch8.unpack
toStrAddress IPV4 = toIPAdress
toStrAddress IPV6 = toIPAdress

getStrAddress :: ATYP -> Either ByteString String -> String
getStrAddress atyp (Left bs) = toStrAddress atyp bs
getStrAddress _ (Right s) = s

toIPAdress :: ByteString -> String
toIPAdress bs = (L.foldl (++) "") . (inbetween ".")  . (L.map $ show . ord) . Ch8.unpack $ bs


-- TODO: fields for IPV6 namely flow and ScopeID are from my ass; need to figure out wtf
toSockAddress :: ATYP -> ByteString -> Word16 -> Maybe SockAddr
toSockAddress atyp bs port
  | atyp == IPV4 = toHostAddress bs >>= (\a -> Just $ SockAddrInet (portNumberle port) a)
  | atyp == IPV6 = toHostAddress6 bs >>= (\a -> Just $ SockAddrInet6 (portNumberle port)
                                                (0 :: Word32) a (0 :: Word32))
  | atyp == DOMAINNAME = undefined



word32Size = 4
-- converts IPV4 address to word32; not needed currently
toHostAddress :: ByteString -> Maybe HostAddress
toHostAddress bs = if' (DatB.length bs == word32Size)
                      (Just $ word8sToWord32 $ DatB.unpack bs)
                      Nothing

-- NOT TESTED (not like anything in this packet is REALLY tested but hey)
-- it's probably wrong!!
toHostAddress6 :: ByteString -> Maybe HostAddress6
toHostAddress6 bs =if' (DatB.length bs == 4 * word32Size)
                   (let ws = L.map (fromJust . toHostAddress) $ L.unfoldr (Just . (DatB.splitAt 4)) bs
                    in Just (ws!!0, ws!!1, ws!!2, ws!!3))
                   Nothing

-- little endian? fuck this shit idk it just works
word8sToWord32 :: [Word8] -> Word32
word8sToWord32 bytes =  sum $ L.zipWith (*)
                      ((L.take (L.length bytes)) $ powers (2 ^ 8)) 
                      (L.map toWord32 bytes)

readIPv4 :: String -> Word32
readIPv4 s = word8sToWord32 $ L.map (\s -> read s :: Word8) $ DLS.splitOn "." s

sockAddrPort :: SockAddr -> PortNumber
sockAddrPort (SockAddrInet p _) = p
sockAddrPort (SockAddrInet6 p _ _ _) = p

toAddressFamily :: ATYP -> Family 
toAddressFamily IPV4 = AF_INET
toAddressFamily IPV6 = AF_INET6
toAddressFamily DOMAINNAME = undefined -- TODO: wut is this

-- fuckaround runners

runForShow = withSocketsDo $ do -- for sho
  runServer (Config  (PortNumber 1080)
            printerInit
            doSocksHandshake)

runNoOpSocks = withSocketsDo $ do
  runServer (Config  (PortNumber 1080) noOpInit doSocksHandshake)

-- used as a reverse proxy...
runNoProtoProxy = withSocketsDo $ do
  runServer (Config  (PortNumber 1080) printerInit
            (\s -> return $ Connection CONNECT IPV4 $ SockAddrInet
                  (PortNum $ toggleEndianW16 3000) $ readIPv4 "127.0.0.1"))

