{-# LANGUAGE ForeignFunctionInterface #-} 
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Socks5Proxy where
  
import Control.Monad.IO.Class
import Data.ByteString.Char8 as DBC
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

import Data.Serialize as DS
import BittorrentParser as BP

--import WireProtocol

import Utils

data PacketHandlers = PacketHandlers {incoming :: PacketHandler, outgoing :: PacketHandler}
type PacketHandler = (MonadIO m) => ByteString -> m (ByteString)


idPacketHandlers = PacketHandlers return return

-- client sock -> remote server sock -> handlers
type InitHook = (MonadIO m) => SockAddr -> SockAddr -> m PacketHandlers
type GetConn = (Data.String.IsString e, MonadIO m, MonadError e m, Functor m) => Socket -> m Connection
data Config = Config {listenPort :: PortID, initHook :: InitHook, getConn :: GetConn}

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


cmdCodes = ['\1', '\2', '\3']
atypCodes = ['\1', '\3', '\4']
toCMD c = lookup c $ L.zip cmdCodes [CONNECT, BIND, UDPASSOCIATE]
toATYP a = lookup a $ L.zip atypCodes [IPV4, DOMAINNAME, IPV6]

logger = "fuin.socks5"

runServer :: (MonadIO io) => Config -> io ()
runServer config = liftIO $ withSocketsDo $  do
  liftIO $ debugM Socks5Proxy.logger "Starting server"
  sock <- listenOn $ (Socks5Proxy.listenPort config)
  loop config sock





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
    Left err -> debugM Socks5Proxy.logger err 
    Right c -> handleConnection c (sock, addr) config
  -- TODO: add code for catching exceptions when connections is broken
  debugM Socks5Proxy.logger "done showing"
  Network.Socket.sClose sock

handleConnection (Connection cmd atyp serverSockAddr) (clientSock, clientAddr) config = do
  debugM Socks5Proxy.logger "handling connection"
  {- getting the address can also be done as such:
  let addr = getStrAddress atyp bsAddr
  addrInfos <- getAddrInfo Nothing (Just addr) (Just $ show $ sockAddrPort ) 
  let serverAddr = L.head addrInfos -- TODO: issue here might be no head
  -}
  serverSock <- socket (toAddressFamily atyp) Stream defaultProtocol --
  {- reverses endianness of whatever i provide as portNumber
    ; the PortNumber thing assumes what
    is given to it is big endian (network byte order)
    and turns it into little endian (host byte order) -}
  debugM Socks5Proxy.logger $ "address is " ++ (show serverSockAddr)
  connect serverSock serverSockAddr
  handlers <- (initHook config) clientAddr serverSockAddr 
{-
  -- fast proxying solution. problem is it does not allow place a hook
  -- and tamper with the packets
  -- using conduit atm -it's prolly efficient nough
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
toStrAddress DOMAINNAME = DBC.unpack
toStrAddress IPV4 = toIPAdress
toStrAddress IPV6 = toIPAdress

getStrAddress :: ATYP -> Either ByteString String -> String
getStrAddress atyp (Left bs) = toStrAddress atyp bs
getStrAddress _ (Right s) = s

toIPAdress :: ByteString -> String
toIPAdress bs = (L.foldl (++) "") . (inbetween ".")  . (L.map $ show . ord) . DBC.unpack $ bs


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


sockAddrPort :: SockAddr -> PortNumber
sockAddrPort (SockAddrInet p _) = p
sockAddrPort (SockAddrInet6 p _ _ _) = p

toAddressFamily :: ATYP -> Family 
toAddressFamily IPV4 = AF_INET
toAddressFamily IPV6 = AF_INET6
toAddressFamily DOMAINNAME = undefined -- TODO: wut is this


-- socks5 proxy which doesn't tamper with the packets
runNoOpSocks = withSocketsDo $ do
  runServer (Config  (PortNumber 1080) noOpInit doSocksHandshake)
noOpInit _ _ = return $ PacketHandlers return return





{-
MANUAL Debugging code
TODO; remove once done
-}
justPrint :: (Show s, MonadIO m) => String -> s -> m s 
justPrint  flag bs = do
  liftIO $ debugM Socks5Proxy.logger flag
  liftIO $ debugM Socks5Proxy.logger $ show bs
  return bs

printerInit _ _ = return $ PacketHandlers (justPrint "INCOMING!!!") (justPrint "OUTGOING!!!")




-- used as a reverse proxy...
runNoProtoProxy = withSocketsDo $ do
  runServer (Config  (PortNumber 1080) printerInit
            (\s -> return $ Connection CONNECT IPV4 $ SockAddrInet
                  (PortNum $ toggleEndianW16 3000) $ readIPv4 "127.0.0.1"))


runRevProxyTest = withSocketsDo $ do
  liftIO $ updateGlobalLogger Socks5Proxy.logger (setLevel DEBUG)
  let inputPort = PortNumber 8888
      outputPort = PortNum $ toggleEndianW16 6881 
  liftIO $ debugM Socks5Proxy.logger
    $ "running reverse proxy from " P.++ (show inputPort) P.++ " to " P.++ (show outputPort)
  runServer (Config inputPort printerInit
            (\s -> return $ Connection CONNECT IPV4 $ SockAddrInet
                  outputPort $ readIPv4 "127.0.0.1"))


tamperOut bs = do 
  let btPacket = (DS.decode bs :: Either String BP.Message)
  liftIO $ debugM Socks5Proxy.logger $ show (DatB.length bs, show btPacket)
  return bs

tamperInc = tamperOut


tamperingInit :: InitHook
tamperingInit s1 s2 = return $ PacketHandlers tamperOut tamperOut


saveToHandle handle bs = do
  liftIO $ P.putStrLn $ show bs
  
  liftIO $ DatB.hPutStr handle bs
  liftIO $ hFlush handle
  
  return bs

testWriteToFile = do
  fh <- openFile "test" WriteMode
  DatB.hPutStr fh (DBC.pack "this is me niggas")
  hFlush fh
  threadDelay (10^8)

runRevTamperingProxy = withSocketsDo $ do
  liftIO $ updateGlobalLogger Socks5Proxy.logger (setLevel DEBUG)

  let inFile = "incomingTraffic"
      outFile = "outgoingTraffic"
  hIn <- liftIO $ openFile inFile WriteMode
  hOut <- liftIO $ openFile outFile WriteMode

  let inputPort = PortNumber 8888
      outputPort = PortNum $ toggleEndianW16 6881 
  liftIO $ debugM Socks5Proxy.logger
    $ "running reverse proxy from " P.++ (show inputPort) P.++ " to " P.++ (show outputPort)
  runServer (Config inputPort (\ s1 s2 -> return $ PacketHandlers (saveToHandle hIn) (saveToHandle hOut))
            (\s -> return $ Connection CONNECT IPV4 $ SockAddrInet
                  outputPort $ readIPv4 "127.0.0.1"))