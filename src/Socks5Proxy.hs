{-# LANGUAGE ForeignFunctionInterface #-} 
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

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
import Control.Monad.Error
import Control.Monad.Error.Class
import Control.Monad as CM
import qualified Data.ByteString.Lazy as BSL
import Data.Attoparsec 
import Data.Attoparsec.Char8 as DAC
import Network.Socket.ByteString as NBS
import Control.Monad.Error
import Data.Char
import Data.String
import Data.Maybe
import System.IO
import System.IO.Strict as SysIOStrict
import Network.Socket.Splice
import Data.Word
import Prelude as P
--import WireProtocol

import Utils

type PacketHandler = (MonadIO m) => ByteString -> m (ByteString)
data Config = Config {getPort :: PortID, handleIncoming :: PacketHandler, handleOutgoing :: PacketHandler}

type Method = Char
data ClientHandshake = CH Char [Method]
  deriving Show

data CMD = CONNECT | BIND | UDPASSOCIATE -- command
  deriving Show
data ATYP = IPV4 | DOMAINNAME | IPV6 -- address type
  deriving Show
data Connection = Connection CMD ATYP (String, ByteString) Int -- cmd, atyp, address, port
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
  sock <- listenOn $ (getPort config)
  loop config sock


justPrint :: (Show s, MonadIO m) => String -> s -> m s 
justPrint  flag bs = do
  liftIO $ P.putStrLn flag
  liftIO $ P.putStrLn $ show bs
  return bs

runForShow = withSocketsDo $ do
  runServer (Config  (PortNumber 1080) (justPrint "INCOMING!!!") (justPrint "OUTGOING!!!"))

loop config sock = do
  (conn, _) <- accept sock
  forkIO $ handleReq config conn
  loop config sock


doHandshake :: (Data.String.IsString e, MonadIO m, MonadError e m, Functor m) => Socket -> m Connection
doHandshake conn = do
  hs <- getMessage conn parseHandshake (isValidHandshake, "Invalid handshake")
  liftIO $ NBS.sendAll conn "\5\0"
  connRequest <- getMessage conn parseConnectionReq (isValidConnectionReq, "Invalid connection request")
--  liftIO $ P.putStrLn $ show connRequest
  return connRequest

handleReq config conn = do
  eitherConnRequest <- runErrorT $ (doHandshake conn)
  case eitherConnRequest of
    Left err -> P.putStrLn err 
    Right c -> handleConnection c conn config
  -- TODO: add code for catching exceptions when connections is broken
  P.putStrLn "done showing"
  Network.Socket.sClose conn

handleConnection (Connection cmd atyp (addr, bsAddr) port) clientSock config = do
  P.putStrLn "handling connection"
  NBS.send clientSock "\5\0\0\1\0\0 01c" --this is magical as fuck...

  addrInfos <- getAddrInfo Nothing (Just addr) (Just $ show port) 
  let serverAddr = L.head addrInfos -- TODO: issue here might be no head
  serverSock <- socket (addrFamily serverAddr) Stream defaultProtocol
  connect serverSock (addrAddress serverAddr) 

{-
  -- fast proxying solution. problem is it does not allow place a hook
  -- and tamper with the packets
  clientH <- socketToHandle clientSock ReadWriteMode
  serverH <- socketToHandle serverSock ReadWriteMode
  let serverSide = (serverSock, Just serverH)
  let clientSide = (clientSock, Just clientH)
  void . forkIO   $ splice msgSize serverSide clientSide 
  splice msgSize clientSide serverSide 
-}
  forkIO $ forwardPackets clientSock serverSock (handleOutgoing config)
  forwardPackets serverSock clientSock (handleIncoming config)
  return ()

getMessage conn parse (validate, errMsg) = do
  tcpMsg <- liftIO $ NBS.recv conn msgSize
  handshakeParse <- return (parseOnly parse tcpMsg)
  hs <- case handshakeParse of
          Left err -> throwError "failed parse "
          Right hs -> if (validate hs) then return hs else throwError errMsg 
  return hs

-- this is wrong - when connection is broken what happens to this loop?
-- now it just goes on forever...
forwardPackets src dst  trans = do
  package <- NBS.recv src packSize
  transPackage <- trans package
  NBS.send dst transPackage
  forwardPackets src dst trans

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
  h <- anyChar
  l <- anyChar
  return (Connection CONNECT atyp (toStrAddress atyp address, address) (charsToInt h l))

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

toStrAddress :: ATYP -> ByteString -> String
toStrAddress DOMAINNAME = Ch8.unpack
toStrAddress IPV4 = toIPAdress
toStrAddress IPV6 = toIPAdress

toIPAdress :: ByteString -> String
toIPAdress bs = (L.foldl (++) "") . (inbetween ".")  . (L.map $ show . ord) . Ch8.unpack $ bs

-- converts IPV4 address to word32; not needed currently
toWord32Addr :: ByteString -> Word32
toWord32Addr bs = sum $ L.zipWith (*) (L.reverse . (L.take (DatB.length bs)) $ powers (2 ^ 8))  (L.map toWord32 $ DatB.unpack bs)

