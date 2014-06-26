{-# LANGUAGE ForeignFunctionInterface #-} 
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Socks5Proxy where
  
import Control.Monad.IO.Class
import Data.ByteString.Char8 as DBC
import Data.ByteString as DB
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
import Data.Attoparsec as DA
import Data.Attoparsec.Char8 as DAC
import Data.Attoparsec.Combinator as DACo
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
import Data.Conduit.Attoparsec
import Control.Monad.Trans.Resource
import Control.Applicative
import Control.Concurrent.STM
import System.Log.Logger
import System.Log.Handler.Syslog
import System.Log.Handler.Simple
import Data.List.Split as DLS
import GHC.Int
import Data.Maybe
import Data.Time
import System.Locale
import Data.Time.Clock.POSIX

import Data.Serialize as DS
import BittorrentParser as BP

--import WireProtocol
import TorrentFileParser
import Utils

data PacketHandlers = PacketHandlers {incoming :: PacketHandler, outgoing :: PacketHandler}
type PacketHandler = (MonadIO m, MonadThrow m) => Conduit ByteString m ByteString


idPacketHandlers = let idPacketHandler = DCL.map id in PacketHandlers idPacketHandler idPacketHandler

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





loop config serverSock
  = forever $ accept serverSock >>= liftIO . forkIO . (handleReq config)


doSocks4Handshake :: GetConn
doSocks4Handshake conn = do
  connRequest <- getMessage conn parseSocks4Cmd (isValidSocks4Cmd, "invalid connect command")
  liftIO $ NBS.sendAll conn  $ DB.concat ["\0\90", DB.replicate 6 0]
  return connRequest 

doSocks5Handshake :: GetConn
doSocks5Handshake conn = do
  hs <- getMessage conn parseHandshake (isValidHandshake, "Invalid handshake")
  liftIO $ NBS.sendAll conn "\5\0"
  connRequest <- getMessage conn parseSocks5Cmd (isValidConnectionReq, "Invalid connection request")
  liftIO $ NBS.send conn "\5\0\0\1\0\0 01c" --this is magical as fuck... it basically says to the client it's ok
  return connRequest

handleReq config (sock, addr) = do
  eitherConnRequest <- runErrorT $ (getConn config $ sock)
  case eitherConnRequest of
    Left err -> debugM Socks5Proxy.logger err 
    Right c -> handleConnection c (sock, addr) config
  -- TODO: add code for catching exceptions when connections is broken
  debugM Socks5Proxy.logger "done showing"
  Network.Socket.sClose sock

handleConnection conn@(Connection cmd atyp serverSockAddr) (clientSock, clientAddr) config = do
  debugM Socks5Proxy.logger $ "handling connection" ++ (show conn) 
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
forwardPackets :: (MonadIO io, MonadThrow io) => Socket ->
                  Socket -> PacketHandler
                  -> io ()
forwardPackets src dst trans = do
  r <- (sourceSocket src) =$ trans $$ (sinkSocket dst)
  return ()

-- only support TCP connect. fail otherwise
parseSocks5Cmd :: Parser Connection
parseSocks5Cmd = do
  verB <- anyChar
  cmd <- fmap fromJust $ satisfyWith (toCMD . word8ToChar) maybeToBool
  anyChar
  atyp <- fmap fromJust $ satisfyWith (toATYP . word8ToChar) maybeToBool
  address <- case atyp of
    IPV4 -> DAC.take 4
    DOMAINNAME -> do {size <- fmap ord $ anyChar ; DAC.take size}
    IPV6 -> DAC.take 16
  port <- anyWord16be
  return (Connection cmd atyp $ fromJust $ toSockAddress atyp address  port)

parseSocks4Cmd :: Parser Connection
parseSocks4Cmd = do
  versionByte <- anyChar
  cmd <- fmap fromJust $ satisfyWith (toCMD . word8ToChar) maybeToBool
  port <- anyWord16be
  address <- DAC.take 4 -- only IPV4
  let terminator = 0
  userid <- DA.takeTill (== terminator)
  word8 terminator
  let atyp = IPV4
  return (Connection cmd atyp $ fromJust $ toSockAddress atyp address  port)


isValidSocks4Cmd :: Connection  -> Bool
isValidSocks4Cmd _ = True

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
toHostAddress bs = if' (DB.length bs == word32Size)
                      (Just $ word8sToWord32 $ DB.unpack bs)
                      Nothing

-- NOT TESTED (not like anything in this packet is REALLY tested but hey)
-- it's probably wrong!!
toHostAddress6 :: ByteString -> Maybe HostAddress6
toHostAddress6 bs =if' (DB.length bs == 4 * word32Size)
                   (let ws = L.map (fromJust . toHostAddress) $ L.unfoldr (Just . (DB.splitAt 4)) bs
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
  liftIO $ updateGlobalLogger Socks5Proxy.logger (setLevel DEBUG)
  runServer (Config  (PortNumber 1080) printerInit doSocks4Handshake)
noOpInit _ _ = return $ idPacketHandlers

runNoOpSocks4 = withSocketsDo $ do
  liftIO $ updateGlobalLogger Socks5Proxy.logger (setLevel DEBUG)
  runServer (Config  (PortNumber 1080) printerInit doSocks4Handshake)




{-
MANUAL Debugging code
TODO; remove once done
-}
--justPrint :: (Show s, MonadIO m) => String -> s -> m s 
justPrint  flag
  = DCL.mapM $ \bs -> do
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
  liftIO $ debugM Socks5Proxy.logger $ show (DB.length bs, show btPacket)
  return bs

tamperInc = tamperOut

type GetPiece = Int ->  GHC.Int.Int64 -> GHC.Int.Int64-> BSL.ByteString

tamperingInit :: Maybe (TChan String) -> (Maybe GetPiece) -> InitHook
tamperingInit msgChan getPiece s1 s2 = do
  liftIO $  debugM Socks5Proxy.logger  $ "local address is " ++ (show s1)
  liftIO $  debugM Socks5Proxy.logger  $ "remote server address is " ++ (show s2)
  epoch_int <- epochTime
  liftIO $ debugM Socks5Proxy.logger $ "connection start is time " ++ (show epoch_int)
  return $ if' (show s2 == "129.31.191.89:6891")
           (PacketHandlers (printHandler msgChan getPiece "incoming") $ printHandler Nothing Nothing "outgoing")
           (PacketHandlers (justPrint "INCOMING!!!") (justPrint "OUTGOING!!!"))


saveToHandle handle bs = do
  liftIO $ P.putStrLn $ show bs
  
  liftIO $ DB.hPutStr handle bs
  liftIO $ hFlush handle
  
  return bs

tamperedPrefix = "TAMPERED"
applyTamper bs = DB.concat [tamperedPrefix, DB.drop (DB.length tamperedPrefix) bs]

printHandler :: (Maybe (TChan String)) -> (Maybe GetPiece) -> String -> PacketHandler
printHandler maybeChan getPc channel 
  = conduitParser (headerParser <|> packageParser)
    =$= DCL.mapM (\(_, pack) -> do
      let prnt p = return p -- liftIO $ debugM Socks5Proxy.logger $ "received BT package " P.++ p
      case pack of 
        Right piece@(Piece num sz payload) -> do
          
          {-
          cmd <- if' (maybeChan /= Nothing)
            (liftIO $ atomically $ tryReadTChan $ fromJust maybeChan)
            (return Nothing)

            -}
            --tryReadTChan
            -- a foo modification to see if it screws up things
          let toTamper = False--cmd /= Nothing
          let trans = if'(toTamper) (return. applyTamper)  return
         -- when (cmd /= Nothing) $ liftIO $ debugM Socks5Proxy.logger $ show piece
          let foundTampered = (not (isNothing getPc)) && DB.isPrefixOf (toStrict tamperedPrefix) payload
          -- when foundTampered $ liftIO $ debugM Socks5Proxy.logger "##############found tampered piece###############"
          epoch_int <- epochTime
          liftIO $ P.putStrLn (show epoch_int)
          liftIO $ debugM Socks5Proxy.logger $ channel ++ (show $ epoch_int)
          let fix = if' (foundTampered && (not $ isNothing getPc))
                        (\ bs -> toStrict $ (fromJust getPc)  num (fromIntegral sz) (fromIntegral $ DB.length payload))
                        P.id
          prnt ("piece")
          newPayload <- (trans payload)
          (return . serializePackage . (Piece num sz)) $ fix newPayload
        Right other -> do
          let serialized = serializePackage other
          prnt ("other than a piece " P.++ (show $ DB.take 20 $ serialized))
          return $ serialized
        Left bs -> do
          prnt ("unparsable package " P.++ (show $ DB.head bs))
          return $ prefixLen bs 
        )

epochTime = liftIO $ getPOSIXTime -- liftIO $ ((read <$> formatTime defaultTimeLocale "%s" <$> getCurrentTime) :: IO Int)

runTamperingSocks = do
  getPiece <- pieceLoader "/homes/dco210/demoFiles/bigFile.dan.torrent" "/homes/dco210/demoFiles/bigFile.dan"
  let getPieceBlock = toGetPieceBlock getPiece
  liftIO $ updateGlobalLogger Socks5Proxy.logger (setLevel DEBUG)
  runServer (Config  (PortNumber 1080) (tamperingInit Nothing $ Just getPieceBlock) doSocks4Handshake)

runSimpleRevTamperingProxy = do
  liftIO $ updateGlobalLogger Socks5Proxy.logger (setLevel DEBUG)
  liftIO $ debugM Socks5Proxy.logger $ "running reverse tampering proxy"
  let inputPort = PortNumber 8888
      outputPort = PortNum $ toggleEndianW16 6891 
  liftIO $ debugM Socks5Proxy.logger
    $ "running reverse proxy from " P.++ (show inputPort) P.++ " to " P.++ (show outputPort)
  let initF = printerInit --(\s1 s2 -> return (PacketHandlers (printHandler $ Just cmdChan) $ printHandler Nothing))
  runServer (Config inputPort initF
            (\s -> return $ Connection CONNECT IPV4 $ SockAddrInet
                  outputPort $ readIPv4 "127.0.0.1"))

runRevTamperingProxy = withSocketsDo $ do
  liftIO $ updateGlobalLogger Socks5Proxy.logger (setLevel DEBUG)
  liftIO $ debugM Socks5Proxy.logger $ "running reverse tampering proxy"
{-
  let inFile = "incomingTraffic"
      outFile = "outgoingTraffic"
  hIn <- liftIO $ openFile inFile WriteMode
  hOut <- liftIO $ openFile outFile WriteMode
-}
  cmdChan <- liftIO $ atomically $ newTChan

  liftIO $ atomically $ writeTChan cmdChan "mue la olteni" -- fuck up the first piece
  liftIO $ forkIO $ forever $ do 
    line <- liftIO $ P.getLine
    liftIO $ atomically $ writeTChan cmdChan line
  let inputPort = PortNumber 8888
      outputPort = PortNum $ toggleEndianW16 6891 
  liftIO $ debugM Socks5Proxy.logger
    $ "running reverse proxy from " P.++ (show inputPort) P.++ " to " P.++ (show outputPort)
  let initF = (\s1 s2 ->  do
          liftIO $  debugM Socks5Proxy.logger  $ "local address is " ++ (show s1)
          liftIO $  debugM Socks5Proxy.logger  $ "remote server address is " ++ (show s2)
          return (PacketHandlers (printHandler (Just cmdChan) Nothing "outgoing") $ printHandler Nothing Nothing "incoming"))
  runServer (Config inputPort initF
            (\s -> return $ Connection CONNECT IPV4 $ SockAddrInet
                  outputPort $ readIPv4 "127.0.0.1"))
