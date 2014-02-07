{-# LANGUAGE ForeignFunctionInterface #-} 
{-# LANGUAGE OverloadedStrings #-}

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
import Data.Attoparsec --(Parser, Result, parseWith)
import Data.Attoparsec.Char8 as DAC
--import Data.Attoparsec.Text as AT  (satisfyWith)
import Network.Socket.ByteString as NBS
import Control.Monad.Error
import Data.Char
import Data.String
import Data.Maybe
import System.IO
import System.IO.Strict as SysIOStrict
import Network.Socket.Splice
import Data.Word

import Utils

data Config = Config {foo :: Int}

type Method = Char
data ClientHandshake = CH Char [Method]
  deriving Show

data CMD = CONNECT | BIND | UDPASSOCIATE -- command
  deriving Show
data ATYP = IPV4 | DOMAINNAME | IPV6 -- address type
  deriving Show
data Connection = Connection CMD ATYP (String, ByteString) Int -- cmd, atyp, address, port
  deriving Show

cmdCodes = ['\1', '\2', '\3']
atypCodes = ['\1', '\3', '\4']
toCMD c = lookup c $ L.zip cmdCodes [CONNECT, BIND, UDPASSOCIATE]
toATYP a = lookup a $ L.zip atypCodes [IPV4, DOMAINNAME, IPV6]

main = withSocketsDo $ do
  Prelude.putStrLn "Starting server"
  sock <- listenOn $ PortNumber 5004
  loop (Config 0) sock

loop config sock = do
  (conn, _) <- accept sock
  forkIO $ handleReq config conn
  loop config sock

msgSize = 1024

doHandshake :: (Data.String.IsString e, MonadIO m, MonadError e m, Functor m) => Socket -> m Connection
doHandshake conn = do
  hs <- getMessage conn parseHandshake (isValidHandshake, "Invalid handshake")
  liftIO $ Prelude.putStrLn $ show hs
  liftIO $ NBS.sendAll conn "\5\0"
  connRequest <- getMessage conn parseConnectionReq (isValidConnectionReq, "Invalid connection request")
  liftIO $ Prelude.putStrLn $ show connRequest
  return connRequest

handleReq config conn = do
  eitherConnRequest <- runErrorT $ (doHandshake conn)
  case eitherConnRequest of
    Left err -> Prelude.putStrLn err 
    Right c -> handleConnection c conn config
  Prelude.putStrLn "done showing"
  Network.Socket.sClose conn

handleConnection (Connection cmd atyp (addr, bsAddr) port) clientSock config = do
  Prelude.putStrLn "handling connection"
--  clientH <- socketToHandle clientSock ReadWriteMode
  NBS.send clientSock "\5\0\0\1\0\0 01c" --this is magical as fuck...
--  DatB.hPutStr clientH "\5\0\0\1\0\0 01c"
  Prelude.putStrLn "ack sent"


  addrInfos <- getAddrInfo Nothing (Just "46.108.226.212") (Just $ show port) -- TODO replace this crap
  let serverAddr = L.head addrInfos -- TODO: issue here might be no head
--  connect serverSock (SockAddrInet (PortNum $ fromIntegral port) 778887892) --(toWord32Addr bsAddr))
  Prelude.putStrLn "fetched address" 
  serverSock <- socket (addrFamily serverAddr) Stream defaultProtocol
  connect serverSock (addrAddress serverAddr) 

  clientH <- socketToHandle clientSock ReadWriteMode
  serverH <- socketToHandle serverSock ReadWriteMode
  Prelude.putStrLn "connected to server"
  let serverSide = (serverSock, Just serverH)
  let clientSide = (clientSock, Just clientH)
  void . forkIO   $ splice msgSize serverSide clientSide 
  splice msgSize clientSide serverSide 
  Prelude.putStrLn "after splicing" 
--  serverSock <- connectTo addr (PortNumber $ fromIntegral port) 
  return ()

getMessage conn parse (validate, errMsg) = do
  tcpMsg <- liftIO $ NBS.recv conn msgSize
  liftIO $ Prelude.putStrLn $ ("message " ++  (Ch8.unpack tcpMsg) ++ "|")
  handshakeParse <- return (parseOnly parse tcpMsg)
--  handshakeParse <- fmap (parseOnly parse) $ liftIO (NBS.recv conn msgSize)
  hs <- case handshakeParse of
          Left err -> throwError "failed parse "
          Right hs -> if (validate hs) then return hs else throwError errMsg 
  return hs

forwardPackets src dst = do
  package <- SysIOStrict.hGetContents src
  DatB.hPutStr dst (Ch8.pack package)
  forwardPackets src dst

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

-- converts IPV4 address to word32
toWord32Addr :: ByteString -> Word32
toWord32Addr bs = sum $ L.zipWith (*) (L.reverse . (L.take (DatB.length bs)) $ powers (2 ^ 8))  (L.map toWord32 $ DatB.unpack bs)

powers x = L.map (x ^ ) [0..]

toWord32 :: Word8 -> Word32
toWord32 = fromIntegral
