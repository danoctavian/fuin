{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ConstraintKinds #-}

module ExtendedORPort where

import Prelude as P
import Data.ByteString as DB
import Data.ByteString.Char8 as DBC
import Data.Conduit.Network
import Data.Conduit as DC
import Data.Conduit.List as DCL
import Data.Conduit.Network
import Data.Conduit.Attoparsec
import Data.Conduit.Binary as DCB
import Data.Conduit.Cereal as DCC
import Control.Monad.Error
import Control.Monad.IO.Class
import Network.Socket as NS
import Crypto.Random as CR
import Data.HMAC
import Data.Digest.SHA256 as SHA256
import Data.Word
import GHC.Generics

import Data.Attoparsec as DA
import Data.Attoparsec.Char8 as DAC

import Control.Applicative

import Data.Serialize hiding (Done)
import Data.Serialize.Put
import Data.Serialize.Get hiding (Done)

import PackageStream
import Utils
import NetworkProtocol

{-
implementation of extended or Port Protocol based on reference
https://gitweb.torproject.org/torspec.git/blob/HEAD:/pt-spec.txt
and reference implementation
https://gitweb.torproject.org/torspec.git/blob/HEAD:/pt-spec.txt
-}

data ORPortInfo = ORPortInfo  {orAddr :: SockAddr}
                | ExtendedORPortInfo {orAddr :: SockAddr, authCookie :: DB.ByteString}                            

-- bytes
hashSize = 32
nonceSize = 32
cookieSize = 32

makeORPortConnection :: (MonadNetworkProtocol m, MonadIO m) =>
    ORPortInfo -> SockAddr -> ByteString -> m Socket
makeORPortConnection orPortInfo remoteAddr methodName
  = do
    orSock <- liftIO $ NS.socket (inferAddrFamily (orAddr orPortInfo)) Stream defaultProtocol
    liftIO $ NS.connect orSock (orAddr orPortInfo)
    case orPortInfo of 
      ORPortInfo orAddr -> return () -- nothing to do
      ExtendedORPortInfo orAddr authCookie -> do
        randGen <- liftIO $ (CR.newGenIO :: IO SystemRandom)
        -- generate cryptographically secure client nonce
        clientNonce <- case CR.genBytes nonceSize randGen of
          Left err -> throwError $ show err
          Right (n, gen) -> return n
        -- perform authentication and setup
        runProtocol (dialORPort clientNonce authCookie methodName (DBC.pack $ show remoteAddr)) orSock
        return ()
    return orSock

readData nbytes =  fmap toStrict $ DCB.take nbytes

dialORPort :: MonadNetworkProtocol m =>
      ByteString -> ByteString -> ByteString -> ByteString -> Conduit DB.ByteString m (ProtocolOutput String)
dialORPort clientNonce authCookie methodName remoteAddr = do
  authORPort clientNonce authCookie
  orPortSetup methodName remoteAddr
  return ()


authORPort clientNonce authCookie = do
  let authTypesMaxSize = 256
  authTypes <- fmap toStrict $ DCB.takeWhile (0 /=) =$ (DCB.take authTypesMaxSize)
  endByte <- readData 1 -- clear out the last byte
  let supportedAuth = 1
  when ((not $ DB.elem supportedAuth authTypes) || DB.length authTypes > authTypesMaxSize) $
    throwError $ "server did not offer auth type " ++ (show supportedAuth)
  writeData . DB.pack $ [supportedAuth]
  writeData clientNonce
  serverHash <- readData hashSize
  serverNonce <- readData nonceSize
  let expectedServerHash = clientServerHash authServerHashHeader authCookie clientNonce serverNonce
  let clientHash = clientServerHash authClientHashHeader authCookie clientNonce serverNonce
  when (serverHash /= expectedServerHash) $ throwError $ "Error server hash doesn't match expected value"
  writeData clientHash
  status <- readData 1
  when (DB.head status /= 1) $ throwError "server rejected authentication"
  return ()
 
extOrCmdDone      = 0x0000
extOrCmdUserAddr  = 0x0001
extOrCmdTransport = 0x0002
extOrCmdOkay      = 0x1000
extOrCmdDeny      = 0x1001


data ORPortCommand = ORPortCommand {cmdCode :: Word16, cmdPayload :: ByteString}
    deriving (Eq, Show)



instance Serialize ORPortCommand where

-- ASSUMES length payload < sizeof Word16
  put (ORPortCommand cmdCode payload) = (putWord16be cmdCode)
                      *> (putWord16be (fromIntegral $ DB.length payload))
                      *> (putByteString payload)
  get = getOrPortCmd

getOrPortCmd = do
    cmdCode <- getWord16be
    sz <- getWord16be
    payload <- getBytes (fromIntegral sz)
    return $ ORPortCommand cmdCode payload


testEnc :: Either String ORPortCommand
testEnc =  decode $ encode (ORPortCommand 5 $ DBC.pack "lamatah")
--data ORPortCommand = ORPortCommand {cmdType :: Word16,  payload :: ByteString}

orPortSetup methodName remoteAddr = do
  writeData $ encode $ ORPortCommand extOrCmdUserAddr remoteAddr
  writeData $ encode $ ORPortCommand extOrCmdTransport methodName
  writeData $ encode $ ORPortCommand extOrCmdDone DB.empty 
  cmd <- DCC.sinkGet getOrPortCmd
  select (throwError "server replies with unknown command")
        [ ((cmdCode cmd) == extOrCmdOkay , return ())
        , ((cmdCode cmd) == extOrCmdDeny, throwError "server denied setup")
        ]  

authServerHashHeader = "ExtORPort authentication server-to-client hash"
authClientHashHeader = "ExtORPort authentication client-to-server hash"

clientServerHash :: ByteString -> ByteString -> ByteString -> ByteString -> ByteString
clientServerHash header authCookie clientNonce serverNonce = 
  DB.pack $ hmac (HashMethod SHA256.hash 512) -- why 512? casue fuck you that's why 
        (DB.unpack authCookie) $ DB.unpack $ DB.concat [header, clientNonce, serverNonce]


authCookieHeader = "! Extended ORPort Auth Cookie !\x0a"
parseAuthCookie :: Parser ByteString
parseAuthCookie = string authCookieHeader *> DA.take cookieSize

{-
  DEBUG CODE:
  delete when done
-}
orPortServerBytes clientNonce authCookie = do
  DC.yield "\3\1\2\0" -- auth type
  let sNonce = compl "serverNonce" nonceSize
  let sHash = clientServerHash authServerHashHeader authCookie clientNonce sNonce
  liftIO $ P.putStrLn $ show sHash
  DC.yield sHash
  DC.yield sNonce
  DC.yield "\1"
  DC.yield $ encode $ ORPortCommand extOrCmdOkay DB.empty

awaitPrnt :: (MonadNetworkProtocol m, MonadIO m) => Sink (ProtocolOutput String) m ()
awaitPrnt = DC.awaitForever (liftIO . P.putStrLn . show)


compl bs sz = DB.concat [bs, DBC.replicate (sz - (DB.length bs)) 'A']
testOrPortProtocol :: (MonadNetworkProtocol m, MonadIO m) => m ()
testOrPortProtocol
  = do
  let clientNonce = compl "clientNonce" 32
  let authCookie = compl "authCookie" 32
  let methodName = compl "methodName" 32
  let remoteAddr = compl "remoteAddr" 32
  (orPortServerBytes clientNonce authCookie)  =$ (dialORPort clientNonce authCookie methodName remoteAddr) $$ awaitPrnt
  return ()


testHash = clientServerHash authServerHashHeader (compl "authCookie" 32) (compl "clientNonce" 32) (compl "serverNonce" nonceSize)

readAndCompare = do
  file <- DB.readFile "../scripts/digesthmac"
  P.putStrLn $ show file
  P.putStrLn $ show testHash

runTestOrConn = do
  cookFile <- DB.readFile "/home/dan/tor/extended_orport_auth_cookie"
  conn <- runErrorT $ makeORPortConnection
                      ExtendedORPortInfo {orAddr = (SockAddrInet 6669  $ readIPv4 "127.0.0.1"),
                                          authCookie  = fromRight $ parseOnly parseAuthCookie cookFile}
                      (SockAddrInet 1234 $ readIPv4 "127.0.0.1")
                      "foo"
  liftIO $ P.putStrLn $ "done with connection result is " P.++ (show conn)