{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ConstraintKinds #-}

module ExtendedORPort where


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

hashSize = 32
nonceSize = 32

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
  authTypes <- fmap toStrict $ DCB.takeWhile (0 /=) =$ (DCB.take 256)
  let supportedAuth = 1
  when (not $ DB.elem supportedAuth authTypes) $
    throwError $ "server did not offer auth type " ++ (show supportedAuth)
  writeData . DB.pack $ [supportedAuth]
  writeData clientNonce
  serverHash <- readData hashSize
  serverNonce <- readData nonceSize
  let expectedServerHash = clientServerHash authServerHashHeader authCookie clientNonce serverNonce
  let clientHash = clientServerHash authClientHashHeader authCookie clientNonce serverNonce
  when (serverHash /= expectedServerHash) $ throwError "Error server hash doesn't match expected value"
  writeData clientHash
  status <- readData 1
  when (DB.head status /= 1) $ throwError "server rejected authentication"
  return ()


--extOrCmdDone extOrCmdUserAddr extOrCmdTransport extOrCmdOkay extOrCmdDeny     

 
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
  DB.pack $ hmac (HashMethod SHA256.hash 256) 
        (DB.unpack authCookie) $ DB.unpack $ DB.concat [header, clientNonce, serverNonce]

