{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module ExtendedORPort where

import Data.ByteString as DB
import Data.ByteString.Char8 as DBC
import Data.Conduit.Network
import Data.Conduit
import Data.Conduit.List as DCL
import Data.Conduit.Attoparsec
import Control.Monad.Error
import Control.Monad.IO.Class
import Network.Socket as NS

import PackageStream
import Utils
-- TODO: implement

{-

implementation of extended or Port Protocol based on reference
https://gitweb.torproject.org/torspec.git/blob/HEAD:/pt-spec.txt
and reference implementation
https://gitweb.torproject.org/torspec.git/blob/HEAD:/pt-spec.txt

the ORdial protocol is implemented as a conduit
 which can return either
  bytestring
  error
  finalResult

-}

data ORPortInfo = ORPortInfo  {orAddr :: SockAddr}
                | ExtendedORPortInfo {orAddr :: SockAddr, authCookie :: DB.ByteString}                            


{-
         Bindaddrs      []Bindaddr
 524         OrAddr         *net.TCPAddr
 525         ExtendedOrAddr *net.TCPAddr
 526         AuthCookie     []byte

 6 type Bindaddr struct {
 367         MethodName string
 368         Addr       *net.TCPAddr
 369         // Options from TOR_PT_SERVER_TRANSPORT_OPTIONS that pertain to this
 370         // transport.
 371         Options Args
 372 }

 -}

data ProtocolOutput a = ProtoData DB.ByteString | ProtoError String | FinalResult a

dialORPort :: (Monad m, MonadError String m) => Conduit DB.ByteString m (ProtocolOutput Int)
dialORPort = undefined


makeORPortConnection :: (MonadError String m, MonadIO m) => ORPortInfo -> SockAddr -> m Socket
makeORPortConnection orPortInfo remoteAddr
  = do
    orSock <- liftIO $ NS.socket (inferAddrFamily (orAddr orPortInfo)) Stream defaultProtocol
    liftIO $ NS.connect orSock (orAddr orPortInfo)
    case orPortInfo of 
      ORPortInfo orAddr -> return orSock -- nothing to do
      ExtendedORPortInfo orAddr authCookie -> do
        -- perform authentication and setup
        return orSock