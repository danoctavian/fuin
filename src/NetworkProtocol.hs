{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ConstraintKinds #-}

module NetworkProtocol where

import Data.ByteString as DB
import Data.ByteString.Char8 as DBC
import Data.Conduit.Network
import Data.Conduit as DC
import Data.Conduit.List as DCL
import Data.Conduit.Network
import Data.Conduit.Attoparsec
import Control.Monad.Error
import Control.Monad.IO.Class
import Network.Socket as NS
import Network.Socket.ByteString as NSB

import Control.Monad.Catch

{-
module with skeletons for writing protocols

-}

data ProtocolOutput a = ProtoData DB.ByteString | FinalResult a
  deriving (Show)

type ProtocolConduit a = (Monad m, MonadNetworkProtocol m) => Conduit DB.ByteString m (ProtocolOutput a)


type MonadNetworkProtocol m = (MonadError String m, MonadThrow m)

writeData = DC.yield . ProtoData
giveFinalResult = DC.yield . FinalResult

runProtocol :: (MonadNetworkProtocol m, MonadIO m) => (ProtocolConduit a) -> Socket -> m (Maybe a)
runProtocol protocol sock = (sourceSocket sock) =$ protocol $$ (sinkSocketWithResult sock)

sinkSocketWithResult :: (MonadIO m) => Socket -> Consumer (ProtocolOutput a) m (Maybe a)
sinkSocketWithResult socket = loop
  where
    loop = do 
      item <- await
      case item of 
        (Just (ProtoData bs)) -> lift (liftIO $ sendAll socket bs) >> loop
        (Just (FinalResult r)) -> return $ Just r
        Nothing -> return Nothing