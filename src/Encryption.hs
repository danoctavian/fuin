{-# LANGUAGE OverloadedStrings #-}

module Encryption where

import Crypto.Cipher.AES
import Data.ByteString.Char8 as DBC
import Data.Byteable
import Prelude as P
import Data.List as DL
import System.Random.MWC
import Control.Monad
import Control.Monad.Primitive
import System.IO
import Data.ByteString as DB
import Data.Word (Word8)

import Utils

{-
TODO verify if using AES directly is the way to go.
i need methods for veryfying something is decryptable
i need to check for tampering (similar to above i guess)
-}

{-
WARNING:
  at this point this module does not perform encryption 

  HOWEVER has the interface functions and datatypes that the rest of the code
  will use. It is flexible enough to accomodate AES and other types of encryption

  At the momoent it simulates encryption by appending a magic header
  at the beginning of the "encrypted" message so as to allow 
  for the check of succesful "decryption"
-}

magicalHeader :: ByteString
magicalHeader = "MAGIC"

type Key = ByteString -- 32 bytes key

type Encrypt = ByteString -> (ByteString, Encryption)
type Decrypt = ByteString -> Maybe ByteString
type IV = ByteString

data Encryption = Encryption {encrypt :: Encrypt, overhead :: Int}
data Decryption = Decryption {decrypt :: Decrypt}

data ClientEncryption = ClientEncryption {
                            bootstrapClientEncrypt :: Encryption,
                            clientEncrypt :: Encryption,
                            clientDecrypt :: Decryption,
                            bootstrapData :: ByteString
                          }
                        

data BootstrapServerEncryption = BootstrapServerEncryption {
                            bootstrapServerDecrypt :: Decryption,
                            serverKey :: Key,
                            serverSeediv :: IV
                          }

data ServerEncryption = ServerEncryption {
                          serverEncrypt :: Encryption,
                          serverDecrypt :: Decryption
                        }

-- TODO: give proper implementation
makeClientEncryption :: Key -> Key -> IV -> ClientEncryption
makeClientEncryption clientKey serverKey iv
  = ClientEncryption (makeEncryption serverKey iv) (makeEncryption sharedSecret iv)
                    (makeDecryption sharedSecret) clientKey
    where
      sharedSecret = makeSharedSecret clientKey serverKey

makeServerBootstrapEncryption :: Key -> IV -> BootstrapServerEncryption
makeServerBootstrapEncryption key iv 
  = BootstrapServerEncryption (makeDecryption key) key iv


-- TODO: implement properly
makeSharedSecret :: Key -> Key -> Key
makeSharedSecret client server = client

makeEncryption :: Key -> IV -> Encryption
makeEncryption key iv = Encryption {encrypt = headerEnc, overhead = DB.length magicalHeader}
  where
    headerEnc bs = (DB.concat [magicalHeader, bs], makeEncryption key iv)

makeDecryption :: Key -> Decryption
makeDecryption key = Decryption {decrypt = \bs -> let len = DB.length magicalHeader in
                                                  if' (DB.take len bs == magicalHeader)
                                                      (Just $ DB.drop len bs) Nothing}


-- TODO: make proper implementation 
makeServerEncryption :: BootstrapServerEncryption -> Key -> ServerEncryption
makeServerEncryption serverEnc clientKey
  = ServerEncryption (makeEncryption sharedSecret (serverSeediv serverEnc)) (makeDecryption sharedSecret)
    where
      sharedSecret = makeSharedSecret clientKey (serverKey serverEnc)


-- fake keys

fakeKey = DB.replicate 32 (fromIntegral 1)


