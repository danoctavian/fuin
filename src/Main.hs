{-# LANGUAGE ForeignFunctionInterface #-} 
{-# LANGUAGE OverloadedStrings #-}

import Prelude as P
import Socks5Proxy



{-

--the threads  handling the tor proxying
 -- this is called when a tor connection is made on one side (client)
createConection send receive
  --do protocol to read
  receive
  send bla

  initiate thread for handling connection to a remote peer
  (btSend, btRec) <- connectTo bittorrentID
  set 2 way pipes
  to handle the connections
  


the IO function passed to the proxy  
  --fuck Channels
IO func called by this...

  (send, rec)  <- getChannels addr 
  writeTChan send package
  transPackage <- readTChan rec



-- the bittorrentConnection 
connectTo :
  add to mvar the new IP for the thing to push stuff through it
  get a send (by wrapping getting a channel to push items into)
  get a receive -> (by putting a func that checks if a package contains tor payload)
  do protocol to handshake


  forkIO to listen on the package out channel and check if smth is in the package in channel -- push it out
  

connectTo 

-}

{-
  socks5 threads
  incoming 
  outgoing
    both send to filtering thread (knows about who needs what)
   
  filtering thread - reads chan about who to filter to
  gets incoming and outgoing - sends them to responsible thread waits for them to get back to be pushed:q
  
-}


{-
INCOMING
 tor <-  if data  | if control <decode>  <- encoded bittorrent <-filter<- bittorrent 
                  V


                   
OUTGOING
 tor-> <encode -> -> push on to bittorrent package
-}

{-
incomingPackage
  if data 
    pass forward
  if control
    if handshake do it
    if switch swarm - do it
-}

{-

makeConnection server torConn = do
  btConn <- createBTConnection server
  proxy btConn torConn

-}


{-

makeTorren1

-}

main = do
  P.putStrLn "main running"
  

