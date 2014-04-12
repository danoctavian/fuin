{-# LANGUAGE ForeignFunctionInterface #-} 
{-# LANGUAGE OverloadedStrings #-}

import Prelude as P
import Socks5Proxy
import PackageStream
import Client
import ChanExample
{-

ISSUES 
- intentional bittorrent connection switches (solutions bellow)
- sending errors - at any point in the pipeline? 
    -- TCP guarantess I must offer - delivery of ALL messages - 
- control messages (non-data) saying do this do that;
    SOLUTION: control channel and data channel in code; is it really necessary
    it's cool that channels hold queues for me - imagine that the no flow to the server remains and that a lot of messages
                                                 enqueued to be sent; just take the chan and give it to the new conn - send
                                                 it all stuff (no need to worry about buffering anywhere)
                                                 IF sending through socket fails - unGetTChan to the rescue


 - CHUNKING - need to send packages in chunks  - if bigger than chunks gather more in one bit; if bigger put more in one
      ASSUMPTIONS - text is encrypted with a shared secret using AES meaning length plain == length encrypted - makes it easier to chunk
      LABELLING CHUNKS - each chunk has an associated label - which tells the other side how to assemble things together
          CHUNK EXAMPLE: 1/1, 1/2, 3/10 - this take a few extra constant bytes () also specify size. 
          PLACE INDEXES AT THE BEGINNING to avoid confusion with the real text

CHANGING CHANNEL
 - problem data needs to be piped from server to client still
  - just pause on the server side - he is in control; stop forwarding data messages 
  - just buffer received data messages; (still being pushed into the pipe by the proxy)
 - client side receives switch channel message - it stops its own forwarding of data
  * it buffers messages needed to be sent by the user (tor)
  * no data messages need to be received; the pipe has been stopped


  IDEA:
  * disconnect the bittorrent side while preserving the rest of the stuff 
  * leave data pushing threads running;
  * LEAKY ABSTRACTION ? handle these control messages earlier... rest of the connection doesn't see anything
  * everything just runs normally; it's just that the bittorrent channels - get dropped in a pool - a per connection key 
  helps retrieve them - after handshake protocol is run; the pipes are picked up from the pool and things resent


  The 2 pipes are the data pipes nothing else matters;  

 

server decides to stop sending - stops sending packets; send control message that packet sending is stopped.


interface to bittorrent flow 2 channels - out channel, in channel
how to make the server work?


in the init generate IO functions to handle the connection; if its a normal connection default to just proxying the messages

handle protocol handshake

connect to tor server through torOrPort and decode/encode


BITTORRENT CONNECT -planned interrupt (eg. switch swarm)
* server side - store open OrPort connection in a pool
  ** when new bittorrent conn comes up do hanshake protocol, if revival of connection get it from pool
* client side - store  



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

  

need a module to tell deluge to do shit:
start file connection - params how fast how slow; proxying; do this now!!



the IO function passed to the proxy  
 -- keep the logic of encoding and decoding here -because you need to understand the proto itself 
  --fuck Channels
IO func called by this...

  (send, rec)  <- getChannels addr 
  writeTChan send package
  transPackage <- readTChan rec


FILTERING THREAD IS DEPRECTATED - bad idea
  

-- the bittorrentConnection 
connectTo :
  add to mvar the new IP for the thing to push stuff through it - DEPRECATED 
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
  

