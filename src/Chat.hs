{-
Jacob Albers
CMSC 22311 - Functional Systems in Haskell
Instructors: Stuart Kurtz & Jakub Tucholski
2015-05-28
-}

module Chat (chat) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception hiding (handle)
import Network
import System.Environment
import System.IO
import Text.Printf

import Client

--a server is composed of:
--A TVar that can be 
data State = State { usernames :: TVar (Int) }

newServer :: IO State
newServer = State <$> (newTVarIO 0)

talk :: Client -> IO ()
talk client@Client { username = u
									 , handle = h 
									 , channel = c
									 } = do
	hSetBuffering h LineBuffering
	atomically $ writeTChan c (Update $ (show u) ++ " has joined")
	_ <- race (issuer client) (receiver client)
	return ()

stopTalk :: Client -> IO ()
stopTalk Client { username = u
								, handle = h 
								, channel = c
								} = do
	atomically $ writeTChan c (Update $ (show u) ++ " has left")
	hClose h

buildThreads :: State -> TChan (Message) -> Socket -> IO ()
buildThreads server@State {usernames = us} c s = do
	(hdl, host, port) <- accept s
	_ <- printf "Accepted connection from %s: %s\n" host (show port)
	newchan <- atomically $ dupTChan c
	atomically $ modifyTVar us (+ 1)
	newname <- atomically $ readTVar us
	client <- mkClient newname hdl newchan
	_ <- forkFinally (talk client) (\_ -> stopTalk client)
	buildThreads server newchan s

findPort :: IO PortNumber
findPort = do
	chatport <- lookupEnv "CHAT_SERVER_PORT"
	let port = case chatport of
	  	Nothing  -> 5000
	  	Just str -> fromIntegral (read str :: Int)
	return port

-- | Chat server entry point.
chat :: IO ()
chat = withSocketsDo $ do
	_ <- printf "Default port set to 5000\n"
	port <- findPort
	server <- newServer
	_ <- printf "listening on Port: %d\n" (toInteger port)
	chan <- newTChanIO
	bracket (listenOn (PortNumber port)) (sClose) (buildThreads server chan)
	return ()

