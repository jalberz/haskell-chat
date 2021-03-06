{-
Jacob Albers
CMSC 22311 - Functional Systems in Haskell
Instructors: Stuart Kurtz & Jakub Tucholski
2015-05-28
-}

module Chat (chat) where

--external libraries
import Control.Applicative 
import Control.Concurrent (forkFinally)
import Control.Concurrent.Async (race)
import Control.Concurrent.STM
import Control.Exception hiding (handle)
import Network
import System.Environment (lookupEnv)
import System.IO (hSetBuffering, BufferMode (LineBuffering)
								 , hClose)
import Text.Printf (printf)

--homebrewed module
import Client

--A server state is composed of:
--A TVar that represents the current username number
data State = State { usernames :: TVar (Int) }

newServer :: IO State
newServer = State <$> (newTVarIO 0)

--talk broadcasts that a client has joined the chat
--and allows a client issue her own messages through 
--and receive messages from others via the race function.
talk :: Client -> IO ()
talk client@Client { username = u
									 , handle = h 
									 , channel = c
									 } = do
	hSetBuffering h LineBuffering
	atomically $ writeTChan c (Update $ (show u) ++ " has joined")
	_ <- race (issuer client) (receiver client)
	return ()

--broadcasts the departure of a client and closes their handle.
stopTalk :: Client -> IO ()
stopTalk Client { username = u
								, handle = h 
								, channel = c
								} = do
	atomically $ writeTChan c (Update $ (show u) ++ " has left")
	hClose h


--buildThreads does most of the heaving lifting in terms of
--architecting the state of the server. It derives the current
--information from the server state, a socket, and the latest 
--channel to build a Client. this client is then sent to join
--the chat via talk -> an exception triggers stopTalk.
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


-- | Chat server entry point.
-- chat searches for the current value of CHAT_SERVER_PORT,
-- if that value has not been set, it uses a default of 5000.
-- It brackets the opening of a socket, building threads and
--running the chat, and the ulimate closing of that socket.
chat :: IO ()
chat = withSocketsDo $ do
	_ <- printf "Default port set to 5000\n"
	port <- findPort
	server <- newServer
	_ <- printf "listening on Port: %d\n" (toInteger port)
	chan <- newTChanIO
	bracket (listenOn (PortNumber port)) (sClose) (buildThreads server chan)
	return ()

findPort :: IO PortNumber
findPort = do
	chatport <- lookupEnv "CHAT_SERVER_PORT"
	let port = 
		case chatport of
	  	Nothing  -> 5000
	  	Just str -> fromIntegral (read str :: Int)
	return port
