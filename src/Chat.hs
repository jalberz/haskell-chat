-- | CS240h Lab 2 Chat Server
module Chat (chat) where

import Control.Applicative
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Network
import System.IO

import Data.Map as Map

--A username is a unique int 
type Username = Int

--A message can either be:
--a) a post by a user containing their
--message and their username
--b) an update from the server on the 
--state of the chat, i.e. when a new client
--is added or drops the chat.
data Message = Post Username String
				| Update String

--a client is composed of the following:
--1 >> A username, which is a number
--2 >> A handle to be referred to when
--placing output
data Client = 
	Client
	{ username :: Username
	, handle :: Handle
	}

--data declaration for the server
data Server = Server (TVar (Map Username Client))

newServer :: IO Server
newServer = Server <$> (newTVarIO Map.empty)


-- | Chat server entry point.
chat :: IO ()
chat = do
	server <- newServer
	port <- listenOn $ PortNumber 5000
	putStrLn "listening on Port 5000"
	return ()

