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

import Client

--data declaration for the server
data Server = Server (TVar (Map Username Client))

newServer :: IO Server
newServer = Server <$> (newTVarIO Map.empty)

{-}
loop :: Server -> Client -> IO ()
loop server client = do
	new <- newEmptyMVar
	let worker io = forkIO ()-}

-- | Chat server entry point.
chat :: IO ()
chat = do
	server <- newServer
	port <- listenOn $ PortNumber 5000
	putStrLn "listening on Port 5000"

	return ()

