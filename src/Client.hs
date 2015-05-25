module Client where

import Control.Applicative
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Network
import System.IO

import Data.Map as Map

--A message can either be:
--a) a post by a user containing their
--message and their username
--b) an update from the server on the 
--state of the chat, i.e. when a new client
--is added or drops the chat.
data Message = Post Username String
				| Update String

--A username is a unique int 
type Username = Int

--a client is composed of the following:
--1 >> A username, which is a number
--2 >> A handle to be referred to when
--placing output
data Client = 
	Client
	{ username :: Username
	, handle :: Handle
	, outgoing :: TChan Message
	}
 
mkClient :: Username -> Handle -> IO Client
mkClient u h = 
 	Client <$> return u <*> return h <*> newTChanIO

issueMsg :: Message -> Client -> STM ()
issueMsg msg Client{username = u
					, handle = h
					, outgoing = o} =
	writeTChan o msg

printMessage :: Client -> Message -> IO ()
printMessage Client{username = u, handle = h} 
			message =
    hPutStrLn h $
        case message of
            Post user msg -> (show user) ++ ": " ++ msg
            Update msg    ->  msg