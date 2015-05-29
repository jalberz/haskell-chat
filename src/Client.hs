{-
Jacob Albers
CMSC 22311 - Functional Systems in Haskell
Instructors: Stuart Kurtz & Jakub Tucholski
2015-05-28
-}

module Client  where

import Control.Applicative
import Control.Monad (forever)
import Control.Concurrent.STM
import System.IO (hPutStrLn, hGetLine, Handle)

--A message can either be:
--a) a post by a user containing their
--message and their username
--b) an update from the server on the 
--state of the chat, i.e. when a new client
--is added or drops the chat.
data Message = Post Username String
				| Update String
				deriving (Show, Eq, Ord)

--A username is a unique int 
type Username = Int

--a client is composed of the following:
--1 >> A username, which is a number
--2 >> A handle to be referred to when
--placing output
--3 >> A channel for broadcasting to
data Client = 
	Client
	{ username :: Username
	, handle :: Handle
	, channel :: TChan (Message)
	}
 
--build Client from component pieces 
mkClient :: Username -> Handle -> TChan (Message) -> IO Client
mkClient u h c = 
 	Client <$> return u <*> return h <*> (atomically (dupTChan c))

issueMessage :: Client -> Message -> IO ()
issueMessage Client {username = u
										, handle = h
										}
	message = do
    hPutStrLn h $
        case message of
            Post user msg -> if (user /= u)
            	then (show user) ++ ": " ++ msg
            	else ""
            Update msg    ->  msg

issuer :: Client -> IO ()
issuer Client { username = u
							, handle = h 
							, channel = c
							} = forever $ do
    line <- hGetLine h
    let msg = Post u line
    atomically $ writeTChan c msg

receiver :: Client -> IO ()
receiver client@Client { channel = c
										 	 } = forever $ do
  msg <- atomically $ readTChan c
  issueMessage client msg