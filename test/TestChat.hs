{-
Jacob Albers
CMSC 22311 - Functional Systems in Haskell
Instructors: Stuart Kurtz & Jakub Tucholski
2015-05-28
-}

-- | Test our chat server.
module Main (main) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Control.Exception
import System.Environment (setEnv, getEnv)
import System.IO
import Control.Concurrent (forkFinally)
import Text.Printf (printf)
import Control.Concurrent.STM
import Network

import Chat


newClient :: IO handle
newClient = do
	conn <- connectTo "localhost" 
		(fromIntegral (read (getEnv "CHAT_SERVER_PORT") :: Int))
	return conn

{-}
main :: IO ()
main = hspec $ describe "Testing Lab 2" $ do

  -- example quickcheck test in hspec.
  describe "read" $ do
    it "is inverse to show" $ property $
      \x -> (read . show) x == (x :: Int)
  --describe "messages sent test" $ do
  --	it "writes to the TChan" $ prop_Message

prop_Message :: Handle-> Property
prop_Message handle= monadicIO $ do
	run (chat)
	hPutStr h "test string"
	run (issuer client)
	msg <- atomically $ readTChan c
	assert (msg == "test string")

prop_incr :: State -> TChan (Message) -> Socket -> Property
prop_incr server c s = monadicIO $ do
	run(chat)
-}

runMessage :: IO Bool
runMessage = do
	handle <- newClient
	handle' <- newClient
	hPutStr handle "test string"
	line <- hGetLine handle'
	return (line == "test string")
	
--runIncrement :: IO ()
--runIncrement = do
--	handle <- newClient
--	h

testbatch :: IO ()
testbatch = do
	forkFinally (runMessage) (\_ -> printf "Run Message Test done\n")
	--forkFinally (runIncrement) (\_ -> check --printproblem)
	--forkFinally (runEnterExit) (\_ -> check --printproblem)
	--forkFinally (runBroadcast) (\_ -> check --printproblem)
	--forkFinally (runReceipt) (\_ -> check --printproblem)
	return ()

main :: IO ()
main = do
	setEnv "CHAT_SERVER_PORT" 3000
	bracket_ (chat) (printf "tests complete\n") (testbatch)
	return ()




