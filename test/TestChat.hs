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


newClient :: IO Handle
newClient = do
	port <- getEnv "CHAT_SERVER_PORT"
	conn <- connectTo "localhost" 
		(PortNumber $ fromIntegral (read port :: Int))
	return conn


main :: IO ()
main = hspec $ describe "Testing Lab 2" $ do
	setEnv "CHAT_SERVER_PORT" "3000"
	bracket_ (chat) (printf "tests complete\n") (testbatch)
  -- example quickcheck test in hspec.
  --describe "read" $ do
  --  it "is inverse to show" $ property $
  --    \x -> (read . show) x == (x :: Int)

runMessage :: Property
runMessage = monadicIO $ do
	hdl <- run (newClient)
	hdl' <- run (newClient)
	run (hPutStr hdl "test string")
	line <- run (hGetLine hdl')
	assert (line == "1: test string")
	
runIncrement :: IO ()
runIncrement = monadicIO $ do
	hdl <- run (newClient)
	hdl' <- run (newClient)
	hdl'' <- run (newClient)
	run (hPutStr hdl "first")
	line <- run (hGetLine hdl')
	run (hPutStr hdl' "second")
	line' <- run (hGetLine hdl'')
	run (hPutStr hdl'' "third")
	line'' <- run (hGetLine hdl)
	assert (line == "1: first"
				&& line' == "2: second"
				&& line'' == "3: third")

testbatch :: IO ()
testbatch = hspec $ do
	describe "messages sent test" $ do
  	it "writes to the TChan" $ runMessage `shouldBe` True
  describe "id increment test" $ do
  	it "increments id with each new client" 
  	$ runIncrement `shouldBe` True
	return ()





