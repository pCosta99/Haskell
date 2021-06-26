{-# LANGUAGE OverloadedStrings #-}
-- Echo client program
module Main (main) where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Control.Concurrent

main :: IO ()
main = runTCPClient "127.0.0.1" "3000" $ \s -> do
    procedure s

procedure :: Socket -> IO ()
procedure s = do
    send <- getLine
    sendAll s $ C.pack send
    msg <- recv s 1024
    putStr "Received: "
    C.putStrLn msg

-- from the "network-run" package.
runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close client
  where
    resolve = do
        let hints = defaultHints { addrSocketType = Stream }
        head <$> getAddrInfo (Just hints) (Just host) (Just port)
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        connect sock $ addrAddress addr
        return sock
