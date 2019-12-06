module Server where

import Network (HostName, PortNumber, listenOn, withSocketsDo, accept, PortID(..), Socket)
import System.IO (hSetBuffering, hGetLine, hPutStrLn, BufferMode(..), Handle)
import Control.Concurrent (forkIO)
import Control.Monad.STM
import Control.Concurrent.STM.TVar

type Port = Int

runServer :: Port -> IO ()
runServer port = withSocketsDo $ do
    sock <- listenOn $ PortNumber (fromIntegral port)
    putStrLn $ "Listening on " ++ (show port)
    ps <- atomically $ newTVar []
    sockHandler sock ps
    return ()

sockHandler :: Socket -> TVar [(HostName, Port)] -> IO ()
sockHandler sock ps = do
    (handle, host, port) <- accept sock
    hSetBuffering handle NoBuffering
    forkIO $ commandProcessor handle ps host
    sockHandler sock ps

commandProcessor :: Handle -> TVar [(HostName, Port)] -> HostName -> IO ()
commandProcessor handle ps host = do
    line <- hGetLine handle
    let cmd = words line
    case (head cmd) of
        ("register")  -> registerPeer handle ps host (read $ (head $ tail cmd) :: Int)
        ("peers")  -> sendPeerList handle ps
        _        -> do hPutStrLn handle "Unknown Command"
    commandProcessor handle ps host

sendPeerList :: Handle -> TVar [(HostName, Port)] -> IO ()
sendPeerList handle ps = do
    peers <- atomically $ readTVar ps
    hPutStrLn handle (show peers)

registerPeer :: Handle -> TVar [(HostName, Port)] -> HostName -> Port -> IO ()
registerPeer handle ps host port= do
    putStrLn ("Registering peer with host: " ++ (show host) ++ "port: " ++ (show port))
    peers <- atomically $ readTVar ps
    let updatePeers = ((host, port) : peers)
    atomically $ writeTVar ps updatePeers

