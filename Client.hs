
import Network 
import System.IO (hClose, hSetBuffering, hGetLine, hPutStrLn, BufferMode(..), Handle)
import Control.Concurrent (forkIO)
import System.Environment (getArgs)

type Port = Int
type SelfPort = Int
type Host = String

commandList = ["1. Register", "2. Get Peers List", "3. Send data to peer"]

connectToServer :: Host -> Port -> SelfPort-> IO ()
connectToServer host port selfPort= do
    sHandle <- connectTo host (PortNumber (fromIntegral port))
    hSetBuffering sHandle NoBuffering
    communicate sHandle selfPort
    hClose sHandle


communicate :: Handle -> SelfPort -> IO ()
communicate handle selfPort = do
    mapM_ putStrLn commandList
    cmd <- getLine
    commandExecutor (read $ cmd :: Int) handle selfPort
    communicate handle selfPort

commandExecutor cmd handle selfPort = do
    case cmd of
        1 -> register handle selfPort
        2 -> getPeersList handle
        3 -> sendDataToPeer
        _ -> putStrLn "Unknown command"

register handle selfPort = do
    hPutStrLn handle ("register " ++ (show selfPort))

getPeersList handle = do
    hPutStrLn handle "peers"
    peers <- hGetLine handle
    putStrLn peers

sendDataToPeer = do
    putStrLn "Enter peer adress "
    host <- getLine
    putStrLn "Enter peer port "
    port <- getLine
    putStrLn "Data: "
    input <- getLine
    sendTo host (PortNumber (fromIntegral (read $ port :: Int))) input


openSocket port = do
    sock <- listenOn $ PortNumber (fromIntegral port)
    putStrLn ("Opened socket on port " ++ (show port))
    sockHandler sock

sockHandler sock = do
    (handle, host, port) <- accept sock
    hSetBuffering handle NoBuffering
    forkIO $ peerConnector host port handle
    sockHandler sock

peerConnector host port handle = do
    line <- hGetLine handle
    putStrLn ("Incoming from : "++(show host)++" "++(show port) )
    putStrLn line
    peerConnector host port handle


main :: IO ()
main = do
    [serverAddr, serverPort, port] <- getArgs
    let selfPort = (read $ port :: Int)
    forkIO $ openSocket selfPort
    connectToServer serverAddr (read $ serverPort :: Int) selfPort
