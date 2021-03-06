{-# LANGUAGE Rank2Types, ScopedTypeVariables, ViewPatterns #-}

import Prelude hiding ((.), catch)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.UTF8 as L8
import Control.Concurrent
import Control.Exception
import Network
import System.IO
import System.Environment
import Data.Binary
import Data.Binary.Get
import Data.Char
import Control.Monad
import MinecraftProxy.Packet
import MinecraftProxy.Packets
import MinecraftProxy.AnyPacket

type PacketFilter = Packet -> ([Packet], [Packet])
serverPacketFilter :: PacketFilter
serverPacketFilter packet = inspect p
  where
    p :: AnyPacket t => Maybe t
    p = fromPacket packet
    inspect :: (forall t. AnyPacket t => Maybe t) -> ([Packet], [Packet])
    inspect otherPacket = ([packet], [])

type PacketHandler = Packet -> IO ()

clientPacketFilter :: PacketFilter
clientPacketFilter packet = inspect p
  where
    passThrough = ([packet], [])
    p :: AnyPacket t => Maybe t
    p = fromPacket packet
    inspect :: (forall t. AnyPacket t => Maybe t) -> ([Packet], [Packet])
    inspect (Just (PacketChat (PrefixString ('/':str)))) = checkSlashCommand str
    inspect otherPacket = passThrough
    checkSlashCommand str =
      let (cmd, dropWhile isSpace -> arg) = break isSpace str
      in  case cmd of
        "send"   -> readSendPacket ("Packet" ++ arg)
        "return" -> readReturnPacket ("Packet" ++ arg)
        "take"   -> parseItemRequest arg
        _        -> passThrough
    readSendPacket s = case readPacket s of
      Left  err       -> ([], [err])
      Right newPacket -> ([newPacket], [])
    readReturnPacket s = case readPacket s of
      Left  err       -> ([], [err])
      Right newPacket -> ([], [newPacket])
    readPacket s = case reads s of
      [(newPacket, "")] -> Right newPacket
      _                 -> Left (Packet . PacketChat . PrefixString $ "error: Packet: no parse")
    parseItemRequest str = ([], [makePacket])
      where
        makePacket = if itemID /= -1
                       then Packet $ PacketAddToInventory itemID count damage
                       else Packet . PacketChat . PrefixString $ "error: take: no item id"
        (itemID, count, damage) = parseNums
        parseNums = case reads str of
                      []               -> (-1, 0, 0)
                      [(itemID, rest)] -> case reads rest of
                                            []               -> (itemID, 1, 0)
                                            [(count, rest')] -> case reads rest' of
                                                                  []                 -> (itemID, count, 0)
                                                                  [(damage, rest'')] -> (itemID, count, damage)


clientListener, serverListener :: Handle -> PacketHandler -> PacketHandler -> Chan (Maybe String) -> IO ()
clientListener client toClnt toSrv consoleChan = connectionListener clientPacketFilter "client" client toSrv toClnt consoleChan
serverListener server toClnt toSrv consoleChan = connectionListener serverPacketFilter "server" server toClnt toSrv consoleChan

connectionListener :: PacketFilter -> String -> Handle -> PacketHandler -> PacketHandler -> Chan (Maybe String) -> IO ()
connectionListener handler prefix handle onHandler backHandler consoleChan = do
    say "running"
    readData `catch` report
    writeChan consoleChan Nothing
  where
    report :: SomeException -> IO ()
    report e = say (show e)
    readData = L.hGetContents handle >>= dealWith
    dealWith str
      | L.null str  = return ()
      | otherwise = do
        result <- try . evaluate $ runGetState (get :: Get Packet) str 0
        case result of 
          Left (e :: SomeException) -> do
            report e
            -- this might force the interleaved IO when printed to stdout,
            -- possibly hanging?
            say $ "buffer contents: " ++ show (L8.toString str)
          Right (p, rest, consumed) -> do
            checkPacket (L.take consumed str) p
            handlePacket p
            dealWith rest

    say = writeChan consoleChan . Just . ((prefix ++ ": ") ++)

    handlePacket p = do
      let (onwards, back) = handler p
      forM onwards onHandler
      forM back    backHandler

    checkPacket chunk p = do
      let recoded = encode p
      when (chunk /= recoded) $ do
        say "parse error:"
        say (show p)
        say ("received: " ++ show chunk)
        say ("recoded: " ++ show recoded)

main :: IO ()
main = withSocketsDo $ do
    (\x -> case x of
          [hostName, portNumber] -> [hostName, portNumber]
          [hostName]             -> [hostName, "25565"]
      -> [hostName, portNumber]) <- getArgs
    listener <- listenOn (PortNumber 25565)
    (client, _, _) <- accept listener
    sClose listener
    server <- connectTo hostName (PortNumber . fromIntegral . read $ portNumber)
    hSetBuffering client NoBuffering
    hSetBuffering server NoBuffering
    (consoleChan :: Chan (Maybe String)) <- newChan
    (clientChan  :: Chan Packet)         <- newChan
    (serverChan  :: Chan Packet)         <- newChan
    serverThread <- forkIO $ serverListener server (writeChan clientChan) (writeChan serverChan) consoleChan
    clientThread <- forkIO $ clientListener client (writeChan clientChan) (writeChan serverChan) consoleChan
    forkIO $ sendLoop clientChan client
    forkIO $ sendLoop serverChan server
    sayLoop consoleChan
    killThread serverThread
    killThread clientThread
  where
    sendLoop chan handle = forever $ do
      packet <- readChan chan
      L.hPut handle (encode packet)
    sayLoop chan =
      let go = readChan chan >>= \msg -> case msg of
                 Just s -> putStrLn (take 160 s) >> hFlush stdout >> go
                 Nothing -> putStrLn "exitting"
      in go
