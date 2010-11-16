{-# LANGUAGE Rank2Types, ExistentialQuantification, ViewPatterns, StandaloneDeriving, TemplateHaskell, ScopedTypeVariables #-}

import Control.Concurrent
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Binary.IEEE754
import Data.Int
import Data.Char
import Data.List
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.UTF8 as UTF8
import Data.DeriveTH
import Control.Applicative
import Data.Array.IArray
import Prelude hiding ((.), catch)
import Control.Exception
import Control.Monad
import Network
import System.IO
import System.Environment
import Unsafe.Coerce

(.) :: Functor f => (a -> b) -> f a -> f b
(.) = fmap
infixr 9 .

newtype PrefixString              = PrefixString String deriving (Show, Read)
newtype PrefixByteArray sizeType  = PrefixByteArray B.ByteString deriving (Show, Read)
newtype BlockChangeArray          = BlockChangeArray (Array Int Int16, Array Int Int8, Array Int Int8) deriving (Show, Read)
newtype InventoryArray            = InventoryArray (Array Int (Int16, Maybe (Int8, Int16))) deriving (Show, Read)
newtype Float64be                 = Float64be Double deriving (Show, Read)
newtype Float32be                 = Float32be Float deriving (Show, Read)

instance Binary Float64be where
  get = Float64be . getFloat64be
  put (Float64be d) = putFloat64be d

instance Binary Float32be where
  get = Float32be . getFloat32be
  put (Float32be f) = putFloat32be f

instance Binary PrefixString where
  get = do
    len <- get :: Get Int16
    PrefixString . UTF8.toString . getByteString (fromIntegral len)
  put (PrefixString s) = do
    let bs = UTF8.fromString s
    put (fromIntegral $ B.length bs :: Int16)
    putByteString bs

instance forall sizeType. (Binary sizeType, Integral sizeType) => Binary (PrefixByteArray sizeType) where
  get = do
    len <- get :: Get sizeType
    PrefixByteArray . getByteString (fromIntegral len)
  put (PrefixByteArray bs) = do
    put (fromIntegral $ B.length bs :: sizeType)
    putByteString bs

instance Binary BlockChangeArray where
  get = do
    (fromIntegral -> len) <- get :: Get Int16
    (listArray (0, len-1) -> coordArray) <- replicateM len get
    (listArray (0, len-1) -> blockArray) <- replicateM len get
    (listArray (0, len-1) -> metaArray ) <- replicateM len get
    return $ BlockChangeArray (coordArray, blockArray, metaArray)
  put (BlockChangeArray (coordArray, blockArray, metaArray)) = do
    let (_, hi) = bounds coordArray
    put (fromIntegral hi + 1 :: Int16)
    mapM_ put (elems coordArray)
    mapM_ put (elems blockArray)
    mapM_ put (elems metaArray)

instance Binary InventoryArray where
  get = do
      (fromIntegral -> len) <- get :: Get Int16
      list <- replicateM len getItem
      return $ InventoryArray (listArray (0, len-1) list)
    where
      getItem = do
        itemID <- get :: Get Int16
        rest <- case itemID of
          -1 -> return Nothing
          _  -> Just . ((,) <$> get <*> get)
        return $ (itemID, rest)
  put (InventoryArray ary) = do
    let (_, hi) = bounds ary
    put (fromIntegral hi + 1 :: Int16)
    forM_ (elems ary) $ \(itemID, ~(Just (count, damage))) -> do
      put itemID
      when (itemID /= -1) $ do
        put count
        put damage

data Packet = forall p. (AnyPacket p) => Packet p

deriving instance Show Packet

class (Binary p, Show p, Read p) => AnyPacket p where
  packetTag :: p -> Int8

fromPacket :: forall p . AnyPacket p => Packet -> Maybe p
fromPacket (Packet q) = guard (packetTag q == packetTag p) >> Just p
  where
    p = unsafeCoerce q :: p

data PacketKeepAlive        = PacketKeepAlive
data PacketLogin            = PacketLogin Int32 PrefixString PrefixString Int64 Int8
data PacketHandshake        = PacketHandshake PrefixString
data PacketChat             = PacketChat PrefixString
data PacketUpdateTime       = PacketUpdateTime Int64
data PacketPlayerInventory  = PacketPlayerInventory Int32 InventoryArray
data PacketSpawnPosition    = PacketSpawnPosition Int32 Int32 Int32
data PacketFlying           = PacketFlying Bool
data PacketPlayerPosition   = PacketPlayerPosition Float64be Float64be Float64be Float64be Bool
data PacketPlayerLook       = PacketPlayerLook Float32be Float32be Bool
data PacketPlayerMoveLook   = PacketPlayerMoveLook Float64be Float64be Float64be Float64be Float32be Float32be Bool
data PacketBlockDig         = PacketBlockDig Int8 Int32 Int8 Int32 Int8
data PacketPlace            = PacketPlace Int16 Int32 Int8 Int32 Int8
data PacketItemSwitch       = PacketItemSwitch Int32 Int16
data PacketAddToInventory   = PacketAddToInventory Int16 Int8 Int16
data PacketArmAnimation     = PacketArmAnimation Int32 Bool
data PacketNamedEntitySpawn = PacketNamedEntitySpawn Int32 PrefixString Int32 Int32 Int32 Int8 Int8 Int16
data PacketPickupSpawn      = PacketPickupSpawn Int32 Int16 Int8 Int32 Int32 Int32 Int8 Int8 Int8
data PacketCollectItem      = PacketCollectItem Int32 Int32
data PacketVehicleSpawn     = PacketVehicleSpawn Int32 Int8 Int32 Int32 Int32
data PacketMobSpawn         = PacketMobSpawn Int32 Int8 Int32 Int32 Int32 Int8 Int8
data PacketDestroyEntity    = PacketDestroyEntity Int32
data PacketEntity           = PacketEntity Int32
data PacketEntityMove       = PacketEntityMove Int32 Int8 Int8 Int8
data PacketEntityLook       = PacketEntityLook Int32 Int8 Int8
data PacketEntityMoveLook   = PacketEntityMoveLook Int32 Int8 Int8 Int8 Int8 Int8
data PacketEntityTeleport   = PacketEntityTeleport Int32 Int32 Int32 Int32 Int8 Int8
data PacketPreChunk         = PacketPreChunk Int32 Int32 Bool
data PacketMapChunk         = PacketMapChunk Int32 Int16 Int32 Int8 Int8 Int8 (PrefixByteArray Int32)
data PacketMultiBlockChange = PacketMultiBlockChange Int32 Int32 BlockChangeArray
data PacketBlockChange      = PacketBlockChange Int32 Int8 Int32 Int8 Int8
data PacketComplexEntity    = PacketComplexEntity Int32 Int16 Int32 (PrefixByteArray Int16)
data PacketDisconnect       = PacketDisconnect PrefixString

deriving instance Show PacketKeepAlive
deriving instance Show PacketLogin
deriving instance Show PacketHandshake
deriving instance Show PacketChat
deriving instance Show PacketUpdateTime
deriving instance Show PacketPlayerInventory
deriving instance Show PacketSpawnPosition
deriving instance Show PacketFlying
deriving instance Show PacketPlayerPosition
deriving instance Show PacketPlayerLook
deriving instance Show PacketPlayerMoveLook
deriving instance Show PacketBlockDig
deriving instance Show PacketPlace
deriving instance Show PacketItemSwitch
deriving instance Show PacketAddToInventory
deriving instance Show PacketArmAnimation
deriving instance Show PacketNamedEntitySpawn
deriving instance Show PacketPickupSpawn
deriving instance Show PacketCollectItem
deriving instance Show PacketVehicleSpawn
deriving instance Show PacketMobSpawn
deriving instance Show PacketDestroyEntity
deriving instance Show PacketEntity
deriving instance Show PacketEntityMove
deriving instance Show PacketEntityLook
deriving instance Show PacketEntityMoveLook
deriving instance Show PacketEntityTeleport
deriving instance Show PacketPreChunk
deriving instance Show PacketMapChunk
deriving instance Show PacketMultiBlockChange
deriving instance Show PacketBlockChange
deriving instance Show PacketComplexEntity
deriving instance Show PacketDisconnect

deriving instance Read PacketKeepAlive
deriving instance Read PacketLogin
deriving instance Read PacketHandshake
deriving instance Read PacketChat
deriving instance Read PacketUpdateTime
deriving instance Read PacketPlayerInventory
deriving instance Read PacketSpawnPosition
deriving instance Read PacketFlying
deriving instance Read PacketPlayerPosition
deriving instance Read PacketPlayerLook
deriving instance Read PacketPlayerMoveLook
deriving instance Read PacketBlockDig
deriving instance Read PacketPlace
deriving instance Read PacketItemSwitch
deriving instance Read PacketAddToInventory
deriving instance Read PacketArmAnimation
deriving instance Read PacketNamedEntitySpawn
deriving instance Read PacketPickupSpawn
deriving instance Read PacketCollectItem
deriving instance Read PacketVehicleSpawn
deriving instance Read PacketMobSpawn
deriving instance Read PacketDestroyEntity
deriving instance Read PacketEntity
deriving instance Read PacketEntityMove
deriving instance Read PacketEntityLook
deriving instance Read PacketEntityMoveLook
deriving instance Read PacketEntityTeleport
deriving instance Read PacketPreChunk
deriving instance Read PacketMapChunk
deriving instance Read PacketMultiBlockChange
deriving instance Read PacketBlockChange
deriving instance Read PacketComplexEntity
deriving instance Read PacketDisconnect

instance Binary Packet where
  put (Packet p) = do
    putWord8 (fromIntegral (packetTag p))
    put p
  get = do
      tag <- fromIntegral . getWord8
      case () of
        _ | tag == packetTag (undefined :: PacketKeepAlive)        -> Packet . (get :: Get PacketKeepAlive)
        _ | tag == packetTag (undefined :: PacketLogin)            -> Packet . (get :: Get PacketLogin)
        _ | tag == packetTag (undefined :: PacketHandshake)        -> Packet . (get :: Get PacketHandshake)
        _ | tag == packetTag (undefined :: PacketChat)             -> Packet . (get :: Get PacketChat)
        _ | tag == packetTag (undefined :: PacketUpdateTime)       -> Packet . (get :: Get PacketUpdateTime)
        _ | tag == packetTag (undefined :: PacketPlayerInventory)  -> Packet . (get :: Get PacketPlayerInventory)
        _ | tag == packetTag (undefined :: PacketSpawnPosition)    -> Packet . (get :: Get PacketSpawnPosition)
        _ | tag == packetTag (undefined :: PacketFlying)           -> Packet . (get :: Get PacketFlying)
        _ | tag == packetTag (undefined :: PacketPlayerPosition)   -> Packet . (get :: Get PacketPlayerPosition)
        _ | tag == packetTag (undefined :: PacketPlayerLook)       -> Packet . (get :: Get PacketPlayerLook)
        _ | tag == packetTag (undefined :: PacketPlayerMoveLook)   -> Packet . (get :: Get PacketPlayerMoveLook)
        _ | tag == packetTag (undefined :: PacketBlockDig)         -> Packet . (get :: Get PacketBlockDig)
        _ | tag == packetTag (undefined :: PacketPlace)            -> Packet . (get :: Get PacketPlace)
        _ | tag == packetTag (undefined :: PacketItemSwitch)       -> Packet . (get :: Get PacketItemSwitch)
        _ | tag == packetTag (undefined :: PacketAddToInventory)   -> Packet . (get :: Get PacketAddToInventory)
        _ | tag == packetTag (undefined :: PacketArmAnimation)     -> Packet . (get :: Get PacketArmAnimation)
        _ | tag == packetTag (undefined :: PacketNamedEntitySpawn) -> Packet . (get :: Get PacketNamedEntitySpawn)
        _ | tag == packetTag (undefined :: PacketPickupSpawn)      -> Packet . (get :: Get PacketPickupSpawn)
        _ | tag == packetTag (undefined :: PacketCollectItem)      -> Packet . (get :: Get PacketCollectItem)
        _ | tag == packetTag (undefined :: PacketVehicleSpawn)     -> Packet . (get :: Get PacketVehicleSpawn)
        _ | tag == packetTag (undefined :: PacketMobSpawn)         -> Packet . (get :: Get PacketMobSpawn)
        _ | tag == packetTag (undefined :: PacketDestroyEntity)    -> Packet . (get :: Get PacketDestroyEntity)
        _ | tag == packetTag (undefined :: PacketEntity)           -> Packet . (get :: Get PacketEntity)
        _ | tag == packetTag (undefined :: PacketEntityMove)       -> Packet . (get :: Get PacketEntityMove)
        _ | tag == packetTag (undefined :: PacketEntityLook)       -> Packet . (get :: Get PacketEntityLook)
        _ | tag == packetTag (undefined :: PacketEntityMoveLook)   -> Packet . (get :: Get PacketEntityMoveLook)
        _ | tag == packetTag (undefined :: PacketEntityTeleport)   -> Packet . (get :: Get PacketEntityTeleport)
        _ | tag == packetTag (undefined :: PacketPreChunk)         -> Packet . (get :: Get PacketPreChunk)
        _ | tag == packetTag (undefined :: PacketMapChunk)         -> Packet . (get :: Get PacketMapChunk)
        _ | tag == packetTag (undefined :: PacketMultiBlockChange) -> Packet . (get :: Get PacketMultiBlockChange)
        _ | tag == packetTag (undefined :: PacketBlockChange)      -> Packet . (get :: Get PacketBlockChange)
        _ | tag == packetTag (undefined :: PacketComplexEntity)    -> Packet . (get :: Get PacketComplexEntity)
        _ | tag == packetTag (undefined :: PacketDisconnect)       -> Packet . (get :: Get PacketDisconnect)
      :: Get Packet

instance Read Packet where
  readsPrec _ s = findReads s
    where
      findReads = case () of
          _ | startsWith "PacketKeepAlive"        -> makePacket . (reads :: ReadS PacketKeepAlive)
          _ | startsWith "PacketLogin"            -> makePacket . (reads :: ReadS PacketLogin)
          _ | startsWith "PacketHandshake"        -> makePacket . (reads :: ReadS PacketHandshake)
          _ | startsWith "PacketChat"             -> makePacket . (reads :: ReadS PacketChat)
          _ | startsWith "PacketUpdateTime"       -> makePacket . (reads :: ReadS PacketUpdateTime)
          _ | startsWith "PacketPlayerInventory"  -> makePacket . (reads :: ReadS PacketPlayerInventory)
          _ | startsWith "PacketSpawnPosition"    -> makePacket . (reads :: ReadS PacketSpawnPosition)
          _ | startsWith "PacketFlying"           -> makePacket . (reads :: ReadS PacketFlying)
          _ | startsWith "PacketPlayerPosition"   -> makePacket . (reads :: ReadS PacketPlayerPosition)
          _ | startsWith "PacketPlayerLook"       -> makePacket . (reads :: ReadS PacketPlayerLook)
          _ | startsWith "PacketPlayerMoveLook"   -> makePacket . (reads :: ReadS PacketPlayerMoveLook)
          _ | startsWith "PacketBlockDig"         -> makePacket . (reads :: ReadS PacketBlockDig)
          _ | startsWith "PacketPlace"            -> makePacket . (reads :: ReadS PacketPlace)
          _ | startsWith "PacketItemSwitch"       -> makePacket . (reads :: ReadS PacketItemSwitch)
          _ | startsWith "PacketAddToInventory"   -> makePacket . (reads :: ReadS PacketAddToInventory)
          _ | startsWith "PacketArmAnimation"     -> makePacket . (reads :: ReadS PacketArmAnimation)
          _ | startsWith "PacketNamedEntitySpawn" -> makePacket . (reads :: ReadS PacketNamedEntitySpawn)
          _ | startsWith "PacketPickupSpawn"      -> makePacket . (reads :: ReadS PacketPickupSpawn)
          _ | startsWith "PacketCollectItem"      -> makePacket . (reads :: ReadS PacketCollectItem)
          _ | startsWith "PacketVehicleSpawn"     -> makePacket . (reads :: ReadS PacketVehicleSpawn)
          _ | startsWith "PacketMobSpawn"         -> makePacket . (reads :: ReadS PacketMobSpawn)
          _ | startsWith "PacketDestroyEntity"    -> makePacket . (reads :: ReadS PacketDestroyEntity)
          _ | startsWith "PacketEntity"           -> makePacket . (reads :: ReadS PacketEntity)
          _ | startsWith "PacketEntityMove"       -> makePacket . (reads :: ReadS PacketEntityMove)
          _ | startsWith "PacketEntityLook"       -> makePacket . (reads :: ReadS PacketEntityLook)
          _ | startsWith "PacketEntityMoveLook"   -> makePacket . (reads :: ReadS PacketEntityMoveLook)
          _ | startsWith "PacketEntityTeleport"   -> makePacket . (reads :: ReadS PacketEntityTeleport)
          _ | startsWith "PacketPreChunk"         -> makePacket . (reads :: ReadS PacketPreChunk)
          _ | startsWith "PacketMapChunk"         -> makePacket . (reads :: ReadS PacketMapChunk)
          _ | startsWith "PacketMultiBlockChange" -> makePacket . (reads :: ReadS PacketMultiBlockChange)
          _ | startsWith "PacketBlockChange"      -> makePacket . (reads :: ReadS PacketBlockChange)
          _ | startsWith "PacketComplexEntity"    -> makePacket . (reads :: ReadS PacketComplexEntity)
          _ | startsWith "PacketDisconnect"       -> makePacket . (reads :: ReadS PacketDisconnect)
          _ -> const []
      makePacket :: AnyPacket p => [(p, String)] -> [(Packet, String)]
      makePacket = map (\(a, b) -> (Packet a, b))
      startsWith prefix = prefix `isPrefixOf` s


instance Binary PacketKeepAlive where
  get = return PacketKeepAlive
  put _ = return ()
$(derive makeBinary ''PacketLogin)
$(derive makeBinary ''PacketHandshake)
$(derive makeBinary ''PacketChat)
$(derive makeBinary ''PacketUpdateTime)
$(derive makeBinary ''PacketPlayerInventory)
$(derive makeBinary ''PacketSpawnPosition)
$(derive makeBinary ''PacketFlying)
$(derive makeBinary ''PacketPlayerPosition)
$(derive makeBinary ''PacketPlayerLook)
$(derive makeBinary ''PacketPlayerMoveLook)
$(derive makeBinary ''PacketBlockDig)
$(derive makeBinary ''PacketPlace)
$(derive makeBinary ''PacketItemSwitch)
$(derive makeBinary ''PacketAddToInventory)
$(derive makeBinary ''PacketArmAnimation)
$(derive makeBinary ''PacketNamedEntitySpawn)
$(derive makeBinary ''PacketPickupSpawn)
$(derive makeBinary ''PacketCollectItem)
$(derive makeBinary ''PacketVehicleSpawn)
$(derive makeBinary ''PacketMobSpawn)
$(derive makeBinary ''PacketDestroyEntity)
$(derive makeBinary ''PacketEntity)
$(derive makeBinary ''PacketEntityMove)
$(derive makeBinary ''PacketEntityLook)
$(derive makeBinary ''PacketEntityMoveLook)
$(derive makeBinary ''PacketEntityTeleport)
$(derive makeBinary ''PacketPreChunk)
$(derive makeBinary ''PacketMapChunk)
$(derive makeBinary ''PacketMultiBlockChange)
$(derive makeBinary ''PacketBlockChange)
$(derive makeBinary ''PacketComplexEntity)
$(derive makeBinary ''PacketDisconnect)

instance AnyPacket PacketKeepAlive        where packetTag = const 0x00
instance AnyPacket PacketLogin            where packetTag = const 0x01
instance AnyPacket PacketHandshake        where packetTag = const 0x02
instance AnyPacket PacketChat             where packetTag = const 0x03
instance AnyPacket PacketUpdateTime       where packetTag = const 0x04
instance AnyPacket PacketPlayerInventory  where packetTag = const 0x05
instance AnyPacket PacketSpawnPosition    where packetTag = const 0x06
instance AnyPacket PacketFlying           where packetTag = const 0x0a
instance AnyPacket PacketPlayerPosition   where packetTag = const 0x0b
instance AnyPacket PacketPlayerLook       where packetTag = const 0x0c
instance AnyPacket PacketPlayerMoveLook   where packetTag = const 0x0d
instance AnyPacket PacketBlockDig         where packetTag = const 0x0e
instance AnyPacket PacketPlace            where packetTag = const 0x0f
instance AnyPacket PacketItemSwitch       where packetTag = const 0x10
instance AnyPacket PacketAddToInventory   where packetTag = const 0x11
instance AnyPacket PacketArmAnimation     where packetTag = const 0x12
instance AnyPacket PacketNamedEntitySpawn where packetTag = const 0x14
instance AnyPacket PacketPickupSpawn      where packetTag = const 0x15
instance AnyPacket PacketCollectItem      where packetTag = const 0x16
instance AnyPacket PacketVehicleSpawn     where packetTag = const 0x17
instance AnyPacket PacketMobSpawn         where packetTag = const 0x18
instance AnyPacket PacketDestroyEntity    where packetTag = const 0x1d
instance AnyPacket PacketEntity           where packetTag = const 0x1e
instance AnyPacket PacketEntityMove       where packetTag = const 0x1f
instance AnyPacket PacketEntityLook       where packetTag = const 0x20
instance AnyPacket PacketEntityMoveLook   where packetTag = const 0x21
instance AnyPacket PacketEntityTeleport   where packetTag = const 0x22
instance AnyPacket PacketPreChunk         where packetTag = const 0x32
instance AnyPacket PacketMapChunk         where packetTag = const 0x33
instance AnyPacket PacketMultiBlockChange where packetTag = const 0x34
instance AnyPacket PacketBlockChange      where packetTag = const 0x35
instance AnyPacket PacketComplexEntity    where packetTag = const 0x3b
instance AnyPacket PacketDisconnect       where packetTag = const 0xff

type PacketFilter = Packet -> ([Packet], [Packet])
serverPacketFilter :: PacketFilter
serverPacketFilter packet = inspect p
  where
    p :: AnyPacket t => Maybe t
    p = fromPacket packet
    inspect :: (forall t. AnyPacket t => Maybe t) -> ([Packet], [Packet])
    inspect (Just (PacketUpdateTime _)) = ([Packet $ PacketUpdateTime 0], [])
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
    readMore L.empty `catch` \(e :: SomeException) -> say (show e)
    writeChan consoleChan Nothing
  where
    readMore buffer = do
      ready <- hWaitForInput handle (-1) `catch` \(e :: SomeException) -> return True
      when (ready) $ do
        nextChunk <- L.hGetNonBlocking handle (64*1024)
        if L.null nextChunk
          then say "no data"
          else dealWith $ L.append buffer nextChunk

    dealWith str = do
      let len = L.length str
      when (len > 1024*5) $
        say ("buffer size: " ++ show (L.length str))
      result <- try (evaluate $ runGetState (get :: Get Packet) str 0)
      case result of
        Left (e :: SomeException) -> do
          readMore str
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
    [hostName, portNumber] <- getArgs
    listener <- listenOn (PortNumber 8000)
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
    hClose client
    hClose server
  where
    sendLoop chan handle = forever $ do
      packet <- readChan chan
      L.hPut handle (encode packet)
    sayLoop chan =
      let go = readChan chan >>= \msg -> case msg of
                 Just s -> putStrLn (take 160 s) >> hFlush stdout >> go
                 Nothing -> putStrLn "exitting"
      in go
