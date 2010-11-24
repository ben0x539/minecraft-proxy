{-# LANGUAGE Rank2Types, ExistentialQuantification, ViewPatterns, StandaloneDeriving, TemplateHaskell, ScopedTypeVariables #-}

module MinecraftProxy.Packet where

import Prelude hiding ((.))
import MinecraftProxy.PacketTH
import MinecraftProxy.AnyPacket
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Binary.IEEE754
import Data.Int
import Data.Char
import Data.List
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UTF8
import Control.Applicative
import Data.Array.IArray
import Control.Monad
import Unsafe.Coerce
import Debug.Trace

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

fromPacket :: forall p . AnyPacket p => Packet -> Maybe p
fromPacket (Packet q) = guard (packetTag q == packetTag p) >> Just p
  where
    p = unsafeCoerce q :: p

type PrefixByteArray16 = PrefixByteArray Int16
type PrefixByteArray32 = PrefixByteArray Int32

$(definePacket "KeepAlive"        0x00 [])
$(definePacket "Login"            0x01 ["Int32", "PrefixString", "PrefixString", "Int64", "Int8"])
$(definePacket "Handshake"        0x02 ["PrefixString"])
$(definePacket "Chat"             0x03 ["PrefixString"])
$(definePacket "UpdateTime"       0x04 ["Int64"])
$(definePacket "PlayerInventory"  0x05 ["Int32", "InventoryArray"])
$(definePacket "SpawnPosition"    0x06 ["Int32", "Int32", "Int32"])
$(definePacket "UseEntity"        0x07 ["Int32", "Int32", "Bool"])
$(definePacket "PlayerHealth"     0x08 ["Int8"])
$(definePacket "PlayerRespawn"    0x09 [])
$(definePacket "Flying"           0x0a ["Bool"])
$(definePacket "PlayerPosition"   0x0b ["Float64be", "Float64be", "Float64be", "Float64be", "Bool"])
$(definePacket "PlayerLook"       0x0c ["Float32be", "Float32be", "Bool"])
$(definePacket "PlayerMoveLook"   0x0d ["Float64be", "Float64be", "Float64be", "Float64be", "Float32be", "Float32be", "Bool"])
$(definePacket "BlockDig"         0x0e ["Int8", "Int32", "Int8", "Int32", "Int8"])
$(definePacket "Place"            0x0f ["Int16", "Int32", "Int8", "Int32", "Int8"])
$(definePacket "ItemSwitch"       0x10 ["Int32", "Int16"])
$(definePacket "AddToInventory"   0x11 ["Int16", "Int8", "Int16"])
$(definePacket "ArmAnimation"     0x12 ["Int32", "Word8"])
$(definePacket "NamedEntitySpawn" 0x14 ["Int32", "PrefixString", "Int32", "Int32", "Int32", "Int8", "Int8", "Int16"])
$(definePacket "PickupSpawn"      0x15 ["Int32", "Int16", "Int8", "Int32", "Int32", "Int32", "Int8", "Int8", "Int8"])
$(definePacket "CollectItem"      0x16 ["Int32", "Int32"])
$(definePacket "VehicleSpawn"     0x17 ["Int32", "Int8", "Int32", "Int32", "Int32"])
$(definePacket "MobSpawn"         0x18 ["Int32", "Int8", "Int32", "Int32", "Int32", "Int8", "Int8"])
$(definePacket "EntityVelocity"   0x1c ["Int32", "Int16", "Int16", "Int16"])
$(definePacket "DestroyEntity"    0x1d ["Int32"])
$(definePacket "Entity"           0x1e ["Int32"])
$(definePacket "EntityMove"       0x1f ["Int32", "Int8", "Int8", "Int8"])
$(definePacket "EntityLook"       0x20 ["Int32", "Int8", "Int8"])
$(definePacket "EntityMoveLook"   0x21 ["Int32", "Int8", "Int8", "Int8", "Int8", "Int8"])
$(definePacket "EntityTeleport"   0x22 ["Int32", "Int32", "Int32", "Int32", "Int8", "Int8"])
$(definePacket "EntityDamage"     0x26 ["Int32", "Int8"])
$(definePacket "AttachEntity"     0x27 ["Int32", "Int32"])
$(definePacket "PreChunk"         0x32 ["Int32", "Int32", "Bool"])
$(definePacket "MapChunk"         0x33 ["Int32", "Int16", "Int32", "Int8", "Int8", "Int8", "PrefixByteArray32"])
$(definePacket "MultiBlockChange" 0x34 ["Int32", "Int32", "BlockChangeArray"])
$(definePacket "BlockChange"      0x35 ["Int32", "Int8", "Int32", "Int8", "Int8"])
$(definePacket "ComplexEntity"    0x3b ["Int32", "Int16", "Int32", "PrefixByteArray16"])
$(definePacket "Disconnect"       0xff ["PrefixString"])

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
        _ | tag == packetTag (undefined :: PacketUseEntity)        -> Packet . (get :: Get PacketUseEntity)
        _ | tag == packetTag (undefined :: PacketPlayerHealth)     -> Packet . (get :: Get PacketPlayerHealth)
        _ | tag == packetTag (undefined :: PacketPlayerRespawn)    -> Packet . (get :: Get PacketPlayerRespawn)
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
        _ | tag == packetTag (undefined :: PacketEntityVelocity)   -> Packet . (get :: Get PacketEntityVelocity)
        _ | tag == packetTag (undefined :: PacketDestroyEntity)    -> Packet . (get :: Get PacketDestroyEntity)
        _ | tag == packetTag (undefined :: PacketEntity)           -> Packet . (get :: Get PacketEntity)
        _ | tag == packetTag (undefined :: PacketEntityMove)       -> Packet . (get :: Get PacketEntityMove)
        _ | tag == packetTag (undefined :: PacketEntityLook)       -> Packet . (get :: Get PacketEntityLook)
        _ | tag == packetTag (undefined :: PacketEntityMoveLook)   -> Packet . (get :: Get PacketEntityMoveLook)
        _ | tag == packetTag (undefined :: PacketEntityTeleport)   -> Packet . (get :: Get PacketEntityTeleport)
        _ | tag == packetTag (undefined :: PacketEntityDamage)     -> Packet . (get :: Get PacketEntityDamage)
        _ | tag == packetTag (undefined :: PacketAttachEntity)     -> Packet . (get :: Get PacketAttachEntity)
        _ | tag == packetTag (undefined :: PacketPreChunk)         -> Packet . (get :: Get PacketPreChunk)
        _ | tag == packetTag (undefined :: PacketMapChunk)         -> Packet . (get :: Get PacketMapChunk)
        _ | tag == packetTag (undefined :: PacketMultiBlockChange) -> Packet . (get :: Get PacketMultiBlockChange)
        _ | tag == packetTag (undefined :: PacketBlockChange)      -> Packet . (get :: Get PacketBlockChange)
        _ | tag == packetTag (undefined :: PacketComplexEntity)    -> Packet . (get :: Get PacketComplexEntity)
        _ | tag == packetTag (undefined :: PacketDisconnect)       -> Packet . (get :: Get PacketDisconnect)
        _ -> trace (show tag) undefined
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
          _ | startsWith "PacketUseEntity"        -> makePacket . (reads :: ReadS PacketUseEntity)
          _ | startsWith "PacketPlayerHealth"     -> makePacket . (reads :: ReadS PacketPlayerHealth)
          _ | startsWith "PacketPlayerRespawn"    -> makePacket . (reads :: ReadS PacketPlayerRespawn)
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
          _ | startsWith "PacketEntityVelocity"   -> makePacket . (reads :: ReadS PacketEntityVelocity)
          _ | startsWith "PacketDestroyEntity"    -> makePacket . (reads :: ReadS PacketDestroyEntity)
          _ | startsWith "PacketEntity"           -> makePacket . (reads :: ReadS PacketEntity)
          _ | startsWith "PacketEntityMove"       -> makePacket . (reads :: ReadS PacketEntityMove)
          _ | startsWith "PacketEntityLook"       -> makePacket . (reads :: ReadS PacketEntityLook)
          _ | startsWith "PacketEntityMoveLook"   -> makePacket . (reads :: ReadS PacketEntityMoveLook)
          _ | startsWith "PacketEntityTeleport"   -> makePacket . (reads :: ReadS PacketEntityTeleport)
          _ | startsWith "PacketEntityDamage"     -> makePacket . (reads :: ReadS PacketEntityDamage)
          _ | startsWith "PacketAttachEntity"     -> makePacket . (reads :: ReadS PacketAttachEntity)
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
