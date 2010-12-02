{-# LANGUAGE Rank2Types, ViewPatterns, ScopedTypeVariables #-}

module MinecraftProxy.Packets (
  PrefixString (..),
  PrefixByteArray (..),
  BlockChangeArray (..),
  BlockOffsetsArray (..),
  InventoryArray (..),
  Float64be (..),
  Float32be (..),
  packetTypes
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UTF8
import Control.Monad
import Control.Applicative
import Data.Array.IArray
import Data.Binary
import Data.Binary.IEEE754
import Data.Int
import Data.Binary.Get
import Data.Binary.Put
import Prelude hiding ((.))

(.) :: Functor f => (a -> b) -> f a -> f b
(.) = fmap
infixr 9 .

newtype PrefixString              = PrefixString String deriving (Show, Read)
newtype PrefixByteArray sizeType  = PrefixByteArray B.ByteString deriving (Show, Read)
newtype BlockChangeArray          = BlockChangeArray (Array Int Int16, Array Int Int8, Array Int Int8) deriving (Show, Read)
newtype BlockOffsetsArray         = BlockOffsetsArray (Array Int (Int8, Int8, Int8)) deriving (Show, Read)
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

instance Binary BlockOffsetsArray where
  get = do
    (fromIntegral -> len) <- get :: Get Int32
    BlockOffsetsArray . listArray (0, len - 1) . replicateM len get
  put (BlockOffsetsArray offsetsArray) = do
    let (_, hi) = bounds offsetsArray
    put (fromIntegral hi + 1 :: Int32)
    mapM_ put (elems offsetsArray)

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

packetTypes :: [(String, Integer, [String])]
packetTypes = [
    ("KeepAlive"        , 0x00 , []),
    ("Login"            , 0x01 , ["Int32", "PrefixString", "PrefixString", "Int64", "Int8"]),
    ("Handshake"        , 0x02 , ["PrefixString"]),
    ("Chat"             , 0x03 , ["PrefixString"]),
    ("UpdateTime"       , 0x04 , ["Int64"]),
    ("PlayerInventory"  , 0x05 , ["Int32", "InventoryArray"]),
    ("SpawnPosition"    , 0x06 , ["Int32", "Int32", "Int32"]),
    ("UseEntity"        , 0x07 , ["Int32", "Int32", "Bool"]),
    ("PlayerHealth"     , 0x08 , ["Int8"]),
    ("PlayerRespawn"    , 0x09 , []),
    ("Flying"           , 0x0a , ["Bool"]),
    ("PlayerPosition"   , 0x0b , ["Float64be", "Float64be", "Float64be", "Float64be", "Bool"]),
    ("PlayerLook"       , 0x0c , ["Float32be", "Float32be", "Bool"]),
    ("PlayerMoveLook"   , 0x0d , ["Float64be", "Float64be", "Float64be", "Float64be", "Float32be", "Float32be", "Bool"]),
    ("BlockDig"         , 0x0e , ["Int8", "Int32", "Int8", "Int32", "Int8"]),
    ("Place"            , 0x0f , ["Int16", "Int32", "Int8", "Int32", "Int8"]),
    ("ItemSwitch"       , 0x10 , ["Int32", "Int16"]),
    ("AddToInventory"   , 0x11 , ["Int16", "Int8", "Int16"]),
    ("ArmAnimation"     , 0x12 , ["Int32", "Word8"]),
    ("NamedEntitySpawn" , 0x14 , ["Int32", "PrefixString", "Int32", "Int32", "Int32", "Int8", "Int8", "Int16"]),
    ("PickupSpawn"      , 0x15 , ["Int32", "Int16", "Int8", "Int32", "Int32", "Int32", "Int8", "Int8", "Int8"]),
    ("CollectItem"      , 0x16 , ["Int32", "Int32"]),
    ("VehicleSpawn"     , 0x17 , ["Int32", "Int8", "Int32", "Int32", "Int32"]),
    ("MobSpawn"         , 0x18 , ["Int32", "Int8", "Int32", "Int32", "Int32", "Int8", "Int8"]),
    ("EntityVelocity"   , 0x1c , ["Int32", "Int16", "Int16", "Int16"]),
    ("DestroyEntity"    , 0x1d , ["Int32"]),
    ("Entity"           , 0x1e , ["Int32"]),
    ("EntityMove"       , 0x1f , ["Int32", "Int8", "Int8", "Int8"]),
    ("EntityLook"       , 0x20 , ["Int32", "Int8", "Int8"]),
    ("EntityMoveLook"   , 0x21 , ["Int32", "Int8", "Int8", "Int8", "Int8", "Int8"]),
    ("EntityTeleport"   , 0x22 , ["Int32", "Int32", "Int32", "Int32", "Int8", "Int8"]),
    ("EntityDamage"     , 0x26 , ["Int32", "Int8"]),
    ("AttachEntity"     , 0x27 , ["Int32", "Int32"]),
    ("PreChunk"         , 0x32 , ["Int32", "Int32", "Bool"]),
    ("MapChunk"         , 0x33 , ["Int32", "Int16", "Int32", "Int8", "Int8", "Int8", "PrefixByteArray32"]),
    ("MultiBlockChange" , 0x34 , ["Int32", "Int32", "BlockChangeArray"]),
    ("BlockChange"      , 0x35 , ["Int32", "Int8", "Int32", "Int8", "Int8"]),
    ("ComplexEntity"    , 0x3b , ["Int32", "Int16", "Int32", "PrefixByteArray16"]),
    ("Explosion"        , 0x3c , ["Float64be", "Float64be", "Float64be", "Float32be", "BlockOffsetsArray"]),
    ("Disconnect"       , 0xff , ["PrefixString"])
  ]
