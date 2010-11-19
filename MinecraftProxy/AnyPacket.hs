module MinecraftProxy.AnyPacket (AnyPacket(..)) where

import Data.Int
import Data.Binary

class (Binary p, Show p, Read p) => AnyPacket p where
  packetTag :: p -> Int8
