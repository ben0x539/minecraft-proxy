{-# LANGUAGE Rank2Types, ExistentialQuantification, ViewPatterns, StandaloneDeriving, TemplateHaskell, ScopedTypeVariables #-}

module MinecraftProxy.Packet where

import Language.Haskell.TH
import MinecraftProxy.PacketTH
import MinecraftProxy.AnyPacket
import MinecraftProxy.Packets
import Data.Binary
import Data.Char
import Data.List
import Unsafe.Coerce
import Debug.Trace
import Data.Int
import Control.Monad
import Prelude hiding ((.))

(.) :: Functor f => (a -> b) -> f a -> f b
(.) = fmap
infixr 9 .

data Packet = forall p. (AnyPacket p) => Packet p

deriving instance Show Packet

fromPacket :: forall p . AnyPacket p => Packet -> Maybe p
fromPacket (Packet q) = guard (packetTag q == packetTag p) >> Just p
  where
    p = unsafeCoerce q :: p

type PrefixByteArray16 = PrefixByteArray Int16
type PrefixByteArray32 = PrefixByteArray Int32

$( fmap concat (sequence $ map (\(name, tag, fields) -> definePacket name tag fields) packetTypes) )

instance Binary Packet where
  put (Packet p) = do
    putWord8 (fromIntegral (packetTag p))
    put p
  get = do
      tag <- fromIntegral . getWord8
      $( let myType name = return $ ConT (mkName ("Packet" ++ name))
             myGuard name = normalG [| tag == packetTag (undefined :: $( myType name )) |]
             myExp   name = [| Packet . (get :: Get $( myType name )) |]
             defaultCase = uncurry (liftM2 (,)) (normalG [| otherwise |], [| trace (show tag) undefined |])
             cases = guardedB $ (map myCase packetTypes ++ [defaultCase])
             myCase (name, _, _) = uncurry (liftM2 (,)) (myGuard name, myExp name)
         in  caseE [| () |] [match wildP cases []] )
        -- _ -> trace (show tag) undefined
      :: Get Packet

instance Read Packet where
  readsPrec _ s = findReads s
    where
      findReads = $(
          let myType name = return $ ConT (mkName ("Packet" ++ name))
              myGuard name = normalG [| startsWith $( litE (StringL $ "Packet" ++ name ) ) |]
              myExp   name = [| makePacket . (reads :: ReadS $( myType name )) |]
              defaultCase = uncurry (liftM2 (,)) (normalG [| otherwise |], [| const [] |])
              myCase (name, _, _) = uncurry (liftM2 (,)) (myGuard name, myExp name)
              cases = guardedB $ (map myCase packetTypes ++ [defaultCase])
          in  caseE [| () |] [ match wildP cases [] ]
        )
      makePacket :: AnyPacket p => [(p, String)] -> [(Packet, String)]
      makePacket = map (\(a, b) -> (Packet a, b))
      startsWith prefix = prefix `isPrefixOf` s
