{-# OPTIONS_GHC -fth -ddump-splices #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module MinecraftProxy.PacketTH (definePacket) where

import Language.Haskell.TH
import Data.Word
import Data.Binary
import Data.DeriveTH
import MinecraftProxy.AnyPacket

definePacket :: String -> Integer -> [String] -> Q [Dec]
definePacket baseName tag fields = do
      b <- binaryInstance dataDecl
      return $ [dataDecl] ++ b ++ [anyPacketInstance]
  where
    name = mkName ("Packet" ++ baseName)
    dataType = conT name
    dataDecl = DataD [] name [] constructor [mkName "Read", mkName "Show"]
    constructor = [NormalC name (map ((,) NotStrict . ConT . mkName) fields)]
    binaryInstance d
      | not (null fields) = deriveFromDec makeBinary d
      | otherwise         = return [InstanceD [] (AppT (ConT ''Binary) (ConT name)) [
                              ValD (VarP 'get)
                                   (NormalB (AppE (VarE 'return) (ConE name)))
                                   [],
                              FunD 'put
                                   [Clause [WildP] (NormalB (AppE (VarE 'return) (TupE [])))
                                   []]
                            ]]
    anyPacketInstance = InstanceD [] (AppT (ConT ''AnyPacket) (ConT name)) [
                          ValD (VarP 'packetTag)
                               (NormalB (AppE (VarE 'const) (LitE (IntegerL tag))))
                               []
                        ]
    -- anyPacketInstance = [d| instanceD AnyPacket $dataType where
    --                           packetTag = const $(litE (WordPrimL tag)) |]
