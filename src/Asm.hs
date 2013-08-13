{-# LANGUAGE TemplateHaskell #-}
module Asm where

import Control.Lens
import Control.Lens.TH
import Control.Monad
import Data.SBV
import Text.Printf

data Op 
  = Var | C0 | C1
  | Not | Shl1 | Shr1 | Shr4 | Shr16 
  | And | Or | Xor | Plus
  | If0
                           
type SVar = SWord32                           
type SAddr = SWord8
type SOpCode = SWord8

data SProgram = SProgram 
  { _opSet :: [Op]
  , _opCodeList :: [SOpCode]
  , _addrList :: [[SAddr]]
  }

makeLenses ''SProgram

genProgram :: Int -> [Op] -> Symbolic SProgram
genProgram size opSet0 = do
  let opSize = length opSet0
  addrss <- forM [0..size-1] $ \ln -> do
    addrs <- sWord8s [ printf "addr-%d-%d" ln i | i<-[0..2::Int]]
    forM addrs $ \addr -> constrain $ addr .< fromIntegral ln
    return addrs
  opCodes <- forM [0..size-1] $ \ln -> do
    sWord8 $ printf "op-%d" ln
  forM opCodes $ \opCode -> constrain $ opCode .<= fromIntegral opSize
  return $ SProgram opSet0 opCodes addrss