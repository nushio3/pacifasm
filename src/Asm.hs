{-# LANGUAGE TemplateHaskell #-}
module Asm where

import Control.Lens
import Control.Lens.TH
import Control.Monad
import Data.Bits
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
genProgram progLen opSet0 = do
  let opSize = length opSet0
  addrss <- forM [0..progLen-1] $ \ln -> do
    addrs <- sequence [ exists $ printf "addr-%d-%d" ln i | i<-[0..2::Int]]
    forM addrs $ \addr -> when (ln >=1) $ constrain $ 
                         addr .< fromIntegral ln
    return addrs
  opCodes <- forM [0..progLen-1] $ \ln -> do
    if ln==0 then return 0
             else exists $ printf "op-%d" ln
  forM opCodes $ \opCode -> constrain $ opCode .<= fromIntegral opSize
  return $ SProgram opSet0 opCodes addrss
  
behave :: SProgram -> (SWord32 , SWord32) -> Symbolic SBool
behave prog (alpha, beta) = do
  let progLen = length $ opCodes
      opCodes = prog^.opCodeList
      addrss = prog^.addrList
  retVals <- forM [0..progLen-1] $ \i ->
    exists $ printf "ret-%d" i
    
  let 
    semantic :: SVar -> SOpCode -> [SAddr] -> SBool
    semantic ret opc addrs = select cands false opc
      where
        [a,b,c] = map (select retVals 0) addrs 
        cands = flip map (prog^.opSet) $ \op -> case op of
          Var  -> ret .== alpha
          C0   -> ret .== 0
          C1   -> ret .== 1
          Not  -> ret .== complement a
          Shl1 -> ret .== shiftL a 1
          Shr1 -> ret .== shiftR a 1
          Shr4 -> ret .== shiftR a 4
          Shr16-> ret .== shiftR a 16
          And  -> ret .== a .&. b
          Or   -> ret .== a .|. b
          Xor  -> ret .== a`xor`b
          Plus -> ret .== a  +  b
          If0  -> ret .== ite (a.==0) b c

    semThm = bAnd $ zipWith3 semantic retVals opCodes addrss 
  return $ 
    semThm &&&
    (last retVals .== beta)
    

testMain :: IO ()  
testMain = do
  res <- sat $ do
    prog <- genProgram 4 [Var, C1, Plus, If0]
    fmap bAnd $ mapM (behave prog)
      [(0,1), (3,6), (100,200)]
  print res