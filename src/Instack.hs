{-# LANGUAGE TemplateHaskell, TupleSections #-}

module Instack where

import Control.Applicative
import Control.Monad
import Control.Lens hiding ((.<))
import Control.Lens.TH 
import Data.SBV
import Text.Printf

type SInst = SWord8
type SVal = SWord32

data Op 
  = Imm | Jmp 
  | Not | Shl1 | Shr1 | Shr4 | Shr16
  | And | Or | Xor | Plus 
  | If0
    
data Arity 
  = Unary 
  | Special SWord32 
  | Binary SWord32 
  | Triary SWord16 SWord16    

parseArg :: Op -> SVal -> Arity
parseArg Imm x = Special x
parseArg Jmp x = Binary x
parseArg Not _ = Unary
parseArg Shl1 _ = Unary
parseArg Shr1 _ = Unary
parseArg Shr4 _ = Unary
parseArg Shr16 _ = Unary
parseArg And x = Binary x
parseArg Or x = Binary x
parseArg Xor x = Binary x
parseArg Plus x = Binary x
parseArg If0 x = let (y,z) = split x in Triary y z


data ReservedOp = Alpha

data SProgram = SProgram
  { _instructionSet ::[Op]
  , _programLines :: [Either ReservedOp (SInst, SVal)]
  }
  
makeLenses ''SProgram
  
genProgram :: Int -> [Op] -> Symbolic SProgram
genProgram lnSize instSet = do
  let instSetSize = length instSet
  pls <- forM [0..lnSize - 1] $ \ln -> do
    if ln==0 
      then return $ Left Alpha
      else 
        do 
          inst <- exists $ printf "inst-%d" ln
          arg  <- exists $ printf "arg-%d" ln
          constrain $ inst .< fromIntegral instSetSize
          return $ Right (inst,arg)
  return $ SProgram instSet pls
  
behave :: SProgram -> (SVal, SVal) -> Symbolic SBool
behave prog (alpha, beta) = do
  let progLines = prog^.programLines 
      instSet = prog^.instructionSet
      lnSize = length progLines
  retVars <- forM [0..lnSize-1] $ \ln -> do
    exists $ printf "val-%d" ln
  thms <- forM (zip3 [(0::Int)..] (retVars :: [SWord32]) progLines) $ \(ln, ret, pl) ->
    case pl of
      Left Alpha -> constrain $ ret .== alpha
      Right (inst, arg) -> constrain $ 
        ret .== select (map retCand instSet) 0 inst
        where
          retVarsL = take ln retVars
          iVal = select retVarsL 0 (fromIntegral $ ln - 1 :: SWord8)
          xVal = select retVarsL 0 arg
          (aVal, bVal) = let (a,b) = split arg in
                             (select retVarsL 0 (extend(a::SWord16))
                             ,select retVarsL 0 (extend(b::SWord16)))
          retCand op = case op of
            Imm -> arg
            Jmp -> xVal
            Not -> complement iVal
            Shl1 -> shiftL iVal 1
            Shr1 -> shiftR iVal 1
            Shr4 -> shiftR iVal 4
            Shr16 -> shiftR iVal 16
            And -> iVal .&. xVal
            Or -> iVal .|. xVal
            Xor -> iVal `xor` xVal
            Plus -> iVal + xVal
            If0 -> ite (iVal.==0) aVal bVal
  return $ last retVars .== beta
        
    
testMain :: IO ()    
testMain = do
  ret <- sat $ do
    prog <- genProgram 5 [Plus, If0, Imm, Jmp]
    thms <- mapM (behave prog)
--      [(0,42),(1,3),(3,9),(50,150)]
      [(0,42),(1,2),(3,6),(50,100)]    
    return $ bAnd $ thms
  print ret
  
{- 
Expected

0 Alpha
1 Plus 0 
2 Imm 42
3 Jmp 0
4 If0 2 1

Actual

0 Alpha       -- x0 = alpha
1 Imm   42    -- x1 = 42
2 If0   0  0  -- x2 = alpha
3 If0   1  0  -- x3 = if alpha==0 then x1 else x0
4 Plus  2     -- x3 + x2

-}  