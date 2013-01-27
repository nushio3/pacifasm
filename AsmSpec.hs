import           Data.Bits
import qualified Data.SBV as SBV
import           Data.SBV ((.==), (.<), (.<=), (&&&),(./=),(==>))
import           Test.Hspec

import Asm

main = hspec spec

shouldProve :: SBV.Predicate -> Expectation
shouldProve theorem = do
  res <- SBV.prove theorem            
  show res `shouldBe` "Q.E.D."

allEqual xs ys = SBV.bAnd $ zipWith (.==) xs ys

spec :: Spec
spec = do
  describe "Nop" $ do
    it "does nothing." $ do
      shouldProve $ do
        rs0 <- symbolicRegs          
        rs1 <- exec (toHex Nop) rs0
        return $ allEqual rs0 rs1

  describe "Iml" $ do
    it "sets the last 4 bits of e0x." $ do
      shouldProve $ do
        rs0 <- symbolicRegs       
        n <- SBV.sWord8 "n"   
        SBV.constrain $ 0 .<= n &&& n .< nReg
        rs1 <- exec (toHex $ Iml n) rs0
        let (rs0h: rs0t) = rs0
            rs1' = ((rs0h .&. 0xf0) .|. n) : rs0t
        return $ allEqual rs1' rs1
    
  describe "Imh" $ do
    it "sets the high 4 bits of e0x." $ do
      shouldProve $ do
        rs0 <- symbolicRegs       
        n <- SBV.sWord8 "n"   
        SBV.constrain $ 0 .<= n &&& n .< nReg
        rs1 <- exec (toHex $ Imh n) rs0
        let (rs0h: rs0t) = rs0
            rs1' = ((rs0h .&. 0x0f) .|. n*16) : rs0t
        return $ allEqual rs1' rs1
    
  describe "Cpy" $ do
    it "copies the lhs to e0x." $ do
      shouldProve $ do
        rs0 <- symbolicRegs       
        n <- SBV.sWord8 "n"   
        undef <- SBV.sWord8 "undefined"   
        SBV.constrain $ 0 .<= n &&& n .< nReg
        rs1 <- exec (toHex $ Cpy n) rs0
        let (rs0h: rs0t) = rs0
            rs1' = (SBV.select rs0 undef n) : rs0t
        return $ allEqual rs1' rs1
    
  describe "Xch" $ do
    it "exchanges the lhs and e0x." $ do
      shouldProve $ do
        rs0 <- symbolicRegs       
        n <- SBV.sWord8 "n"   
        n'<- SBV.sWord8 "n'"   
        undef <- SBV.sWord8 "undefined"   
        undef' <- SBV.sWord8 "undefined'"   
        SBV.constrain $ 0 .<= n &&& n .< nReg
        SBV.constrain $ 0 .<= n' &&& n' .< nReg
        rs1 <- exec (toHex $ Xch n) rs0
        let z :: SReg
            z = 0
        return $ (SBV.select rs1 undef z .== SBV.select rs0 undef' n) &&&
                 (SBV.select rs1 undef n .== SBV.select rs0 undef' z) &&&
                 ((n' ./= n &&& n' ./= z) ==> 
                 (SBV.select rs1 undef n' .== SBV.select rs0 undef' n'))
       
       

