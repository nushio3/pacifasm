import Instack
import FromProp
import Data.SBV

main :: IO ()
main = synthesizeProgramDef [Plus, And, If0, Imm 1, Imm 0xffffffff, Jmp] myProp

myProp :: ProgramProperty
myProp alpha beta = ite (sbvPopCount alpha .<= 1) (beta .==0) (beta .==1)


