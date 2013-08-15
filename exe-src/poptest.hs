import Instack
import FromProp
import Data.SBV

main :: IO ()
main = synthesizeProgram 16 [Plus, And, If0, Imm 0xffffffff, Jmp] myProp

myProp :: ProgramProperty
myProp alpha beta = ite (sbvPopCount alpha .<= 1) (beta .==1) (beta .==0)


