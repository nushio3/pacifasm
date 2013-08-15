import Instack
import FromProp
import Data.SBV

main :: IO ()
main = synthesizeProgram [Plus, And, If0, Imm, Jmp] myProp

myProp :: ProgramProperty
myProp alpha beta = ite (sbvPopCount alpha .<= 1) (beta .==1) (beta .==0)


