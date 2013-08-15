import Instack
import FromProp
import Data.SBV

main :: IO ()
main = synthesizeProgram 1 [Plus, If0, Imm 42, Jmp] myProp

myProp :: ProgramProperty
myProp alpha beta = beta .== ite (alpha .==0) 42 (2*alpha)

