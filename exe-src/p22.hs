import Instack
import FromProp
import Data.SBV

main :: IO ()
main = synthesizeProgram 10 0 [Plus, Shr1, Shr4, Shr16, Or, Imm 0xffffffff, Imm 1, Jmp] myProp

-- round up to the next highest power of 2

myProp :: ProgramProperty
myProp alpha beta =
  ite (msb alpha ||| alpha.==0) (beta .==0) (sbvPopCount beta .==1 &&& beta .<= 2*alpha &&& beta .> alpha)

