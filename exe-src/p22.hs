import Instack
import FromProp
import Data.SBV

main :: IO ()
main = synthesizeProgram [Plus, Shr1, Shr4, Shr16, Or, Imm, Jmp] myProp

myProp :: ProgramProperty
myProp alpha beta =
  ite (msb alpha) (beta .==0) (sbvPopCount beta .==1 &&& beta .<= 2*alpha &&& beta .> alpha)

