import FromProp

import Data.SBV

main :: IO ()
main = synthesizeProgram [Plus, If0, Imm, Jmp] myProp

myProp :: ProgramProperty
myProp alpha beta = beta .== ite (alpha .==0) 42 (2*alpha)


--main = synthesizeProgram [Plus, And, Shr1 , Shr4 , Shr16, Imm, Jmp] myProp