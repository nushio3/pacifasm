import FromProp

import Data.SBV

main :: IO ()
main = synthesizeProgram [Plus, And, Shr1 , Shr4 , Shr16, Imm, Jmp] myProp

myProp :: ProgramProperty
myProp alpha beta = beta .== extend (extend (sbvPopCount alpha))


