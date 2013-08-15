import Instack
import FromProp
import Data.SBV

main :: IO ()
main = synthesizeProgram 0
 [Plus, And, Shr1 , Shr4 , Shr16, 
  Imm 0x55555555, Imm 0x33333333, Imm 0x0f0f0f0f, Imm 0x00ff00ff, Imm 0x0000ffff, 
  Jmp] myProp

myProp :: ProgramProperty
myProp alpha beta = beta .== extend (extend (sbvPopCount alpha))


