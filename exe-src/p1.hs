import Instack
import FromProp
import Data.SBV

main :: IO ()
main = synthesizeProgram [Plus, And, Imm, Jmp] myProp

-- turn off the rightmost 1 bit

myProp :: ProgramProperty
myProp alpha beta =
  beta .== fromBitsLE [ite (bOr $ take n bits) (bits!!n) false |n <- [0..31]]
  where
    bits = blastLE alpha

