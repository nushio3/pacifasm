import Instack
import FromProp
import Data.SBV

main :: IO ()
main = synthesizeProgram 8 [Plus, And, Imm 0xffffffff, Jmp] myProp

-- turn off the rightmost 1 bit

myProp :: ProgramProperty
myProp alpha beta =
  beta .== fromBitsLE [ite (bOr $ take n bits) (bits!!n) false |n <- [0..31]]
  where
    bits = blastLE alpha

