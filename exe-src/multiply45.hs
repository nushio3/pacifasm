import Instack
import FromProp
import Data.SBV

main :: IO ()
main = synthesizeProgram [Plus, Shl1, Jmp] myProp

myProp :: ProgramProperty
myProp alpha beta = beta .== alpha*45


