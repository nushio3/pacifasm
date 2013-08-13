module Asm where

import Data.SBV

data Op = Var | Imm 
        | Not | Shl1 | Shr1 | Shr4 | Shr16 
        | And | Or | Xor | Plus
                           
                           