module FromProp where

import Instack

import Data.SBV

myProp :: SVal -> SVal -> SBool
myProp alpha beta = beta .== ite (alpha .==0) 42 (2*alpha)