module FromProp where

import Control.Lens
import Control.Monad
import Instack

import Data.SBV
import Text.Printf

myProp :: SVal -> SVal -> SBool
myProp alpha beta = beta .== ite (alpha .==0) 42 (2*alpha)


fromExamples :: Int -> [Op] -> [(CVal,CVal)] -> IO (Maybe CProgram)
fromExamples size instSet examples = do
  ret <- sat $ do
    prog <- genProgram size instSet
    let sExamples = map (both %~ fromIntegral) examples
        runTags = flip map examples
          (\(a,b) -> printf "(%d->%d)" a b)
    thms <- zipWithM (behave prog) runTags sExamples
    return $ bAnd thms
  return $ readProgram size instSet (show ret)

proveOrExample :: (SVal -> SVal -> SBool) -> CProgram -> IO (Maybe (CVal, CVal))
proveOrExample prop cProg = do
  ret <- prove $ \alpha ->
    prop alpha (sEval cProg alpha)
  print ret
  return Nothing

testMain :: IO ()
testMain = do
  mayProg <- fromExamples 5 [Plus, If0, Imm, Jmp]
          [(0,42),(1,2),(3,6),(50,100)]
  case mayProg of
    Nothing -> putStrLn "no way!"
    Just prog -> do
      putStrLn $ prettyPrint $ prog
      proveOrExample myProp prog
      return ()
