{-# LANGUAGE MultiWayIf #-}
module FromProp where

import Control.Lens
import Control.Monad
import Data.List
import Instack

import Data.SBV
import Text.Printf

type ProgramProperty = SVal -> SVal -> SBool

myProp :: ProgramProperty
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

proveOrExample :: ProgramProperty -> CProgram -> IO (Maybe (CVal, CVal))
proveOrExample prop cProg = do
  msg <- fmap show $ prove $ do
    alpha <- forall "alpha"
    return $ prop alpha (sEval cProg alpha)
  putStrLn msg

  if
    | "Q.E.D." `isPrefixOf` msg -> return Nothing
    | otherwise -> do
      let ceAlpha :: CVal
          ceAlpha = read $ (!!2) $ words $ head $ filter ("alpha"`isInfixOf`) $ lines msg
      msg2 <- fmap show $ sat $ do
        beta <- exists "beta"
        return $ prop (fromIntegral ceAlpha) beta
      putStrLn msg2
      let ceBeta :: CVal
          ceBeta = read $ (!!2) $ words $ head $ filter ("beta"`isInfixOf`) $ lines msg2

      return $ Just (ceAlpha,ceBeta)


synthesizeProgram :: [Op] -> ProgramProperty -> IO ()
synthesizeProgram instSet prop = go 0 [] where
  go :: Int -> [(CVal,CVal)] -> IO ()
  go size examples = do
    printf "testing %d %s\n" size (show examples)
    mayProg <- fromExamples size instSet examples
    case mayProg of
      Nothing -> do
        printf "no program of size %d and instSet %s satisfies the property.\n" size (show instSet)
        go  (size+1) []
      Just prog -> do
        putStrLn "candidate program obtained:"
        putStrLn $ prettyPrint $ prog
        putStrLn "testing for property:"
        mayCE <- proveOrExample prop prog
        case mayCE of
          Nothing -> do
            putStrLn "program synthesized:"
            putStrLn $ prettyPrint prog
          Just cePair -> do
            printf "counter example found: %s\n" (show cePair)
            go size (cePair:examples)

testMain :: IO ()
testMain = synthesizeProgram [Plus, If0, Imm, Jmp] myProp