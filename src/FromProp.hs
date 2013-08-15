{-# LANGUAGE MultiWayIf #-}
module FromProp where

import Control.Lens
import Control.Monad
import Data.List
import Instack

import Data.SBV
import System.Random
import Text.Printf

type ProgramProperty = SVal -> SVal -> SBool



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

proveOrExampleMasked :: ProgramProperty -> CProgram -> CVal -> IO (Maybe (CVal, CVal))
proveOrExampleMasked prop cProg mask = do
  msg <- fmap show $ prove $ do
    alpha <- forall "alpha"
    return $ (alpha .<= fromIntegral mask) ==> prop alpha (sEval cProg alpha)
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



synthesizeProgram :: Int -> [Op] -> ProgramProperty -> IO ()
synthesizeProgram defaultExSize instSet prop = do
  exs <- getExamples defaultExSize
  go 30 exs
  where
  getExamples :: Int -> IO [(CVal,CVal)]
  getExamples size = forM [0..size-1] $ \n -> do
    alpha0 <- randomIO
    let alpha = (alpha0 .&. complement (fromIntegral $ size-1)) .|. (fromIntegral n)
    msg2 <- fmap show $ sat $ do
      beta <- exists "beta"
      return $ prop (fromIntegral alpha) beta
    let beta = read $ (!!2) $ words $ head $ filter ("beta"`isInfixOf`) $ lines msg2
    return (alpha,beta)



  go :: Int -> [(CVal,CVal)] -> IO ()
  go size examples = do
    printf "testing %d %s\n" size (show examples)
    mayProg <- fromExamples size instSet examples
    case mayProg of
      Nothing -> do
        printf "no program of size %d and instSet %s satisfies the property.\n" size (show instSet)
        exs <- getExamples defaultExSize
        go  (size+1) exs
      Just prog -> do
        putStrLn "candidate program obtained:"
        putStrLn $ prettyPrint $ prog
        putStrLn "testing for property:"
        go2 size examples 1 prog

  go2 size examples mask prog = do
    mayCE <- proveOrExampleMasked prop prog mask
    case mayCE of
      Nothing | mask == maxBound -> do
        putStrLn "program synthesized:"
        putStrLn $ prettyPrint prog
      Nothing -> go2 size examples (2*mask+1) prog
      Just cePair -> do
        printf "counter example found: %s\n" (show cePair)
        go size (cePair:examples)
