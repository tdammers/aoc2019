{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE DeriveFunctor #-}
module IntcodeVM
where

import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import Control.Monad.State
import Control.Monad.Trans.Except
import Text.Printf
import Data.Maybe
import Data.List

data RunState
  = Running
  | Terminated
  | AccessViolation Int
  | InvalidOpcode Int
  | InvalidParamMode Int Int
  deriving (Show, Read, Eq, Ord)

data VM =
  VM
    { vmIP :: Int
    , vmRAM :: IntMap Int
    }
    deriving (Show)

dumpVM :: VM -> IO ()
dumpVM (VM ip ram) = do
  printf "IP: %d\n" ip
  let max = fromMaybe 0 $ fst <$> IntMap.lookupMax ram
  forM_ [0..max] $ \addr -> do
    when (addr `mod` 10 == 0) $ do
      printf "\n%04d |" addr
    maybe (printf " <NIL>") (printf " %7s" . show) (ram IntMap.!? addr)
  printf "\n"

defVM :: VM
defVM = VM 0 IntMap.empty

loadVM :: [Int] -> VM
loadVM program =
  VM 0 (IntMap.fromList $ zip [0..] program)

type MonadVM m = ExceptT RunState (StateT VM m)

peek :: Monad m => Int -> MonadVM m Int
peek addr = do
  ram <- gets vmRAM
  maybe (throwE $ AccessViolation addr) pure $ IntMap.lookup addr ram

poke :: Monad m => Int -> Int -> MonadVM m ()
poke addr val = do
  ram <- gets vmRAM
  let ram' = IntMap.insert addr val ram
  modify $ \vm -> vm { vmRAM = ram' }


numArgs :: Opcode -> Int
numArgs (OpAdd _ _) = 3
numArgs (OpMul _ _) = 3
numArgs OpTerminate = 0

data ParamMode
  = PositionMode
  | ImmediateMode
  deriving (Show, Enum, Eq, Ord, Bounded)

data Opcode
  = OpAdd ParamMode ParamMode
  | OpMul ParamMode ParamMode
  | OpTerminate
  deriving (Show, Eq, Ord)

getParamMode :: Monad m => Int -> Int -> MonadVM m ParamMode
getParamMode digit raw =
  let factor = 10 ^ (digit + 2)
      rawMode = (raw `div` factor) `mod` 10
  in case rawMode of
    0 -> pure PositionMode
    1 -> pure ImmediateMode
    x -> throwE (InvalidParamMode digit rawMode)

mkOpcode :: (Monad m) => Int -> MonadVM m Opcode
mkOpcode i = do
  case i `mod` 100 of
    1 -> OpAdd <$> getParamMode 0 i <*> getParamMode 1 i
    2 -> OpMul <$> getParamMode 0 i <*> getParamMode 1 i
    99 -> pure OpTerminate
    x -> throwE (InvalidOpcode i)

getParam :: Monad m => ParamMode -> Int -> MonadVM m Int
getParam ImmediateMode i = pure i
getParam PositionMode i = peek i

step :: (Monad m) => MonadVM m ()
step = do
  ip <- gets vmIP
  opcodeExpr <- peek ip
  opcode <- mkOpcode opcodeExpr
  case opcode of
    OpAdd a b -> do
      op1 <- getParam a =<< peek (ip + 1)
      op2 <- getParam b =<< peek (ip + 2)
      dstAddr <- peek (ip + 3)
      poke dstAddr (op1 + op2)
    OpMul a b -> do
      op1 <- getParam a =<< peek (ip + 1)
      op2 <- getParam b =<< peek (ip + 2)
      dstAddr <- peek (ip + 3)
      poke dstAddr (op1 * op2)
    OpTerminate ->
      throwE Terminated
  modify (\vm -> vm { vmIP = ip + 1 + numArgs opcode })

run :: (Monad m) => MonadVM m ()
run = forever step

runVM :: Monad m => MonadVM m a -> VM -> m (RunState, VM, Maybe a)
runVM a vm = do
  (result, vm') <- runStateT (runExceptT a) vm
  case result of
    Left exitCond ->
      pure (exitCond, vm', Nothing)
    Right x ->
      pure (Running, vm', Just x)
