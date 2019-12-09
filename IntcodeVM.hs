{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE DeriveFunctor #-}
module IntcodeVM
where

import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Text.Printf
import Data.Maybe
import Data.List
import Debug.Trace

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

data VMContext m =
  VMContext
    { vmInput :: MonadVM m Int
    , vmOutput :: Int -> MonadVM m ()
    }

type MonadVM m = ExceptT RunState (ReaderT (VMContext m) (StateT VM m))

peek :: Monad m => Int -> MonadVM m Int
peek addr = do
  ram <- gets vmRAM
  maybe (throwE $ AccessViolation addr) pure $ IntMap.lookup addr ram

poke :: Monad m => Int -> Int -> MonadVM m ()
poke addr val = do
  ram <- gets vmRAM
  let ram' = IntMap.insert addr val ram
  modify $ \vm -> vm { vmRAM = ram' }

input :: Monad m => MonadVM m Int
input = do
  r <- asks vmInput
  r

output :: Monad m => Int -> MonadVM m ()
output val = do
  w <- asks vmOutput
  w val

numArgs :: Opcode -> Int
numArgs (OpAdd _ _) = 3
numArgs (OpMul _ _) = 3
numArgs OpInput = 1
numArgs (OpOutput _) = 1
numArgs (OpJumpIfTrue _ _) = 2
numArgs (OpJumpIfFalse _ _) = 2
numArgs (OpLessThan _ _) = 3
numArgs (OpEquals _ _) = 3
numArgs OpTerminate = 0

data ParamMode
  = PositionMode
  | ImmediateMode
  deriving (Show, Enum, Eq, Ord, Bounded)

data Opcode
  = OpAdd ParamMode ParamMode
  | OpMul ParamMode ParamMode
  | OpInput
  | OpOutput ParamMode
  | OpJumpIfTrue ParamMode ParamMode
  | OpJumpIfFalse ParamMode ParamMode
  | OpLessThan ParamMode ParamMode
  | OpEquals ParamMode ParamMode
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
    3 -> pure OpInput
    4 -> OpOutput <$> getParamMode 0 i
    5 -> OpJumpIfTrue <$> getParamMode 0 i <*> getParamMode 1 i
    6 -> OpJumpIfFalse <$> getParamMode 0 i <*> getParamMode 1 i
    7 -> OpLessThan <$> getParamMode 0 i <*> getParamMode 1 i
    8 -> OpEquals <$> getParamMode 0 i <*> getParamMode 1 i
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
  mJumpTarget <- case opcode of
    OpAdd a b -> do
      param1 <- peek (ip + 1)
      param2 <- peek (ip + 2)
      op1 <- getParam a param1
      op2 <- getParam b param2
      dstAddr <- peek (ip + 3)
      let dstVal = op1 + op2
      poke dstAddr dstVal
      pure Nothing
    OpMul a b -> do
      param1 <- peek (ip + 1)
      param2 <- peek (ip + 2)
      op1 <- getParam a param1
      op2 <- getParam b param2
      dstAddr <- peek (ip + 3)
      let dstVal = op1 * op2
      poke dstAddr dstVal
      pure Nothing
    OpInput -> do
      dstAddr <- peek (ip + 1)
      input >>= poke dstAddr
      pure Nothing
    OpOutput a -> do
      srcParam <- peek (ip + 1)
      srcVal <- getParam a srcParam
      output srcVal
      pure Nothing
    OpJumpIfTrue a b -> do
      cond <- peek (ip + 1) >>= getParam a
      target <- peek (ip + 2) >>= getParam b
      if cond /= 0 then
        pure $ Just target
      else
        pure Nothing
    OpJumpIfFalse a b -> do
      cond <- peek (ip + 1) >>= getParam a
      target <- peek (ip + 2) >>= getParam b
      if cond == 0 then
        pure $ Just target
      else
        pure Nothing
    OpLessThan a b -> do
      lhs <- peek (ip + 1) >>= getParam a
      rhs <- peek (ip + 2) >>= getParam b
      dstAddr <- peek (ip + 3)
      poke dstAddr $ if lhs < rhs then 1 else 0
      pure Nothing
    OpEquals a b -> do
      lhs <- peek (ip + 1) >>= getParam a
      rhs <- peek (ip + 2) >>= getParam b
      dstAddr <- peek (ip + 3)
      poke dstAddr $ if lhs == rhs then 1 else 0
      pure Nothing
    OpTerminate ->
      throwE Terminated
  let nextIP = fromMaybe (ip + 1 + numArgs opcode) mJumpTarget
  modify (\vm -> vm { vmIP = nextIP })

run :: (Monad m) => MonadVM m ()
run = forever step

runVM :: Monad m => MonadVM m a -> VM -> m (RunState, VM, Maybe a)
runVM = runVMWith (pure 0) (const $ pure ())

runVMWith :: Monad m => (MonadVM m Int) -> (Int -> MonadVM m ()) -> MonadVM m a -> VM -> m (RunState, VM, Maybe a)
runVMWith r w a vm = do
  (result, vm') <- runStateT (runReaderT (runExceptT a) (VMContext r w)) vm
  case result of
    Left exitCond ->
      pure (exitCond, vm', Nothing)
    Right x ->
      pure (Running, vm', Just x)
