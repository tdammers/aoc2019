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

data RunState i
  = Running
  | Terminated
  | AccessViolation Int
  | InvalidOpcode i
  | TooDynamic i
  deriving (Show, Read, Eq, Ord)

data VMOf a =
  VM
    { vmIP :: Int
    , vmRAM :: IntMap a
    }
    deriving (Show, Functor)

type VM = VMOf Int

dumpVM :: Show i => VMOf i -> IO ()
dumpVM (VM ip ram) = do
  printf "IP: %d\n" ip
  let max = fromMaybe 0 $ fst <$> IntMap.lookupMax ram
  forM_ [0..max] $ \addr -> do
    when (addr `mod` 10 == 0) $ do
      printf "\n%04d |" addr
    maybe (printf " <NIL>") (printf " %7s" . show) (ram IntMap.!? addr)
  printf "\n"

defVM :: VMOf a
defVM = VM 0 IntMap.empty

loadVM :: [a] -> VMOf a
loadVM program =
  VM 0 (IntMap.fromList $ zip [0..] program)

type MonadVM i m = ExceptT (RunState i) (StateT (VMOf i) m)

peek :: Monad m => Int -> MonadVM i m i
peek addr = do
  ram <- gets vmRAM
  maybe (throwE $ AccessViolation addr) pure $ IntMap.lookup addr ram

poke :: Monad m => Int -> i -> MonadVM i m ()
poke addr val = do
  ram <- gets vmRAM
  let ram' = IntMap.insert addr val ram
  modify $ \vm -> vm { vmRAM = ram' }

class Operand a where
  add :: a -> a -> a
  mul :: a -> a -> a
  toAddr :: a -> Maybe Int

instance Operand Int where
  add = (+)
  mul = (*)
  toAddr = Just

operate :: (Monad m, Operand i, Show i) => String -> (i -> i -> i) -> MonadVM i m ()
operate label f = do
  ip <- gets vmIP
  op1Addr <- peek (ip + 1) >>= mkAddr
  op2Addr <- peek (ip + 2) >>= mkAddr
  dstAddr <- peek (ip + 3) >>= mkAddr
  op1 <- peek op1Addr
  op2 <- peek op2Addr
  let dst = f op1 op2
  poke dstAddr dst

mkAddr :: (Monad m, Operand i) => i -> MonadVM i m Int
mkAddr i = maybe (throwE $ TooDynamic i) pure . toAddr $ i

numArgs :: Opcode -> Int
numArgs OpAdd = 3
numArgs OpMul = 3
numArgs OpTerminate = 0

data Opcode
  = OpAdd
  | OpMul
  | OpTerminate
  deriving (Show, Enum, Eq, Ord, Bounded)

mkOpcode :: (Monad m, Operand i) => i -> MonadVM i m Opcode
mkOpcode i = do
  mkAddr i >>= \case
    1 -> pure OpAdd
    2 -> pure OpMul
    99 -> pure OpTerminate
    x -> throwE (InvalidOpcode i)

step :: (Show i, Operand i, Monad m) => MonadVM i m ()
step = do
  ip <- gets vmIP
  opcodeExpr <- peek ip
  opcode <- mkOpcode opcodeExpr
  case opcode of
    OpAdd -> operate "+" add
    OpMul -> operate "-" mul
    OpTerminate -> throwE Terminated
  modify (\vm -> vm { vmIP = ip + 1 + numArgs opcode })

run :: (Show i, Operand i, Monad m) => MonadVM i m ()
run = forever step

runVM :: Monad m => MonadVM i m a -> VMOf i -> m (RunState i, VMOf i, Maybe a)
runVM a vm = do
  (result, vm') <- runStateT (runExceptT a) vm
  case result of
    Left exitCond ->
      pure (exitCond, vm', Nothing)
    Right x ->
      pure (Running, vm', Just x)
