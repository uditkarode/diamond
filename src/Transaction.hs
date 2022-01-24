module Transaction where

import Logger (logErrorLn, logErrorMln, logInfoLn)
import SystemUtils (bail)

data Reversal = Reversal
  { userMsg :: Text,
    reversal :: TransactionT ()
  }

type Step = TransactionT Text

newtype TransactionT a = TransactionT {runTransaction :: [Reversal] -> IO ([Reversal], a)}

makeStep :: Text -> Reversal -> Step
makeStep t r = TransactionT $ \_ -> pure ([r], t)

getReversals :: TransactionT [Reversal]
getReversals = TransactionT $ \r0 -> pure (r0, r0)

addReversal :: Reversal -> TransactionT ()
addReversal r = TransactionT $ \r0 -> pure (r0 <> [r], ())

instance Functor TransactionT where
  fmap f (TransactionT g) = TransactionT $ \r0 -> do
    (r1, a) <- g r0
    pure (r0 <> r1, f a)

instance Applicative TransactionT where
  pure val = TransactionT $ \r0 -> pure (r0, val)
  TransactionT fn <*> TransactionT val = TransactionT $ \r0 -> do
    (r1, a) <- fn r0
    bimap (r0 <>) a <$> val r1

instance Monad TransactionT where
  (TransactionT val) >>= fn = TransactionT $ \r0 -> do
    (r1, a) <- val r0
    ret <- runTransaction (fn a) r1
    pure (first (r0 <>) ret)

instance MonadIO TransactionT where
  liftIO action = TransactionT $ \r0 -> do
    v <- action
    pure (r0, v)

instance MonadFail TransactionT where
  fail reason = do
    liftIO $ logErrorMln "An operation in the previous step failed!" (toText reason)
    reversals <- getReversals
    forM_ reversals $ \v -> do
      liftIO $ logInfoLn (userMsg v)
      reversal v
    liftIO $ bail "Exiting due to errors"
    TransactionT $ \_ -> fail reason
