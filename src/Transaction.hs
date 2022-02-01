module Transaction where

import Logger (logErrorLn, logErrorMln, logInfoLn)

data Reversal = Reversal
  { userMsg :: Text,
    reversal :: Transaction ()
  }

type Step = Transaction Text

type Command = Transaction ()

newtype Transaction a = Transaction {runTransaction :: [Reversal] -> IO ([Reversal], a)}

makeStep :: Text -> Reversal -> Step
makeStep t r = Transaction $ \r0 -> pure ([r] <> r0, t)

getReversals :: Transaction [Reversal]
getReversals = Transaction $ \r0 -> pure (r0, r0)

addReversal :: Reversal -> Transaction ()
addReversal r = Transaction $ \r0 -> pure ([r] <> r0, ())

bail :: Text -> IO ()
bail msg = logErrorLn msg >> exitFailure

instance Functor Transaction where
  fmap f (Transaction g) = Transaction $ \r0 -> do
    (r1, a) <- g r0
    pure (r1, f a)

instance Applicative Transaction where
  pure val = Transaction $ \r0 -> pure (r0, val)
  Transaction fn <*> Transaction val = Transaction $ \r0 -> do
    (r1, a) <- fn r0
    second a <$> val r1

instance Monad Transaction where
  (Transaction val) >>= fn = Transaction $ \r0 -> do
    (r1, a) <- val r0
    runTransaction (fn a) r1

instance MonadIO Transaction where
  liftIO action = Transaction $ \r0 -> do
    v <- action
    pure (r0, v)

instance MonadFail Transaction where
  fail reason = do
    liftIO $ logErrorMln "An operation in the previous step failed!" (toText reason)
    reversals <- getReversals
    forM_ reversals $ \v -> do
      liftIO $ logInfoLn (userMsg v)
      reversal v
    liftIO $ bail "Exiting due to errors"
    Transaction $ \_ -> fail reason
