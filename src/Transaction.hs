module Transaction where

import Control.Exception (try)
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

-- gets the Just value or `fail`s if it is Nothing
fromMaybe :: Maybe a -> String -> Transaction a
fromMaybe (Just v) _ = pure v
fromMaybe Nothing err = fail err

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
    v <- try action :: IO (Either SomeException _)
    case v of
      Left exc -> fail $ "Failed due to exception: " <> show exc
      Right a -> pure (r0, a)

-- on `fail` in a transaction, any previous reversals are ran
-- and the program then exits
-- for example, if the 6th step fails, the actions performed by
-- the first 5 steps will be reversed (if the reversals were added)
-- and the program will then exit (check Commands/Create for an example)
instance MonadFail Transaction where
  fail reason = do
    liftIO $ logErrorMln "An operation in the previous step failed!" (toText reason) "reversing previous steps..."
    reversals <- getReversals
    forM_ reversals $ \v -> do
      liftIO $ logInfoLn (userMsg v)
      reversal v
    liftIO $ bail "Exiting due to errors"
    Transaction $ \_ -> fail reason
