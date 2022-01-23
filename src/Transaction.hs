module Transaction where

data Reversal = Reversal
  { userMsg :: Text,
    reversal :: IO ()
  }

type Transaction = TransactionT Text

newtype TransactionT a = TransactionT {runTransaction :: [Reversal] -> IO ([Reversal], a)}

makeTransaction :: Text -> Reversal -> Transaction
makeTransaction t r = TransactionT $ \_ -> pure ([r], t)

getReversals :: TransactionT [Reversal]
getReversals = TransactionT $ \r0 -> pure ([], r0)

instance Functor TransactionT where
  fmap f (TransactionT g) = TransactionT $ \r0 -> do
    (r1, a) <- g r0
    pure (r1, f a)

instance Applicative TransactionT where
  pure val = TransactionT $ \r0 -> pure (r0, val)
  TransactionT fn <*> TransactionT val = TransactionT $ \r0 -> do
    fn <- fn r0
    bimap (fst fn <>) (snd fn) <$> val r0

instance Monad TransactionT where
  (TransactionT val) >>= fn = TransactionT $ \r0 -> do
    val <- val r0
    ret <- runTransaction (fn (snd val)) r0
    pure (first (fst val <>) ret)

instance MonadIO TransactionT where
  liftIO action = TransactionT $ \r0 -> do
    v <- action
    pure (r0, v)
