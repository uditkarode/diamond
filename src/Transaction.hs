module Transaction where

data Reversal = Reversal
  { text :: Text,
    command :: Text
  }
  deriving (Show, Eq)

type Transaction = TransactionT Text

newtype TransactionT a = TransactionT {runTransaction :: IO ([Reversal], a)}

instance Functor TransactionT where
  fmap f (TransactionT val) = TransactionT $ do
    tup <- val
    pure $ second (const . f $ snd tup) tup

instance Applicative TransactionT where
  pure val = TransactionT $ pure ([], val)
  TransactionT fn <*> TransactionT val = TransactionT $ do
    fn <- fn
    bimap (fst fn <>) (snd fn) <$> val

instance Monad TransactionT where
  (TransactionT val) >>= fn = TransactionT $ do
    val <- val
    ret <- runTransaction $ fn (snd val)
    pure (first (fst val <>) ret)
