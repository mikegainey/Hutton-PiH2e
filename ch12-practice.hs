-- functor, applicative, monad practice

data Sein a = Ja a | Nichts
  deriving (Show)

instance Functor Sein where
  -- fmap :: Functor f => (a -> b) -> f a -> f b
  fmap g Nichts = Nichts
  fmap g (Ja x) = Ja (g x)

instance Applicative Sein where
  -- pure :: Applicative f => a -> f a
  pure x = Ja x
  -- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
  Nichts <*> _ = Nichts
  (Ja g) <*> (Ja x) = Ja (g x)

instance Monad Sein where
  -- return :: Monad m => a -> m a
  return = pure
  -- (>>=) :: Monad m => m a -> (a -> m b) -> m b
  Nichts >>= _ = Nichts
  (Ja x) >>= g = g x

test1 = (Ja 2) >>= \x -> (Ja 3) >>= \y -> return (x+y)

test2 = do x <- Ja 2
           y <- Nichts
           z <- Ja 4
           return (x+y+z)

type State = Int

newtype ST a = S (State -> (a,State))

st :: ST String
st s = (sign,s)
  where sign = case (compare s 0) of
                 LT -> "negative"
                 EQ -> "zero"
                 GT -> "positive"
