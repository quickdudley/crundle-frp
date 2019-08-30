module Reactive.Crundle.Future (
  Future,
  awaitIO,
  await,
  (<^>),
  (<^),
  future,
  asFuture,
  lagF
 ) where

import Control.Applicative
import Control.Monad
import Reactive.Crundle.Internal

newtype Future a = Future (Behaviour (Maybe a))

instance Functor Future where
  fmap f (Future b) = Future $ (fmap . fmap) f b

instance Applicative Future where
  pure = Future . pure . pure
  Future f <*> Future a = Future $
    (<*>) <$> f <*> a

instance Monad Future where
  return = pure
  Future a >>= f = Future $ a >>= \a' -> case a' of
    Nothing -> return Nothing
    Just a' -> let Future b = f a' in b

instance Alternative Future where
  empty = Future (pure Nothing)
  Future a <|> Future b = Future $ a >>= \a' -> case a' of
    Just _ -> return a'
    Nothing -> b

awaitIO :: Event a -> (a -> IO (Maybe b)) -> (a -> b -> IO b) -> IO (Future b)
awaitIO e w u = Future <$> stepperIO e (\a s -> case s of
  Just s -> Just <$> u a s
  Nothing -> w a
 ) Nothing

await :: Event a -> (a -> (Maybe b)) -> (a -> b -> b) -> IO (Future b)
await e w u = awaitIO e (return . w) ((return .) . u)

infixl 4 <^>
(<^>) :: Future (a -> b) -> Event a -> Event b
Future f <^> e = queryFilter (((\b -> case b of
  Nothing -> const Nothing
  Just b' -> Just . b'
 ) <$> f) <@> e) return

infixl 4 <^
(<^) :: Future a -> Event b -> Event a
f <^ e = (const <$> f) <^> e

future :: Future a -> Behaviour (Maybe a)
future (Future a) = a

asFuture :: Behaviour a -> Future a
asFuture a = Future (Just <$> a)

lagF :: Future a -> Future a
lagF (Future a) = Future (lag a)

observeF :: Event (Future a) -> Event a
observeF = flip queryFilter return . observe . fmap future