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
import Control.Monad.Fix
import Reactive.Crundle.Internal
import System.IO.Unsafe
import System.Mem.Weak
import Control.Concurrent.MVar
import qualified Data.SkewHeap as Q

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
  Future (Behaviour ag ai@(Event _ na)) <|>
   b@(Future (Behaviour bg bi@(Event _ nb))) = unsafePerformIO $ do
    cache <- newMVar Nothing
    br <- newMVar b
    na' <- takeMVar na
    nb' <- takeMVar nb
    let p = max na' nb' + 1
    (i,w,ub) <- mfix $ \ ~(Event _ nr, w, _) -> do
       ur <- newEmptyMVar
       (i,w') <- innerEventK p cache $ \t up -> do
         let
           clean = IOCascade $ deRefWeak w >>= \mc -> case mc of
             Nothing -> mempty
             Just cache' -> takeMVar cache' >>= \mv -> putMVar cache' Nothing >> case mv of
               Nothing -> mempty
               Just _ -> do
                 p' <- readMVar nr
                 return $ Q.singleton p' $ t ()
         u1 <- subscribe' ai (\_ -> clean) up
         u2 <- subscribe' bi (\_ -> clean) up
         putMVar ur u2
         return (u1 >> u2)
       u2 <- takeMVar ur
       return (i,w',u2)
    putMVar nb nb'
    putMVar na na'
    let
      g' c = IOCascade $ takeMVar cache >>= \mv -> case mv of
        Just v -> putMVar cache mv >> let IOCascade rst = c mv in rst
        Nothing -> let
          IOCascade rst = ag $ \ma -> case ma of
            Just v -> IOCascade $ do
              tryTakeMVar br >>= \mb -> case mb of
                Nothing -> return ()
                _ -> ub
              putMVar cache ma
              let IOCascade r2 = c ma in r2
            Nothing -> IOCascade $ do
              Future (Behaviour bg' _) <- readMVar br
              let
                IOCascade r2 = bg' $ \mb -> IOCascade $ do
                  putMVar cache mb
                  let IOCascade r3 = c mb in r3
              r2
          in rst
    return $ Future (Behaviour g' i)

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