module Reactive.Crundle.Internal where

import System.Mem.Weak
import Control.Concurrent.MVar
import Control.Concurrent (forkIO)
import Data.IORef
import System.IO.Unsafe
import qualified Data.SkewHeap as Q
import Control.Monad
import Control.Monad.Fix
import Control.Parallel
import Control.Exception (finally)
import Foreign.StablePtr

data Event a = Event (MVar [(IORef (), a -> IOCascade, Int -> IOCascade)]) (MVar Int)
data Behaviour a = Behaviour ((a -> IOCascade) -> IOCascade) (Event ())

newtype IOCascade = IOCascade (IO (Q.SkewHeap Int IOCascade))

runCascade :: IOCascade -> IO ()
runCascade = step mempty where
  step q (IOCascade a) = do
    r <- a
    loop (q <> r)
  loop q = case Q.pop q of
    Nothing -> return ()
    Just (_, a, r) -> step r a

smallCascade :: IO () -> IOCascade
smallCascade a = IOCascade (mempty <$ a)

instance Semigroup IOCascade where
  IOCascade a <> IOCascade b = IOCascade ((<>) <$> a <*> b)

instance Monoid IOCascade where
  mempty = IOCascade (return mempty)
  mappend = (<>)
  mconcat l = IOCascade $ mconcat <$> mapM (\(IOCascade a) -> a) l

{-# NOINLINE cascadeLock #-}
cascadeLock :: MVar ()
cascadeLock = unsafePerformIO $ newMVar ()

sourceEvent :: IO (Event a, a -> IO ())
sourceEvent = do
  lr <- newMVar []
  num <- newMVar 0
  let
    t a = do
      takeMVar cascadeLock
      flip finally (putMVar cascadeLock ()) $ do
        h <- readMVar lr
        runCascade $ mconcat $ map (\(_,r,_) -> r a) h
  return (Event lr num, t)

innerEventK :: Int -> MVar k -> ((a -> IOCascade) -> (Int -> IOCascade) -> IO (IO ())) -> IO (Event a, Weak (MVar k))
innerEventK p k sub = do
  lr <- newMVar []
  num <- newMVar p
  u <- sub (\a -> IOCascade $ do
    h <- readMVar lr
    let IOCascade rst = mconcat $ map (\(_,r,_) -> r a) h in rst
   ) (\np -> IOCascade $ do
    num' <- takeMVar num
    if num' < np
      then do
        let d = np + 1
        h <- readMVar lr
        putMVar num np
        let IOCascade rst = mconcat $ map (\(_,_,r) -> r d) h in rst
      else putMVar num num' >> mempty
   )
  w <- mkWeakMVar k u
  return (Event lr num, w)

innerEventW :: Int -> ((a -> IOCascade) -> (Int -> IOCascade) -> IO (IO ())) -> IO (Event a)
innerEventW p sub = do
  lr <- newMVar []
  num <- newMVar p
  void $ mfix $ \w -> do
    u <- sub (\a -> IOCascade $ deRefWeak w >>= \ml -> case ml of
      Nothing -> return mempty
      Just lr' -> do
        h <- readMVar lr'
        let IOCascade rst = mconcat $ map (\(_,r,_) -> r a) h in rst
     ) (\np -> IOCascade $ do
      num' <- takeMVar num
      if num' < np
        then do
          let d = np + 1
          h <- readMVar lr
          putMVar num np
          let IOCascade rst = mconcat $ map (\(_,_,r) -> r d) h in rst
        else putMVar num num' >> mempty
     )
    mkWeakMVar lr u
  return (Event lr num)

subscribe :: Event a -> (a -> IO ()) -> IO (IO ())
subscribe e h = do
  sp <- newStablePtr e
  u <- subscribe' e (smallCascade <$> h) mempty
  return (freeStablePtr sp >> u)

subscribe' :: Event a -> (a -> IOCascade) -> (Int -> IOCascade) -> IO (IO ())
subscribe' (Event lr pr) h rp = do
  i <- newIORef ()
  t <- takeMVar lr
  putMVar lr ((i,h,rp):t)
  return $ do
    t' <- takeMVar lr
    let t'' = filter (\(i',_,_) -> i' /= i) t'
    foldr (flip const) () t'' `par` putMVar lr t''

subscribeP :: Event a -> (Int -> IOCascade) -> IO (IO ())
subscribeP (Event lr pr) rp = do
  i <- newIORef ()
  t <- takeMVar lr
  putMVar lr ((i,mempty,rp):t)
  return $ do
    t' <- takeMVar lr
    let t'' = filter (\(i',_,_) -> i' /= i) t'
    foldr (flip const) () t'' `par` putMVar lr t''

never :: Event a
never = unsafePerformIO $ innerEventW 0 mempty

instance Functor Event where
  fmap f e@(Event _ n) = unsafePerformIO $ do
    n' <- takeMVar n
    r <- innerEventW (n' + 1) (\t -> subscribe' e (t . f))
    putMVar n n'
    return r

instance Semigroup (Event a) where
  (<>) = mappend

instance Monoid (Event a) where
  mempty = never
  mappend a b = mconcat [a,b]
  mconcat [] = never
  mconcat [a] = a
  mconcat l = unsafePerformIO $ do
    pl <- foldr (\(Event _ n) c -> do
      n' <- takeMVar n
      (n':) <$> c
     ) (return []) l
    r <- innerEventW (maximum (1:pl) + 1) (\t n -> foldr (\e c -> do
      u <- subscribe' e t n
      (u >>) <$> c
     ) (return (return ())) l)
    zipWithM_ (\(Event _ n) n' -> putMVar n n') l pl
    return r

instance Functor Behaviour where
  fmap f (Behaviour g i@(Event _ n)) = unsafePerformIO $ do
    cache <- newMVar Nothing
    p <- takeMVar n
    (i',w) <- mfix $ \ ~(i'@(Event _ n'),w') -> innerEventK (p + 1) cache $ \t ->
      subscribe' i (\_ -> IOCascade $ deRefWeak w' >>= \mc -> case mc of
        Nothing -> mempty
        Just cache' -> takeMVar cache' >>= \mv -> putMVar cache' Nothing >> case mv of
          Nothing -> mempty
          Just _ -> do
            n'' <- readMVar n'
            return $ Q.singleton n'' $ t ()
       )
    putMVar n p
    let
      g' c = IOCascade $ takeMVar cache >>= \mv -> case mv of
        Just v -> putMVar cache mv >> let IOCascade rst = c v in rst
        Nothing -> let
          IOCascade rst = g $ \a -> IOCascade $ do
            let v = f a
            putMVar cache (Just v)
            let IOCascade z = c v in z
          in rst
    return $ Behaviour g' i'

instance Applicative Behaviour where
  pure a = Behaviour ($ a) never
  Behaviour fg fi@(Event _ nf) <*> Behaviour ag ai@(Event _ na) = unsafePerformIO $ do
    cache <- newMVar Nothing
    nf' <- takeMVar nf
    na' <- takeMVar na
    let p = max nf' na'
    (i',w) <- mfix $ \ ~(Event _ nr,w') -> innerEventK (p + 1) cache $ \t up -> do
      let
        clean = IOCascade $ deRefWeak w' >>= \mc -> case mc of
          Nothing -> mempty
          Just cache' -> takeMVar cache' >>= \mv -> putMVar cache' Nothing >> case mv of
            Nothing -> mempty
            Just _ -> do
              p' <- readMVar nr
              return $ Q.singleton p' $ t ()
      u1 <- subscribe' fi (\_ -> clean) up
      u2 <- subscribe' ai (\_ -> clean) up
      return (u1 >> u2)
    putMVar na na'
    putMVar nf nf'
    let
      g' c = IOCascade $ takeMVar cache >>= \mv -> case mv of
        Just v -> putMVar cache mv >> let IOCascade rst = c v in rst
        Nothing -> let
          IOCascade rst = fg $ \f -> ag $ \a -> let
            b = f a
            IOCascade r2 = c b
            in IOCascade $ putMVar cache (Just b) >> r2
          in rst
    return $ Behaviour g' i'

instance Monad Behaviour where
  return = pure
  Behaviour g i@(Event _ ni) >>= f = unsafePerformIO $ do
    bCache <- newMVar Nothing
    wbc <- mkWeakMVar bCache mempty
    cache <- newMVar Nothing
    lr <- newMVar []
    n0 <- takeMVar ni
    nr <- newMVar (n0 + 1)
    let
      un n' = IOCascade $ do
        n <- takeMVar nr
        let d = n' + 1
        if d > n
          then do
            h <- readMVar lr
            putMVar nr d
            let IOCascade rst = mconcat $ map (\(_,_,r) -> r d) h in rst
          else putMVar nr n >> mempty
    (w,cleanB) <- mfix $ \ ~(w',_) -> do
      let
        cleanB = deRefWeak wbc >>= \mb -> case mb of
          Nothing -> return ()
          Just bCache' -> takeMVar bCache' >>= \mbv -> case mbv of
            Nothing -> putMVar bCache' Nothing
            Just (u',_) -> u' >> putMVar bCache' Nothing
      u <- subscribe' i (\_ -> IOCascade $ do
        cleanB
        deRefWeak w' >>= \mc -> case mc of
          Nothing -> mempty
          Just cache' -> takeMVar cache' >>= \mc -> case mc of
            Nothing -> putMVar cache' Nothing >> mempty
            Just _ -> do
              putMVar cache' Nothing
              h <- readMVar lr
              let IOCascade rst = mconcat $ map (\(_,r,_) -> r ()) h in rst
        ) un
      (flip (,) cleanB) <$> mkWeakMVar cache (cleanB >> u)
    putMVar ni n0
    let
      b c = IOCascade $ takeMVar bCache >>= \mb -> case mb of
        Just (_,h) -> putMVar bCache mb >> let IOCascade rst = c h in rst
        Nothing -> let
          IOCascade rst = g $ \b' -> let
            Behaviour h j@(Event _ jn) = f b'
            in IOCascade $ do
              u <- subscribe' j (\_ -> IOCascade $ takeMVar cache >>= \mc -> case mc of
                Nothing -> putMVar cache Nothing >> mempty
                Just _ -> do
                  putMVar cache Nothing
                  h' <- readMVar lr
                  let IOCascade rst = mconcat $ map (\(_,r,_) -> r ()) h' in rst
               ) un
              jn' <- readMVar jn
              putMVar bCache $ Just (u,h)
              let IOCascade rst = (un jn') <> (c h) in rst
          in rst
      g' c = IOCascade $ takeMVar cache >>= \mc -> case mc of
        Just v -> putMVar cache mc >> let IOCascade rst = c v in rst
        Nothing -> let
          IOCascade rst = b $ \h -> h $ \v -> IOCascade $ do
            putMVar cache $ Just v
            let IOCascade rst' = c v in rst'
          in rst
    return $ Behaviour g' (Event lr nr)

stepperIO :: Event a -> (a -> b -> IO b) -> b -> IO (Behaviour b)
stepperIO e@(Event _ nr) c i = do
  cache <- newMVar i
  n <- takeMVar nr
  (i',w) <- mfix $ \ ~((Event _ nr'),w') -> innerEventK (n + 1) cache $ \t ->
    subscribe' e $ \a -> IOCascade $ deRefWeak w' >>= \mc -> case mc of
      Nothing -> mempty
      Just cache' -> do
        n' <- takeMVar nr'
        s <- takeMVar cache'
        s' <- c a s
        putMVar cache' s'
        putMVar nr' n'
        return $ Q.singleton n' $ t ()
  putMVar nr n
  return $ Behaviour (\c -> IOCascade $ readMVar cache >>= \a -> let IOCascade rst = c a in rst) i'

stepper :: Event a -> (a -> b -> b) -> b -> IO (Behaviour b)
stepper e c i = stepperIO e (\a s -> return $ c a s) i

poll :: IO a -> Behaviour a
poll s = unsafePerformIO $ do
  num <- newMVar 0
  lr <- newMVar []
  cache <- newMVar Nothing
  let
    g c = IOCascade $ takeMVar cache >>= \mc -> case mc of
      Nothing -> s >>= \v -> putMVar cache (Just v) >>
        (let IOCascade rst = c v in rst) <>
        (return $ Q.singleton maxBound $ IOCascade $ do
          takeMVar cache >> putMVar cache Nothing
          l <- readMVar lr
          let IOCascade rst = mconcat $ map (\(_,r,_) -> r ()) l in rst
         )
      Just v -> putMVar cache mc >> let IOCascade rst = c v in rst
  return $ Behaviour g (Event lr num)

poll2 :: Behaviour a -> (a -> IO b) -> Behaviour b
poll2 (Behaviour g i@(Event _ gn)) f = unsafePerformIO $ do
  n <- readMVar gn
  cache <- newMVar Nothing
  (i',w) <- mfix $ \ ~(i'@(Event _ n'), w) -> innerEventK (n + 1) cache $ \t ->
    subscribe' i (\_ -> IOCascade $ deRefWeak w >>= \mc -> case mc of
      Nothing -> mempty
      Just cache' -> takeMVar cache' >>= \mv -> putMVar cache' Nothing >> case mv of
        Nothing -> mempty
        Just _ -> do
          n'' <- readMVar n'
          return $ Q.singleton n'' $ t ()
     )
  let
    g' c = IOCascade $ takeMVar cache >>= \mv -> case mv of
      Just v -> putMVar cache mv >> let IOCascade rst = c v in rst
      Nothing -> let
        IOCascade rst = g $ \a -> IOCascade $ do
          v <- f a
          putMVar cache (Just v)
          let IOCascade z = c v in z
        in rst
  return $ Behaviour g' i'

flushes :: Behaviour a -> Event ()
flushes (Behaviour _ e) = e

applyIO :: Behaviour (a -> IO b) -> Event a -> Event b
applyIO (Behaviour g c@(Event _ gn)) e@(Event _ en) = unsafePerformIO $ do
  gn' <- takeMVar gn
  en' <- takeMVar en
  let n = 1 + max gn' en'
  r <- mfix $ \ ~(Event _ rn) -> innerEventW n $ \t un -> do
    u1 <- subscribe' e (\a -> let
       cl lp = IOCascade $ do
         n' <- readMVar rn
         if n' > lp
           then return $ Q.singleton n' $ cl n'
           else let
             IOCascade rst = g $ \f -> IOCascade $ do
               n2 <- readMVar rn
               if n2 > lp
                 then return $ Q.singleton n2 $ cl n2
                 else do
                   b <- f a
                   return $ Q.singleton lp $ t b
             in rst
       in IOCascade $ do
         n' <- readMVar rn
         return $ Q.singleton n' $ cl n'
     ) un
    u2 <- subscribeP c un
    return (u1 >> u2)
  putMVar gn gn'
  putMVar en en'
  return r

apply :: Behaviour (a -> b) -> Event a -> Event b
apply b e = applyIO (fmap (return .) b) e

infixl 4 <@>
(<@>) :: Behaviour (a -> b) -> Event a -> Event b
(<@>) = apply

infixl 4 <@
(<@) :: Behaviour b -> Event a -> Event b
b <@ e = applyIO (fmap (return . return) b) e

observe :: Event (Behaviour a) -> Event a
observe e@(Event _ nr) = unsafePerformIO $ do
  n <- takeMVar nr
  r <- mfix $ \ ~(Event rl rr) -> innerEventW (n + 1) $ \t -> subscribe' e $
    \ ~(Behaviour g (Event _ gr)) -> IOCascade $ let
      retry p = do
        takeMVar rr
        putMVar rr p
        unl <- readMVar rl
        (return $ Q.singleton p $ IOCascade $ go p) <>
          (let IOCascade rst = mconcat $ map (\(_,_,r) -> r p) unl in rst)
      go p = readMVar gr >>= \gp -> if gp > p
        then retry gp
        else let
          IOCascade rst = g $ \a -> IOCascade $ do
            gp2 <- readMVar gr
            if gp2 > p
              then retry gp2
              else return $ Q.singleton p $ t a
          in rst
      in readMVar rr >>= go
  putMVar nr n
  return r

query :: Event a -> (a -> IO b) -> Event b
query p@(Event _ nr) q = unsafePerformIO $ do
  n <- takeMVar nr
  r <- mfix $ \ ~(Event _ rr) -> innerEventW n (\t ->
    subscribe' p (\a -> IOCascade $ do
      b <- q a
      n' <- readMVar rr
      return $ Q.singleton n' $ t b
     )
   )
  putMVar nr n
  return r

queryFilter :: Event a -> (a -> IO (Maybe b)) -> Event b
queryFilter p@(Event _ pr) q = unsafePerformIO $ do
  n <- takeMVar pr
  r <- mfix $ \ ~(Event _ ps) -> innerEventW (n + 1) (\t ->
    subscribe' p (\a -> IOCascade $ q a >>= \mr -> case mr of
      Nothing -> mempty
      Just b -> do
        n' <- readMVar ps
        return $ Q.singleton n' $ t b
    )
   )
  putMVar pr n
  return r

eitherEIO :: Event a -> (a -> IO (Either l r)) -> (Event l, Event r)
eitherEIO p@(Event _ pr) q = unsafePerformIO $ do
  n <- takeMVar pr
  let n' = n + 1
  lr <- newEmptyMVar
  r <- mfix $ \ ~(Event _ psr) -> innerEventW n' (\rt rn -> do
    uc <- newMVar 2
    ur <- newEmptyMVar
    let
      u = takeMVar uc >>= \c -> if c == 1
        then takeMVar ur >>= id
        else putMVar uc (c - 1)
    l <- mfix $ \ ~(Event _ psl) -> innerEventW n' (\lt ln ->
      subscribe' p (\a -> IOCascade $ q a >>= \er -> case er of
        Left b -> do
          n'' <- readMVar psl
          return $ Q.singleton n'' $ lt b
        Right b -> do
          n'' <- readMVar psr
          return $ Q.singleton n'' $ rt b
       ) (ln <> rn) >>= putMVar ur >> return u
     )
    putMVar lr l
    return u
   )
  putMVar pr n
  l <- takeMVar lr
  return (l,r)

-- | Create an event which changes source when the original event fires.
-- Covers the same use-cases as the 'Monad' instance suggested in Conal
-- Elliott's paper, but is not a direct replacement.
switcher :: Event (Event a) -> IO (Event a)
switcher s@(Event _ sr) = do
  u1r <- newMVar (return ())
  n <- readMVar sr
  innerEventW n (\t0 un -> do
    u0 <- subscribe' s (\e1@(Event _ ir) -> IOCascade $ do
      u1 <- takeMVar u1r
      u1
      u' <- subscribe' e1 t0 un
      putMVar u1r u'
      n' <- readMVar ir
      let IOCascade z = un n' in z
     ) un
    return $ do
      u1 <- takeMVar u1r
      u1
      u0
   )

-- | Allow a 'Behaviour' to be used recursively. Semantically:
-- @
--   lag b = b . pred
-- @
-- The implementation assumes the resulting 'Behaviour' will actually be used
-- recursively. If this is not the case; the resulting 'Behaviour' may behave
-- identically to the original one.
lag :: Behaviour a -> Behaviour a
lag ~(Behaviour g u) = unsafePerformIO $ do
  lr <- newMVar []
  num <- newMVar 0
  cache <- newMVar Nothing
  qs <- unsafeInterleaveIO $ mfix $ \w -> do
    u_ <- subscribe' u (\a -> IOCascade $ return $ Q.singleton maxBound $
      IOCascade $ takeMVar cache >> putMVar cache Nothing >> deRefWeak w >>=
      \ml -> case ml of
        Nothing -> return mempty
        Just lr' -> do
          h <- readMVar lr'
          let IOCascade rst = mconcat $ map (\(_,r,_) -> r a) h in rst
     ) mempty
    mkWeakMVar lr u_ 
  let
    u' = Event lr num
    g' c = IOCascade $ takeMVar cache >>= \mc -> case mc of
      Nothing -> let
        IOCascade rst = g $ \a -> IOCascade $ putMVar cache (Just a) >>
          let IOCascade rs' = c a in rs'
        in rst
      Just v -> putMVar cache mc >> let IOCascade rst = c v in rst
  return $ Behaviour (qs `seq` g') u'

async :: Event a -> (a -> IO b) -> Event b
async e f = unsafePerformIO $
  innerEventW 0 (\t n -> subscribe' e (\a -> IOCascade $
     mempty <$ forkIO (f a >>= \b -> takeMVar cascadeLock >>
       finally (runCascade (t b)) (putMVar cascadeLock ())
     )
    ) mempty
   )