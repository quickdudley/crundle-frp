{-# LANGUAGE TypeFamilies #-}
module Data.SkewHeap (
  SkewHeap,
  singleton,
  fromList,
  toList,
  foldrWithKey,
  pop,
  popEq,
  searchM,
  unsafeMapKeys
 ) where

import qualified GHC.Exts
import Data.Semigroup

data SkewHeap k a = SkTip | SkNode k a (SkewHeap k a) (SkewHeap k a)

singleton :: k -> a -> SkewHeap k a
singleton k a = SkNode k a SkTip SkTip

instance Ord k => Semigroup (SkewHeap k a) where
  SkTip <> b = b
  a <> SkTip = a
  ha@(SkNode ka a la ra) <> hb@(SkNode kb b lb rb) = let
    (kc,c,lc,rc) = if ka > kb
      then (kb,b,ha <> rb, lb)
      else (ka,a,hb <> ra, la)
    in SkNode kc c lc rc

instance Ord k => Monoid (SkewHeap k a) where
  mempty = SkTip
  mconcat [] = SkTip
  mconcat [a] = a
  mconcat l = mconcat $ mergePairs l where
    mergePairs [] = []
    mergePairs r@[_] = r
    mergePairs (a:b:r) = a <> b : mergePairs r

instance Ord k => GHC.Exts.IsList (SkewHeap k a) where
  type Item (SkewHeap k a) = (k,a)
  fromList = fromList
  toList = toList

fromList :: Ord k => [(k,a)] -> SkewHeap k a
fromList = mconcat . map (uncurry singleton)

toList :: Ord k => SkewHeap k a -> [(k,a)]
toList = foldrWithKey (\k a r -> (k,a) : r) []

foldrWithKey :: Ord k => (k -> a -> b -> b) -> b -> SkewHeap k a -> b
foldrWithKey f z = go where
  go SkTip = z
  go (SkNode k a l r) = f k a (go (l <> r))

pop :: Ord k => SkewHeap k a -> Maybe (k,a,SkewHeap k a)
pop SkTip = Nothing
pop (SkNode k a l r) = Just (k, a, l <> r)

popEq :: Ord k => SkewHeap k a -> ([(k,a)],SkewHeap k a)
popEq SkTip = ([], SkTip)
popEq (SkNode k a l r) = let
  go SkTip = id
  go n@(SkNode k' b l' r') = if k' == k
    then (\ ~(r,h) -> ((k',b) : r, h)) . go l' . go r'
    else (\ ~(r,h) -> (r, n : h))
  (vl,rh) = go l . go r $ ([],[])
  in ((k,a) : vl, mconcat rh)

instance Functor (SkewHeap k) where
  fmap f = go where
    go SkTip = SkTip
    go (SkNode k a l r) = SkNode k (f a) (go l) (go r)

instance Ord k => Foldable (SkewHeap k) where
  foldr f b = go where
    go SkTip = b
    go (SkNode k a l r) = f a (go (l <> r))
  null SkTip = True
  null _ = False

data Branch = L | R

instance Ord k => Traversable (SkewHeap k) where
  traverse f = fmap (rebuild SkTip) . go . addPaths where
    addPaths = goP id where
      goP acc SkTip = SkTip
      goP acc (SkNode k a l r) = SkNode k (a, acc [])
        (goP (acc . (L:)) l) 
        (goP (acc . (R:)) r)
    rebuild n [] = n
    rebuild n ((p,k,a):r) = rebuild (fill1 p k a n) r
    fill1 [] k a SkTip = SkNode k a SkTip SkTip
    fill1 (p1:pr) k a (SkNode nk na l r) = case p1 of
      L -> SkNode nk na (fill1 pr k a l) r
      R -> SkNode nk na l (fill1 pr k a r)
    fill1 _ _ _ _ = error "Violation of heap contract found"
    go SkTip = pure []
    go (SkNode k (a,p) l r) = (\b s -> (p,k,b) : s) <$> f a <*> go (l <> r)


searchM :: (Ord k, Monad m) => (k -> a -> m (Either [(k,a)] b)) ->
  SkewHeap k a ->
  m (Maybe b)
searchM f = go where
  go SkTip = return Nothing
  go (SkNode k a l r) = f k a >>= \e -> case e of
    Left n -> go $ fromList n <> (l <> r)
    Right r -> return (Just r)

unsafeMapKeys :: (k1 -> k2) -> SkewHeap k1 a -> SkewHeap k2 a
unsafeMapKeys f = go where
  go SkTip = SkTip
  go (SkNode k a l r) = SkNode (f k) a (go l) (go r)
