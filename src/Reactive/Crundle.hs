module Reactive.Crundle (
  Event,
  Behaviour,
  IOCascade,
  sourceEvent,
  subscribe,
  never,
  stepperIO,
  stepper,
  applyIO,
  apply,
  (<@>),
  (<@),
  observe,
  query,
  justE,
  eitherE,
  eitherEIO,
  aggregate,
  queryFilter,
  switcher,
  filterE,
  lag,
  poll,
  poll2,
  flushes,
  async,
  split,
  Split,
  SinkList(..),
  EventList(..),
  module Reactive.Crundle.Future
 ) where

import Reactive.Crundle.Internal
import Reactive.Crundle.Future

filterE :: Event a -> (a -> Maybe b) -> Event b
filterE s p = queryFilter s (return . p)

eitherE :: Event a -> (a -> Either l r) -> (Event l, Event r)
eitherE s p = eitherEIO s (return . p)

justE :: Event (Maybe a) -> Event a
justE s = filterE s id
