{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Eff where

import           Data.Proxy (Proxy(..))
import qualified Control.Monad.Reader as R
import qualified Control.Monad.State as S

import qualified Data.Vinyl as V
import           Data.Vinyl (Rec((:&)))
import qualified Data.Vinyl.TypeLevel as V

import qualified GHC.Generics as G
import           GHC.Generics ((:*:)(..))

-- | noitacilppa rotcurtsnoc epyt desreveR
newtype Ylppa (x :: k) (f :: k -> *) = Ylppa { unYlppa :: f x }

-- Handlers m '[e1, e2, e3] ~ (e1 m, e2 m, e3 m)
type Handlers m es = V.Rec (Ylppa m) es

newtype Eff (es :: [(* -> *) -> *]) a =
  Eff { unEff :: forall (m :: * -> *). Monad m => R.ReaderT (Handlers m es) m a }
  deriving (Functor)

instance Applicative (Eff es) where
  pure x = Eff (pure x)
  Eff f <*> Eff x = Eff (f <*> x)

instance Monad (Eff es) where
  Eff m >>= k = Eff (m >>= unEff . k)

-- | Run an Eff computation, providing effect handlers in terms of the base monad.
runEff :: Monad m => Handlers m es -> Eff es a -> m a
runEff handlers (Eff m) = R.runReaderT m handlers

-- | Lift a computation that uses an effect handler to Eff that requires that effect.
withHandler :: forall e es a. Has e es => (forall m. Monad m => e m -> m a) -> Eff es a
withHandler f = Eff $ do
  es <- R.ask
  R.lift $ f $ unYlppa $ rget' es

--------------------------------------------------------------------------------

class Effect (e :: (* -> *) -> *) where
  liftNat :: (forall a. n a -> m a) -> e n -> e m
  default liftNat ::
    ( G.Generic (e n)
    , G.Generic (e m)
    , GLiftNat n m (G.Rep (e n)) (G.Rep (e m))
    ) => (forall a. n a -> m a) -> e n -> e m
  liftNat f = G.to . gliftNat f . G.from

class GLiftNat (n :: * -> *) (m :: * -> *) (a :: * -> *) (b :: * -> *) where
  gliftNat :: (forall a. n a -> m a) -> a x -> b x

instance GLiftNat n m f g => GLiftNat n m (G.M1 i c f) (G.M1 i c g) where
  gliftNat f (G.M1 x) = G.M1 (gliftNat f x)

instance LiftNat n m a b => GLiftNat n m (G.K1 i a) (G.K1 i b) where
  gliftNat f (G.K1 x) = G.K1 (liftNat' f x)

class LiftNat n m a b where
  liftNat' :: (forall a. n a -> m a) -> a -> b

instance {-# INCOHERENT #-} LiftNat n m a b
    => LiftNat n m (r -> a) (r -> b) where
  liftNat' f = fmap (liftNat' f)

instance LiftNat n m (n a) (m a) where
  liftNat' f = f

instance (GLiftNat n m f1 g1, GLiftNat n m f2 g2)
    => GLiftNat n m (f1 :*: f2) (g1 :*: g2) where
  gliftNat f (x :*: y) = gliftNat f x :*: gliftNat f y

-- | Provide an effect implemented in terms of other effects.
withEffect :: Effect e => e (Eff es) -> Eff (e ': es) a -> Eff es a
withEffect e (Eff m) = Eff $ do
  es <- R.ask
  R.lift $ R.runReaderT m (Ylppa (liftNat (flip R.runReaderT es . unEff) e) :& es)

class Has r rs where
  rget' :: V.Rec f rs -> f r

instance {-# INCOHERENT #-} Has r (r ': rs) where
  rget' (x :& _) = x

instance Has r rs => Has r (x ': rs) where
  rget' (_ :& rs) = rget' rs

--------------------------------------------------------------------------------

data Reader r m = Reader { _ask :: m r }
  deriving (G.Generic, Effect)

ask :: Has (Reader r) es => Eff es r
ask = withHandler _ask

-- | Base Reader effect implemented in terms of mtl `MonadReader`
mtlReader :: R.MonadReader r m => Reader r m
mtlReader = Reader { _ask = R.ask }

-- | Reader effect that returns a given value
constReader :: Applicative m => r -> Reader r m
constReader r = Reader { _ask = pure r }

-- | Derived Reader effect handler that returns a projection of the environment
-- | of another Reader effect.
zoomReader :: Has (Reader a) e => (a -> b) -> Reader b (Eff e)
zoomReader f = Reader { _ask = f <$> ask }

--------------------------------------------------------------------------------

data State s m = State
  { _get :: m s
  , _put :: s -> m ()
  } deriving (G.Generic, Effect)

get :: Has (State s) es => Eff es s
get = withHandler _get

put :: Has (State s) es => s -> Eff es ()
put s = withHandler (\i -> _put i s)

modify :: Has (State s) es => (s -> s) -> Eff es ()
modify f = do
  s <- get
  put (f s)

-- | Base State effect implemented in terms of mtl `MonadState`
mtlState :: S.MonadState s m => State s m
mtlState = State
  { _get = S.get
  , _put = S.put
  }

--------------------------------------------------------------------------------

-- Reader can be implemented in terms of State
readOnlyState :: proxy r -> Has (State r) es => Reader r (Eff es)
readOnlyState _ = Reader { _ask = get }

--------------------------------------------------------------------------------

foo :: (Has (Reader Int) e, Has (Reader String) e, Has (State Int) e) => Eff e ()
foo = do
  x :: Int <- ask
  y :: String <- ask
  modify (\s -> s + x + length y)

runFoo :: Int
runFoo =
  flip S.execState 5 $
  runEff (Ylppa mtlState :& V.RNil) $
  withEffect (readOnlyState (Proxy :: Proxy Int)) $
  withEffect (zoomReader (show :: Int -> String)) $
  foo
