{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE UndecidableInstances #-}

module FreeTransactions where

import           Control.Monad.Free
import Data.IORef

newtype WithTransaction f a = WithTransaction (Free f a) deriving (Functor)

type UserId = Int

newtype CreateUser f a = CreateUser (UserId -> a) deriving (Functor)

data (l :+: r) f a = InL (l f a) | InR (r f a) deriving (Functor)

class Inj f g where
  inj :: forall x a. f x a -> g x a

instance Inj f f where
  inj = id

instance {-# OVERLAPPING #-} Inj f (f :+: g) where
  inj = InL

instance Inj f g => Inj f (x :+: g) where
  inj = InR . inj

newtype Fix1 f a = Fix1 { unFix1 :: f (Fix1 f) a }

instance Functor (f (Fix1 f)) => Functor (Fix1 f) where
  fmap f (Fix1 x) = Fix1 (fmap f x)

type Free' f a = Free (Fix1 f) a

-------------------------------------------------------------------

withTransaction :: (Functor (Fix1 f), Inj WithTransaction f) => Free' f a -> Free' f a
withTransaction x = liftF $ Fix1 $ inj $ WithTransaction x

createUser :: (Functor (Fix1 f), Inj CreateUser f) => Free' f UserId
createUser = liftF $ Fix1 $ inj $ CreateUser id

-------------------------------------------------------------------

run :: Free' (WithTransaction :+: CreateUser) a -> IO a
run f = do
  ref <- newIORef 0
  runWithRef ref f

runWithRef :: IORef Int -> Free' (WithTransaction :+: CreateUser) a -> IO a
runWithRef ref f = do
  let run' x =
        case x of
          InL (WithTransaction m) -> do
            putStrLn "begin transaction"
            result <- runWithRef ref m
            putStrLn "end transaction"
            pure result
          InR (CreateUser k) -> do
            nextId <- readIORef ref
            putStrLn $ "Creating user " ++ show nextId
            writeIORef ref (nextId + 1)
            pure (k nextId)
  foldFree (\(Fix1 x) -> run' x) f

-------------------------------------------------------------------

foo :: (Inj CreateUser f, Inj WithTransaction f, Functor (Fix1 f)) => Free' f [UserId]
foo = do
  u1 <- createUser
  withTransaction $ do
    u2 <- createUser
    u3 <- createUser
    pure [u1,u2,u3]

main :: IO ()
main = do
  result <- run foo
  putStrLn $ "Result: " ++ show result
