{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Family of types that contain either exception  or a value.
-- Copyright:    (c) 2015-2016, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- Family of types that contain either exception  or a value.
module Data.EVar.Type.EVar_
  where

import Control.Applicative (Applicative((<*>), pure))
import Control.Monad (Monad(return), (>=>))
import Control.Exception (Exception(toException), SomeException)
import Data.Either (Either(Left, Right), either)
import Data.Function (id)
import Data.Functor ((<$>))

import Control.Monad.Catch (MonadThrow(throwM))
import Data.Function.Between.Strict.Internal ((.), flip)


-- | Family of types that contain either exception (wrapped in 'SomeException')
-- or a value of type @a@. Type @f@ can be any container, most importantly it
-- can be e.g.:
--
-- * 'Control.Concurrent.MVar.MVar'
-- * 'Control.Concurrent.Chan.Chan'
-- * 'Data.IORef.IORef'
-- * etc.
type EVar_ f a = f (Either SomeException a)

-- | Smart constructor for 'EVar_' using a low-level function that doesn't
-- necessarily produce a side effect.
mkEVarPure_
    :: Exception e
    => (Either SomeException a -> EVar_ f a)
    -> Either e a
    -> EVar_ f a
mkEVarPure_ f = f . either (Left . toException) Right

-- | Smart constructor for 'EVar_' using a low-level function that does produce
-- a monadic side effect.
mkEVar_
    :: Exception e
    => (Either SomeException a -> m (EVar_ f a))
    -> Either e a
    -> m (EVar_ f a)
mkEVar_ f = f . either (Left . toException) Right

newEVar_
    :: Monad m
    => (Either SomeException a -> m (EVar_ f a))
    -> a
    -> m (EVar_ f a)
newEVar_ new = new . Right
{-# INLINE newEVar_ #-}

newExceptionEVar_
    :: (Exception e, Monad m)
    => (Either SomeException a -> m (EVar_ f a))
    -> e
    -> m (EVar_ f a)
newExceptionEVar_ new = new . Left . toException
{-# INLINE newExceptionEVar_ #-}

-- | Function that behaves as a type restriction for function that creates
-- empty container like 'Control.Concurrent.MVar.MVar', or
-- 'Control.Concurrent.Chan.Chan', etc.
--
-- Usage examples:
--
-- @
-- 'newEmptyEVar_' 'Control.Concurrent.MVar.newEmptyMVar'
--     :: 'System.IO.IO' (EVar_ 'Control.Concurrent.MVar.MVar' a)
--
-- 'newEmptyEVar_' 'Control.Concurrent.Chan.newChan'
--     :: 'System.IO.IO' (EVar_ 'Control.Concurrent.Chan.Chan' a)
-- @
newEmptyEVar_
    :: Monad m
    => m (EVar_ f a)
    -> m (EVar_ f a)
newEmptyEVar_ = id
{-# INLINE newEmptyEVar_ #-}

-- | Construct an 'EVar_' from a pure value for any 'Applicative' container.
pureEVar_ :: Applicative f => a -> EVar_ f a
pureEVar_ = pure . pure
{-# INLINE pureEVar_ #-}

-- | Lift '<*>' from container context in to 'EVar_' context. Works for any
-- applicative container @f@.
apEVar_ :: Applicative f => EVar_ f (a -> b) -> EVar_ f a -> EVar_ f b
apEVar_ f x = (<*>) <$> f <*> x
{-# INLINE apEVar_ #-}

-- | Construct an 'EVar_' from an exception for any 'Applicative' container.
exceptEVar_ :: (Applicative f, Exception e) => e -> EVar_ f a
exceptEVar_ = pure . Left . toException
{-# INLINE exceptEVar_ #-}

-- | Read value of 'EVar_'. This function forces exception handling so that one
-- can view value of type @a@.
readEVar_
    :: Monad m
    => (EVar_ f a -> EVar_ m a)
    -- ^ Read 'EVar_' value. This can be 'Control.Concurrent.MVar.readMVar',
    -- 'Data.IORef.readIORef', etc.
    --
    -- Type of this function with unwrapped type aliases:
    --
    -- @
    -- 'Monad' m
    -- => f ('Either' 'SomeException' a)
    -- -> m ('Either' 'SomeException' a)
    -- @
    --
    -- If we take e.g. @f = 'Data.IORef.IORef'@, then it would colapse in to:
    --
    -- @
    -- 'Monad' m
    -- => 'Data.IORef.IORef' ('Either' 'SomeException' a)
    -- -> m ('Either' 'SomeException' a)
    -- @
    --
    -- Now we can see that 'Data.IORef.readIORef' is suitable candidate for
    -- this function, in which case the type would be:
    --
    -- @
    -- 'Data.IORef.IORef' ('Either' 'SomeException' a)
    -- -> 'System.IO.IO' ('Either' 'SomeException' a)
    -- @
    -> (SomeException -> m a)
    -- ^ Exception handler.
    -> EVar_ f a
    -- ^ Variable we are reading.
    -> m a
readEVar_ read exceptionHandler = read >=> either exceptionHandler return
{-# INLINE readEVar_ #-}

-- | Variant of 'readEVar_' that uses 'throwM' as an exception handler. In
-- other words it throws that exception.
viewEVar_
    :: MonadThrow m
    => (EVar_ f a -> EVar_ m a)
    -- ^ Read 'EVar_' value. This can be 'Control.Concurrent.MVar.readMVar',
    -- 'Data.IORef.readIORef', etc.
    -> EVar_ f a
    -> m a
viewEVar_ = flip readEVar_ throwM
{-# INLINE viewEVar_ #-}
