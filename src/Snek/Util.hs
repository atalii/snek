module Snek.Util
  ( (<.>),
    unwrapOr,
    escalate,
    leftToMaybe,
    insertOr,
    failEitherWith,
    maybeToEither,
    orElse,
  )
where

import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Map

-- This <.> function is taken from Agda; https://hackage-content.haskell.org/package/Agda-2.8.0.1/docs/src/Agda.Utils.Functor.html#%3C.%3E. I'm surprised it's not in Prelude!
infixr 9 <.>

-- | Composition: pure function after functorial (monadic) function.
(<.>) :: (Functor m) => (b -> c) -> (a -> m b) -> a -> m c
(f <.> g) a = f <$> g a

unwrapOr :: (Exception e, MonadIO m) => e -> Maybe a -> m a
unwrapOr _ (Just a) = return a
unwrapOr x Nothing = liftIO $ throwIO x

escalate :: (Exception a) => Either a b -> IO b
escalate (Left x) = throwIO x
escalate (Right x) = return x

leftToMaybe :: Either a b -> Maybe a
leftToMaybe (Left a) = Just a
leftToMaybe _ = Nothing

insertOr :: (Ord k) => (k -> e) -> Map k v -> (k, v) -> Either e (Map k v)
insertOr makeErr acc (k, v) = if member k acc then Left (makeErr k) else Right (insert k v acc)

failEitherWith :: e -> Bool -> Either e ()
failEitherWith e True = Left e
failEitherWith _ False = Right ()

maybeToEither :: Maybe a -> e -> Either e a
maybeToEither (Just x) _ = Right x
maybeToEither Nothing e = Left e

orElse :: Maybe a -> Maybe a -> Maybe a
orElse (Just a) _ = Just a
orElse Nothing b = b
