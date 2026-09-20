module Snek.Util ((<.>), unwrapOr, escalate) where

import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (MonadIO, liftIO)

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
