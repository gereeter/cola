{-# LANGUAGE RankNTypes #-}

module Control.Monad.Partial.ST where

import Control.Monad.Trans.Iter
import Control.Monad.Partial
import Control.Monad.ST (ST)
import Control.Monad.ST.Unsafe
import System.IO.Unsafe

walkST :: (forall s . IterT (ST s) a) -> Partial a
walkST act = go act where
    go (IterT m) = case unsafePerformIO (unsafeSTToIO m) of
        Pure a -> Stop a
        Iter m -> Step (go m)
