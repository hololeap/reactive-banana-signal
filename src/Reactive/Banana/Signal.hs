
module Reactive.Banana.Signal where

import Control.Event.Handler
import Control.Monad.IO.Class
import System.Signal

registerSignal :: MonadIO m => Signal -> m (AddHandler Signal)
registerSignal s = do
    (addHandler, fire) <- liftIO newAddHandler
    liftIO $ installHandler s fire
    pure addHandler
