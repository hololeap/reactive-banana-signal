{-# LANGUAGE LambdaCase #-}

module Reactive.Banana.Signal where

import Control.Event.Handler
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.Functor
import Data.Maybe
import Reactive.Banana
import Reactive.Banana.Frameworks
import System.Signal

allSignals :: [Signal]
allSignals = [sigABRT, sigFPE, sigILL, sigINT, sigSEGV, sigTERM]

showSignal :: Signal -> Maybe String
showSignal = \case
    2  -> Just "INT"
    4  -> Just "ILL"
    6  -> Just "ABRT"
    8  -> Just "FPE"
    11 -> Just "SEGV"
    15 -> Just "TERM"
    _  -> Nothing

registerSignal :: MonadIO m => Signal -> m (AddHandler Signal)
registerSignal s = do
    (addHandler, fire) <- liftIO newAddHandler
    liftIO $ installHandler s fire
    pure addHandler

signalsEvent :: [Signal] -> MomentIO (Event [Signal])
signalsEvent = 
    fmap fold . traverse (fmap (fmap pure) . fromAddHandler <=< registerSignal)

findE :: Foldable f => (a -> Bool) -> Event (f a) -> Event a
findE f = filterJust . fmap (find f)
