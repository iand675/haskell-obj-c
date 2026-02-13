{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CMStepCounter@.
module ObjC.CoreMotion.CMStepCounter
  ( CMStepCounter
  , IsCMStepCounter(..)
  , isStepCountingAvailable
  , queryStepCountStartingFrom_to_toQueue_withHandler
  , startStepCountingUpdatesToQueue_updateOn_withHandler
  , stopStepCountingUpdates
  , isStepCountingAvailableSelector
  , queryStepCountStartingFrom_to_toQueue_withHandlerSelector
  , startStepCountingUpdatesToQueue_updateOn_withHandlerSelector
  , stopStepCountingUpdatesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreMotion.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ isStepCountingAvailable@
isStepCountingAvailable :: IO Bool
isStepCountingAvailable  =
  do
    cls' <- getRequiredClass "CMStepCounter"
    sendClassMessage cls' isStepCountingAvailableSelector

-- | @- queryStepCountStartingFrom:to:toQueue:withHandler:@
queryStepCountStartingFrom_to_toQueue_withHandler :: (IsCMStepCounter cmStepCounter, IsNSDate start, IsNSDate end, IsNSOperationQueue queue) => cmStepCounter -> start -> end -> queue -> Ptr () -> IO ()
queryStepCountStartingFrom_to_toQueue_withHandler cmStepCounter start end queue handler =
  sendMessage cmStepCounter queryStepCountStartingFrom_to_toQueue_withHandlerSelector (toNSDate start) (toNSDate end) (toNSOperationQueue queue) handler

-- | @- startStepCountingUpdatesToQueue:updateOn:withHandler:@
startStepCountingUpdatesToQueue_updateOn_withHandler :: (IsCMStepCounter cmStepCounter, IsNSOperationQueue queue) => cmStepCounter -> queue -> CLong -> Ptr () -> IO ()
startStepCountingUpdatesToQueue_updateOn_withHandler cmStepCounter queue stepCounts handler =
  sendMessage cmStepCounter startStepCountingUpdatesToQueue_updateOn_withHandlerSelector (toNSOperationQueue queue) stepCounts handler

-- | @- stopStepCountingUpdates@
stopStepCountingUpdates :: IsCMStepCounter cmStepCounter => cmStepCounter -> IO ()
stopStepCountingUpdates cmStepCounter =
  sendMessage cmStepCounter stopStepCountingUpdatesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isStepCountingAvailable@
isStepCountingAvailableSelector :: Selector '[] Bool
isStepCountingAvailableSelector = mkSelector "isStepCountingAvailable"

-- | @Selector@ for @queryStepCountStartingFrom:to:toQueue:withHandler:@
queryStepCountStartingFrom_to_toQueue_withHandlerSelector :: Selector '[Id NSDate, Id NSDate, Id NSOperationQueue, Ptr ()] ()
queryStepCountStartingFrom_to_toQueue_withHandlerSelector = mkSelector "queryStepCountStartingFrom:to:toQueue:withHandler:"

-- | @Selector@ for @startStepCountingUpdatesToQueue:updateOn:withHandler:@
startStepCountingUpdatesToQueue_updateOn_withHandlerSelector :: Selector '[Id NSOperationQueue, CLong, Ptr ()] ()
startStepCountingUpdatesToQueue_updateOn_withHandlerSelector = mkSelector "startStepCountingUpdatesToQueue:updateOn:withHandler:"

-- | @Selector@ for @stopStepCountingUpdates@
stopStepCountingUpdatesSelector :: Selector '[] ()
stopStepCountingUpdatesSelector = mkSelector "stopStepCountingUpdates"

