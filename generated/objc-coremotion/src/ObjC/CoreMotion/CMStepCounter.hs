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

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreMotion.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ isStepCountingAvailable@
isStepCountingAvailable :: IO Bool
isStepCountingAvailable  =
  do
    cls' <- getRequiredClass "CMStepCounter"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "isStepCountingAvailable") retCULong []

-- | @- queryStepCountStartingFrom:to:toQueue:withHandler:@
queryStepCountStartingFrom_to_toQueue_withHandler :: (IsCMStepCounter cmStepCounter, IsNSDate start, IsNSDate end, IsNSOperationQueue queue) => cmStepCounter -> start -> end -> queue -> Ptr () -> IO ()
queryStepCountStartingFrom_to_toQueue_withHandler cmStepCounter  start end queue handler =
withObjCPtr start $ \raw_start ->
  withObjCPtr end $ \raw_end ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg cmStepCounter (mkSelector "queryStepCountStartingFrom:to:toQueue:withHandler:") retVoid [argPtr (castPtr raw_start :: Ptr ()), argPtr (castPtr raw_end :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

-- | @- startStepCountingUpdatesToQueue:updateOn:withHandler:@
startStepCountingUpdatesToQueue_updateOn_withHandler :: (IsCMStepCounter cmStepCounter, IsNSOperationQueue queue) => cmStepCounter -> queue -> CLong -> Ptr () -> IO ()
startStepCountingUpdatesToQueue_updateOn_withHandler cmStepCounter  queue stepCounts handler =
withObjCPtr queue $ \raw_queue ->
    sendMsg cmStepCounter (mkSelector "startStepCountingUpdatesToQueue:updateOn:withHandler:") retVoid [argPtr (castPtr raw_queue :: Ptr ()), argCLong (fromIntegral stepCounts), argPtr (castPtr handler :: Ptr ())]

-- | @- stopStepCountingUpdates@
stopStepCountingUpdates :: IsCMStepCounter cmStepCounter => cmStepCounter -> IO ()
stopStepCountingUpdates cmStepCounter  =
  sendMsg cmStepCounter (mkSelector "stopStepCountingUpdates") retVoid []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isStepCountingAvailable@
isStepCountingAvailableSelector :: Selector
isStepCountingAvailableSelector = mkSelector "isStepCountingAvailable"

-- | @Selector@ for @queryStepCountStartingFrom:to:toQueue:withHandler:@
queryStepCountStartingFrom_to_toQueue_withHandlerSelector :: Selector
queryStepCountStartingFrom_to_toQueue_withHandlerSelector = mkSelector "queryStepCountStartingFrom:to:toQueue:withHandler:"

-- | @Selector@ for @startStepCountingUpdatesToQueue:updateOn:withHandler:@
startStepCountingUpdatesToQueue_updateOn_withHandlerSelector :: Selector
startStepCountingUpdatesToQueue_updateOn_withHandlerSelector = mkSelector "startStepCountingUpdatesToQueue:updateOn:withHandler:"

-- | @Selector@ for @stopStepCountingUpdates@
stopStepCountingUpdatesSelector :: Selector
stopStepCountingUpdatesSelector = mkSelector "stopStepCountingUpdates"

