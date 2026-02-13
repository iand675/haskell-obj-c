{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CMPedometer@.
module ObjC.CoreMotion.CMPedometer
  ( CMPedometer
  , IsCMPedometer(..)
  , isStepCountingAvailable
  , isDistanceAvailable
  , isFloorCountingAvailable
  , isPaceAvailable
  , isCadenceAvailable
  , isPedometerEventTrackingAvailable
  , authorizationStatus
  , queryPedometerDataFromDate_toDate_withHandler
  , startPedometerUpdatesFromDate_withHandler
  , stopPedometerUpdates
  , startPedometerEventUpdatesWithHandler
  , stopPedometerEventUpdates
  , authorizationStatusSelector
  , isCadenceAvailableSelector
  , isDistanceAvailableSelector
  , isFloorCountingAvailableSelector
  , isPaceAvailableSelector
  , isPedometerEventTrackingAvailableSelector
  , isStepCountingAvailableSelector
  , queryPedometerDataFromDate_toDate_withHandlerSelector
  , startPedometerEventUpdatesWithHandlerSelector
  , startPedometerUpdatesFromDate_withHandlerSelector
  , stopPedometerEventUpdatesSelector
  , stopPedometerUpdatesSelector

  -- * Enum types
  , CMAuthorizationStatus(CMAuthorizationStatus)
  , pattern CMAuthorizationStatusNotDetermined
  , pattern CMAuthorizationStatusRestricted
  , pattern CMAuthorizationStatusDenied
  , pattern CMAuthorizationStatusAuthorized

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreMotion.Internal.Classes
import ObjC.CoreMotion.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ isStepCountingAvailable@
isStepCountingAvailable :: IO Bool
isStepCountingAvailable  =
  do
    cls' <- getRequiredClass "CMPedometer"
    sendClassMessage cls' isStepCountingAvailableSelector

-- | @+ isDistanceAvailable@
isDistanceAvailable :: IO Bool
isDistanceAvailable  =
  do
    cls' <- getRequiredClass "CMPedometer"
    sendClassMessage cls' isDistanceAvailableSelector

-- | @+ isFloorCountingAvailable@
isFloorCountingAvailable :: IO Bool
isFloorCountingAvailable  =
  do
    cls' <- getRequiredClass "CMPedometer"
    sendClassMessage cls' isFloorCountingAvailableSelector

-- | @+ isPaceAvailable@
isPaceAvailable :: IO Bool
isPaceAvailable  =
  do
    cls' <- getRequiredClass "CMPedometer"
    sendClassMessage cls' isPaceAvailableSelector

-- | @+ isCadenceAvailable@
isCadenceAvailable :: IO Bool
isCadenceAvailable  =
  do
    cls' <- getRequiredClass "CMPedometer"
    sendClassMessage cls' isCadenceAvailableSelector

-- | @+ isPedometerEventTrackingAvailable@
isPedometerEventTrackingAvailable :: IO Bool
isPedometerEventTrackingAvailable  =
  do
    cls' <- getRequiredClass "CMPedometer"
    sendClassMessage cls' isPedometerEventTrackingAvailableSelector

-- | @+ authorizationStatus@
authorizationStatus :: IO CMAuthorizationStatus
authorizationStatus  =
  do
    cls' <- getRequiredClass "CMPedometer"
    sendClassMessage cls' authorizationStatusSelector

-- | @- queryPedometerDataFromDate:toDate:withHandler:@
queryPedometerDataFromDate_toDate_withHandler :: (IsCMPedometer cmPedometer, IsNSDate start, IsNSDate end) => cmPedometer -> start -> end -> Ptr () -> IO ()
queryPedometerDataFromDate_toDate_withHandler cmPedometer start end handler =
  sendMessage cmPedometer queryPedometerDataFromDate_toDate_withHandlerSelector (toNSDate start) (toNSDate end) handler

-- | @- startPedometerUpdatesFromDate:withHandler:@
startPedometerUpdatesFromDate_withHandler :: (IsCMPedometer cmPedometer, IsNSDate start) => cmPedometer -> start -> Ptr () -> IO ()
startPedometerUpdatesFromDate_withHandler cmPedometer start handler =
  sendMessage cmPedometer startPedometerUpdatesFromDate_withHandlerSelector (toNSDate start) handler

-- | @- stopPedometerUpdates@
stopPedometerUpdates :: IsCMPedometer cmPedometer => cmPedometer -> IO ()
stopPedometerUpdates cmPedometer =
  sendMessage cmPedometer stopPedometerUpdatesSelector

-- | @- startPedometerEventUpdatesWithHandler:@
startPedometerEventUpdatesWithHandler :: IsCMPedometer cmPedometer => cmPedometer -> Ptr () -> IO ()
startPedometerEventUpdatesWithHandler cmPedometer handler =
  sendMessage cmPedometer startPedometerEventUpdatesWithHandlerSelector handler

-- | @- stopPedometerEventUpdates@
stopPedometerEventUpdates :: IsCMPedometer cmPedometer => cmPedometer -> IO ()
stopPedometerEventUpdates cmPedometer =
  sendMessage cmPedometer stopPedometerEventUpdatesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isStepCountingAvailable@
isStepCountingAvailableSelector :: Selector '[] Bool
isStepCountingAvailableSelector = mkSelector "isStepCountingAvailable"

-- | @Selector@ for @isDistanceAvailable@
isDistanceAvailableSelector :: Selector '[] Bool
isDistanceAvailableSelector = mkSelector "isDistanceAvailable"

-- | @Selector@ for @isFloorCountingAvailable@
isFloorCountingAvailableSelector :: Selector '[] Bool
isFloorCountingAvailableSelector = mkSelector "isFloorCountingAvailable"

-- | @Selector@ for @isPaceAvailable@
isPaceAvailableSelector :: Selector '[] Bool
isPaceAvailableSelector = mkSelector "isPaceAvailable"

-- | @Selector@ for @isCadenceAvailable@
isCadenceAvailableSelector :: Selector '[] Bool
isCadenceAvailableSelector = mkSelector "isCadenceAvailable"

-- | @Selector@ for @isPedometerEventTrackingAvailable@
isPedometerEventTrackingAvailableSelector :: Selector '[] Bool
isPedometerEventTrackingAvailableSelector = mkSelector "isPedometerEventTrackingAvailable"

-- | @Selector@ for @authorizationStatus@
authorizationStatusSelector :: Selector '[] CMAuthorizationStatus
authorizationStatusSelector = mkSelector "authorizationStatus"

-- | @Selector@ for @queryPedometerDataFromDate:toDate:withHandler:@
queryPedometerDataFromDate_toDate_withHandlerSelector :: Selector '[Id NSDate, Id NSDate, Ptr ()] ()
queryPedometerDataFromDate_toDate_withHandlerSelector = mkSelector "queryPedometerDataFromDate:toDate:withHandler:"

-- | @Selector@ for @startPedometerUpdatesFromDate:withHandler:@
startPedometerUpdatesFromDate_withHandlerSelector :: Selector '[Id NSDate, Ptr ()] ()
startPedometerUpdatesFromDate_withHandlerSelector = mkSelector "startPedometerUpdatesFromDate:withHandler:"

-- | @Selector@ for @stopPedometerUpdates@
stopPedometerUpdatesSelector :: Selector '[] ()
stopPedometerUpdatesSelector = mkSelector "stopPedometerUpdates"

-- | @Selector@ for @startPedometerEventUpdatesWithHandler:@
startPedometerEventUpdatesWithHandlerSelector :: Selector '[Ptr ()] ()
startPedometerEventUpdatesWithHandlerSelector = mkSelector "startPedometerEventUpdatesWithHandler:"

-- | @Selector@ for @stopPedometerEventUpdates@
stopPedometerEventUpdatesSelector :: Selector '[] ()
stopPedometerEventUpdatesSelector = mkSelector "stopPedometerEventUpdates"

