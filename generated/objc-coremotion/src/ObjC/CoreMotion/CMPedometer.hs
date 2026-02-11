{-# LANGUAGE PatternSynonyms #-}
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
  , isStepCountingAvailableSelector
  , isDistanceAvailableSelector
  , isFloorCountingAvailableSelector
  , isPaceAvailableSelector
  , isCadenceAvailableSelector
  , isPedometerEventTrackingAvailableSelector
  , authorizationStatusSelector
  , queryPedometerDataFromDate_toDate_withHandlerSelector
  , startPedometerUpdatesFromDate_withHandlerSelector
  , stopPedometerUpdatesSelector
  , startPedometerEventUpdatesWithHandlerSelector
  , stopPedometerEventUpdatesSelector

  -- * Enum types
  , CMAuthorizationStatus(CMAuthorizationStatus)
  , pattern CMAuthorizationStatusNotDetermined
  , pattern CMAuthorizationStatusRestricted
  , pattern CMAuthorizationStatusDenied
  , pattern CMAuthorizationStatusAuthorized

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
import ObjC.CoreMotion.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ isStepCountingAvailable@
isStepCountingAvailable :: IO Bool
isStepCountingAvailable  =
  do
    cls' <- getRequiredClass "CMPedometer"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "isStepCountingAvailable") retCULong []

-- | @+ isDistanceAvailable@
isDistanceAvailable :: IO Bool
isDistanceAvailable  =
  do
    cls' <- getRequiredClass "CMPedometer"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "isDistanceAvailable") retCULong []

-- | @+ isFloorCountingAvailable@
isFloorCountingAvailable :: IO Bool
isFloorCountingAvailable  =
  do
    cls' <- getRequiredClass "CMPedometer"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "isFloorCountingAvailable") retCULong []

-- | @+ isPaceAvailable@
isPaceAvailable :: IO Bool
isPaceAvailable  =
  do
    cls' <- getRequiredClass "CMPedometer"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "isPaceAvailable") retCULong []

-- | @+ isCadenceAvailable@
isCadenceAvailable :: IO Bool
isCadenceAvailable  =
  do
    cls' <- getRequiredClass "CMPedometer"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "isCadenceAvailable") retCULong []

-- | @+ isPedometerEventTrackingAvailable@
isPedometerEventTrackingAvailable :: IO Bool
isPedometerEventTrackingAvailable  =
  do
    cls' <- getRequiredClass "CMPedometer"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "isPedometerEventTrackingAvailable") retCULong []

-- | @+ authorizationStatus@
authorizationStatus :: IO CMAuthorizationStatus
authorizationStatus  =
  do
    cls' <- getRequiredClass "CMPedometer"
    fmap (coerce :: CLong -> CMAuthorizationStatus) $ sendClassMsg cls' (mkSelector "authorizationStatus") retCLong []

-- | @- queryPedometerDataFromDate:toDate:withHandler:@
queryPedometerDataFromDate_toDate_withHandler :: (IsCMPedometer cmPedometer, IsNSDate start, IsNSDate end) => cmPedometer -> start -> end -> Ptr () -> IO ()
queryPedometerDataFromDate_toDate_withHandler cmPedometer  start end handler =
withObjCPtr start $ \raw_start ->
  withObjCPtr end $ \raw_end ->
      sendMsg cmPedometer (mkSelector "queryPedometerDataFromDate:toDate:withHandler:") retVoid [argPtr (castPtr raw_start :: Ptr ()), argPtr (castPtr raw_end :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

-- | @- startPedometerUpdatesFromDate:withHandler:@
startPedometerUpdatesFromDate_withHandler :: (IsCMPedometer cmPedometer, IsNSDate start) => cmPedometer -> start -> Ptr () -> IO ()
startPedometerUpdatesFromDate_withHandler cmPedometer  start handler =
withObjCPtr start $ \raw_start ->
    sendMsg cmPedometer (mkSelector "startPedometerUpdatesFromDate:withHandler:") retVoid [argPtr (castPtr raw_start :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

-- | @- stopPedometerUpdates@
stopPedometerUpdates :: IsCMPedometer cmPedometer => cmPedometer -> IO ()
stopPedometerUpdates cmPedometer  =
  sendMsg cmPedometer (mkSelector "stopPedometerUpdates") retVoid []

-- | @- startPedometerEventUpdatesWithHandler:@
startPedometerEventUpdatesWithHandler :: IsCMPedometer cmPedometer => cmPedometer -> Ptr () -> IO ()
startPedometerEventUpdatesWithHandler cmPedometer  handler =
  sendMsg cmPedometer (mkSelector "startPedometerEventUpdatesWithHandler:") retVoid [argPtr (castPtr handler :: Ptr ())]

-- | @- stopPedometerEventUpdates@
stopPedometerEventUpdates :: IsCMPedometer cmPedometer => cmPedometer -> IO ()
stopPedometerEventUpdates cmPedometer  =
  sendMsg cmPedometer (mkSelector "stopPedometerEventUpdates") retVoid []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isStepCountingAvailable@
isStepCountingAvailableSelector :: Selector
isStepCountingAvailableSelector = mkSelector "isStepCountingAvailable"

-- | @Selector@ for @isDistanceAvailable@
isDistanceAvailableSelector :: Selector
isDistanceAvailableSelector = mkSelector "isDistanceAvailable"

-- | @Selector@ for @isFloorCountingAvailable@
isFloorCountingAvailableSelector :: Selector
isFloorCountingAvailableSelector = mkSelector "isFloorCountingAvailable"

-- | @Selector@ for @isPaceAvailable@
isPaceAvailableSelector :: Selector
isPaceAvailableSelector = mkSelector "isPaceAvailable"

-- | @Selector@ for @isCadenceAvailable@
isCadenceAvailableSelector :: Selector
isCadenceAvailableSelector = mkSelector "isCadenceAvailable"

-- | @Selector@ for @isPedometerEventTrackingAvailable@
isPedometerEventTrackingAvailableSelector :: Selector
isPedometerEventTrackingAvailableSelector = mkSelector "isPedometerEventTrackingAvailable"

-- | @Selector@ for @authorizationStatus@
authorizationStatusSelector :: Selector
authorizationStatusSelector = mkSelector "authorizationStatus"

-- | @Selector@ for @queryPedometerDataFromDate:toDate:withHandler:@
queryPedometerDataFromDate_toDate_withHandlerSelector :: Selector
queryPedometerDataFromDate_toDate_withHandlerSelector = mkSelector "queryPedometerDataFromDate:toDate:withHandler:"

-- | @Selector@ for @startPedometerUpdatesFromDate:withHandler:@
startPedometerUpdatesFromDate_withHandlerSelector :: Selector
startPedometerUpdatesFromDate_withHandlerSelector = mkSelector "startPedometerUpdatesFromDate:withHandler:"

-- | @Selector@ for @stopPedometerUpdates@
stopPedometerUpdatesSelector :: Selector
stopPedometerUpdatesSelector = mkSelector "stopPedometerUpdates"

-- | @Selector@ for @startPedometerEventUpdatesWithHandler:@
startPedometerEventUpdatesWithHandlerSelector :: Selector
startPedometerEventUpdatesWithHandlerSelector = mkSelector "startPedometerEventUpdatesWithHandler:"

-- | @Selector@ for @stopPedometerEventUpdates@
stopPedometerEventUpdatesSelector :: Selector
stopPedometerEventUpdatesSelector = mkSelector "stopPedometerEventUpdates"

