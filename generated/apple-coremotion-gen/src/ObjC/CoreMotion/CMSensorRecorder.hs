{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CMSensorRecorder@.
module ObjC.CoreMotion.CMSensorRecorder
  ( CMSensorRecorder
  , IsCMSensorRecorder(..)
  , isAccelerometerRecordingAvailable
  , authorizationStatus
  , isAuthorizedForRecording
  , accelerometerDataFromDate_toDate
  , recordAccelerometerForDuration
  , accelerometerDataFromDate_toDateSelector
  , authorizationStatusSelector
  , isAccelerometerRecordingAvailableSelector
  , isAuthorizedForRecordingSelector
  , recordAccelerometerForDurationSelector

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

-- | @+ isAccelerometerRecordingAvailable@
isAccelerometerRecordingAvailable :: IO Bool
isAccelerometerRecordingAvailable  =
  do
    cls' <- getRequiredClass "CMSensorRecorder"
    sendClassMessage cls' isAccelerometerRecordingAvailableSelector

-- | @+ authorizationStatus@
authorizationStatus :: IO CMAuthorizationStatus
authorizationStatus  =
  do
    cls' <- getRequiredClass "CMSensorRecorder"
    sendClassMessage cls' authorizationStatusSelector

-- | @+ isAuthorizedForRecording@
isAuthorizedForRecording :: IO Bool
isAuthorizedForRecording  =
  do
    cls' <- getRequiredClass "CMSensorRecorder"
    sendClassMessage cls' isAuthorizedForRecordingSelector

-- | @- accelerometerDataFromDate:toDate:@
accelerometerDataFromDate_toDate :: (IsCMSensorRecorder cmSensorRecorder, IsNSDate fromDate, IsNSDate toDate) => cmSensorRecorder -> fromDate -> toDate -> IO (Id CMSensorDataList)
accelerometerDataFromDate_toDate cmSensorRecorder fromDate toDate =
  sendMessage cmSensorRecorder accelerometerDataFromDate_toDateSelector (toNSDate fromDate) (toNSDate toDate)

-- | @- recordAccelerometerForDuration:@
recordAccelerometerForDuration :: IsCMSensorRecorder cmSensorRecorder => cmSensorRecorder -> CDouble -> IO ()
recordAccelerometerForDuration cmSensorRecorder duration =
  sendMessage cmSensorRecorder recordAccelerometerForDurationSelector duration

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isAccelerometerRecordingAvailable@
isAccelerometerRecordingAvailableSelector :: Selector '[] Bool
isAccelerometerRecordingAvailableSelector = mkSelector "isAccelerometerRecordingAvailable"

-- | @Selector@ for @authorizationStatus@
authorizationStatusSelector :: Selector '[] CMAuthorizationStatus
authorizationStatusSelector = mkSelector "authorizationStatus"

-- | @Selector@ for @isAuthorizedForRecording@
isAuthorizedForRecordingSelector :: Selector '[] Bool
isAuthorizedForRecordingSelector = mkSelector "isAuthorizedForRecording"

-- | @Selector@ for @accelerometerDataFromDate:toDate:@
accelerometerDataFromDate_toDateSelector :: Selector '[Id NSDate, Id NSDate] (Id CMSensorDataList)
accelerometerDataFromDate_toDateSelector = mkSelector "accelerometerDataFromDate:toDate:"

-- | @Selector@ for @recordAccelerometerForDuration:@
recordAccelerometerForDurationSelector :: Selector '[CDouble] ()
recordAccelerometerForDurationSelector = mkSelector "recordAccelerometerForDuration:"

