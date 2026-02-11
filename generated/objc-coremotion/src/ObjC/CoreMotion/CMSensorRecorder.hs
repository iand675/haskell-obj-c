{-# LANGUAGE PatternSynonyms #-}
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
  , isAccelerometerRecordingAvailableSelector
  , authorizationStatusSelector
  , isAuthorizedForRecordingSelector
  , accelerometerDataFromDate_toDateSelector
  , recordAccelerometerForDurationSelector

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

-- | @+ isAccelerometerRecordingAvailable@
isAccelerometerRecordingAvailable :: IO Bool
isAccelerometerRecordingAvailable  =
  do
    cls' <- getRequiredClass "CMSensorRecorder"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "isAccelerometerRecordingAvailable") retCULong []

-- | @+ authorizationStatus@
authorizationStatus :: IO CMAuthorizationStatus
authorizationStatus  =
  do
    cls' <- getRequiredClass "CMSensorRecorder"
    fmap (coerce :: CLong -> CMAuthorizationStatus) $ sendClassMsg cls' (mkSelector "authorizationStatus") retCLong []

-- | @+ isAuthorizedForRecording@
isAuthorizedForRecording :: IO Bool
isAuthorizedForRecording  =
  do
    cls' <- getRequiredClass "CMSensorRecorder"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "isAuthorizedForRecording") retCULong []

-- | @- accelerometerDataFromDate:toDate:@
accelerometerDataFromDate_toDate :: (IsCMSensorRecorder cmSensorRecorder, IsNSDate fromDate, IsNSDate toDate) => cmSensorRecorder -> fromDate -> toDate -> IO (Id CMSensorDataList)
accelerometerDataFromDate_toDate cmSensorRecorder  fromDate toDate =
withObjCPtr fromDate $ \raw_fromDate ->
  withObjCPtr toDate $ \raw_toDate ->
      sendMsg cmSensorRecorder (mkSelector "accelerometerDataFromDate:toDate:") (retPtr retVoid) [argPtr (castPtr raw_fromDate :: Ptr ()), argPtr (castPtr raw_toDate :: Ptr ())] >>= retainedObject . castPtr

-- | @- recordAccelerometerForDuration:@
recordAccelerometerForDuration :: IsCMSensorRecorder cmSensorRecorder => cmSensorRecorder -> CDouble -> IO ()
recordAccelerometerForDuration cmSensorRecorder  duration =
  sendMsg cmSensorRecorder (mkSelector "recordAccelerometerForDuration:") retVoid [argCDouble (fromIntegral duration)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isAccelerometerRecordingAvailable@
isAccelerometerRecordingAvailableSelector :: Selector
isAccelerometerRecordingAvailableSelector = mkSelector "isAccelerometerRecordingAvailable"

-- | @Selector@ for @authorizationStatus@
authorizationStatusSelector :: Selector
authorizationStatusSelector = mkSelector "authorizationStatus"

-- | @Selector@ for @isAuthorizedForRecording@
isAuthorizedForRecordingSelector :: Selector
isAuthorizedForRecordingSelector = mkSelector "isAuthorizedForRecording"

-- | @Selector@ for @accelerometerDataFromDate:toDate:@
accelerometerDataFromDate_toDateSelector :: Selector
accelerometerDataFromDate_toDateSelector = mkSelector "accelerometerDataFromDate:toDate:"

-- | @Selector@ for @recordAccelerometerForDuration:@
recordAccelerometerForDurationSelector :: Selector
recordAccelerometerForDurationSelector = mkSelector "recordAccelerometerForDuration:"

