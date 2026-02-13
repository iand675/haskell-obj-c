{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSString@.
module ObjC.SensorKit.NSString
  ( NSString
  , IsNSString(..)
  , sr_sensorForDeletionRecordsFromSensor
  , sr_sensorForDeletionRecordsFromSensorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SensorKit.Internal.Classes
import ObjC.Foundation.Internal.Classes
import Data.String (IsString(..))
import ObjC.Runtime.NSString (pureNSString)

-- | Returns a sensor stream that contains deletion records of the sensor
--
-- This sensor stream should only be used for fetching. All other operations will be ignored. Deletion records share the recording and authorization state with their parent sensor.
--
-- Returns: May return nil if there is no deletion record available for this sensor
--
-- ObjC selector: @- sr_sensorForDeletionRecordsFromSensor@
sr_sensorForDeletionRecordsFromSensor :: IsNSString nsString => nsString -> IO (Id NSString)
sr_sensorForDeletionRecordsFromSensor nsString =
  sendMessage nsString sr_sensorForDeletionRecordsFromSensorSelector


-- | Allows using @OverloadedStrings@ for @Id NSString@.
--
-- >>> :set -XOverloadedStrings
-- >>> let s = "hello" :: Id NSString
instance IsString (Id NSString) where
  fromString = pureNSString
-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sr_sensorForDeletionRecordsFromSensor@
sr_sensorForDeletionRecordsFromSensorSelector :: Selector '[] (Id NSString)
sr_sensorForDeletionRecordsFromSensorSelector = mkSelector "sr_sensorForDeletionRecordsFromSensor"

