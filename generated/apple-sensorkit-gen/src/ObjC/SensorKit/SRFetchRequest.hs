{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRFetchRequest@.
module ObjC.SensorKit.SRFetchRequest
  ( SRFetchRequest
  , IsSRFetchRequest(..)
  , from
  , setFrom
  , to
  , setTo
  , device
  , setDevice
  , deviceSelector
  , fromSelector
  , setDeviceSelector
  , setFromSelector
  , setToSelector
  , toSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SensorKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Fetch data starting after this time.
--
-- This value must be specified for a valid request to be performed. If it is not specified, this will result in a SRErrorInvalidRequest error in the -sensorReader:fetchingRequest:failedWithError: SRSensorReaderDelegate callback.
--
-- The time range for fetching will be exclusive of start time and inclusive of end time: (start, end] . An SRSensorReader can use this to continue fetching a stream of data based on the last sample timestamp they have read.
--
-- ObjC selector: @- from@
from :: IsSRFetchRequest srFetchRequest => srFetchRequest -> IO CDouble
from srFetchRequest =
  sendMessage srFetchRequest fromSelector

-- | Fetch data starting after this time.
--
-- This value must be specified for a valid request to be performed. If it is not specified, this will result in a SRErrorInvalidRequest error in the -sensorReader:fetchingRequest:failedWithError: SRSensorReaderDelegate callback.
--
-- The time range for fetching will be exclusive of start time and inclusive of end time: (start, end] . An SRSensorReader can use this to continue fetching a stream of data based on the last sample timestamp they have read.
--
-- ObjC selector: @- setFrom:@
setFrom :: IsSRFetchRequest srFetchRequest => srFetchRequest -> CDouble -> IO ()
setFrom srFetchRequest value =
  sendMessage srFetchRequest setFromSelector value

-- | Fetch data ending at this time.
--
-- This value must be specified for a valid request to be performed. If it is not specified, this will result in a SRErrorInvalidRequest error in the -sensorReader:fetchingRequest:failedWithError: SRSensorReaderDelegate callback.
--
-- ObjC selector: @- to@
to :: IsSRFetchRequest srFetchRequest => srFetchRequest -> IO CDouble
to srFetchRequest =
  sendMessage srFetchRequest toSelector

-- | Fetch data ending at this time.
--
-- This value must be specified for a valid request to be performed. If it is not specified, this will result in a SRErrorInvalidRequest error in the -sensorReader:fetchingRequest:failedWithError: SRSensorReaderDelegate callback.
--
-- ObjC selector: @- setTo:@
setTo :: IsSRFetchRequest srFetchRequest => srFetchRequest -> CDouble -> IO ()
setTo srFetchRequest value =
  sendMessage srFetchRequest setToSelector value

-- | Fetch data generated on this device
--
-- If this is not specified, the current device will be used.
--
-- ObjC selector: @- device@
device :: IsSRFetchRequest srFetchRequest => srFetchRequest -> IO (Id SRDevice)
device srFetchRequest =
  sendMessage srFetchRequest deviceSelector

-- | Fetch data generated on this device
--
-- If this is not specified, the current device will be used.
--
-- ObjC selector: @- setDevice:@
setDevice :: (IsSRFetchRequest srFetchRequest, IsSRDevice value) => srFetchRequest -> value -> IO ()
setDevice srFetchRequest value =
  sendMessage srFetchRequest setDeviceSelector (toSRDevice value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @from@
fromSelector :: Selector '[] CDouble
fromSelector = mkSelector "from"

-- | @Selector@ for @setFrom:@
setFromSelector :: Selector '[CDouble] ()
setFromSelector = mkSelector "setFrom:"

-- | @Selector@ for @to@
toSelector :: Selector '[] CDouble
toSelector = mkSelector "to"

-- | @Selector@ for @setTo:@
setToSelector :: Selector '[CDouble] ()
setToSelector = mkSelector "setTo:"

-- | @Selector@ for @device@
deviceSelector :: Selector '[] (Id SRDevice)
deviceSelector = mkSelector "device"

-- | @Selector@ for @setDevice:@
setDeviceSelector :: Selector '[Id SRDevice] ()
setDeviceSelector = mkSelector "setDevice:"

