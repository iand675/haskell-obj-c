{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCameraAVStreamManagementClusterCaptureSnapshotResponseParams@.
module ObjC.Matter.MTRCameraAVStreamManagementClusterCaptureSnapshotResponseParams
  ( MTRCameraAVStreamManagementClusterCaptureSnapshotResponseParams
  , IsMTRCameraAVStreamManagementClusterCaptureSnapshotResponseParams(..)
  , initWithResponseValue_error
  , data_
  , setData
  , imageCodec
  , setImageCodec
  , resolution
  , setResolution
  , dataSelector
  , imageCodecSelector
  , initWithResponseValue_errorSelector
  , resolutionSelector
  , setDataSelector
  , setImageCodecSelector
  , setResolutionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTRCameraAVStreamManagementClusterCaptureSnapshotResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRCameraAVStreamManagementClusterCaptureSnapshotResponseParams mtrCameraAVStreamManagementClusterCaptureSnapshotResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrCameraAVStreamManagementClusterCaptureSnapshotResponseParams -> responseValue -> error_ -> IO (Id MTRCameraAVStreamManagementClusterCaptureSnapshotResponseParams)
initWithResponseValue_error mtrCameraAVStreamManagementClusterCaptureSnapshotResponseParams responseValue error_ =
  sendOwnedMessage mtrCameraAVStreamManagementClusterCaptureSnapshotResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- data@
data_ :: IsMTRCameraAVStreamManagementClusterCaptureSnapshotResponseParams mtrCameraAVStreamManagementClusterCaptureSnapshotResponseParams => mtrCameraAVStreamManagementClusterCaptureSnapshotResponseParams -> IO (Id NSData)
data_ mtrCameraAVStreamManagementClusterCaptureSnapshotResponseParams =
  sendMessage mtrCameraAVStreamManagementClusterCaptureSnapshotResponseParams dataSelector

-- | @- setData:@
setData :: (IsMTRCameraAVStreamManagementClusterCaptureSnapshotResponseParams mtrCameraAVStreamManagementClusterCaptureSnapshotResponseParams, IsNSData value) => mtrCameraAVStreamManagementClusterCaptureSnapshotResponseParams -> value -> IO ()
setData mtrCameraAVStreamManagementClusterCaptureSnapshotResponseParams value =
  sendMessage mtrCameraAVStreamManagementClusterCaptureSnapshotResponseParams setDataSelector (toNSData value)

-- | @- imageCodec@
imageCodec :: IsMTRCameraAVStreamManagementClusterCaptureSnapshotResponseParams mtrCameraAVStreamManagementClusterCaptureSnapshotResponseParams => mtrCameraAVStreamManagementClusterCaptureSnapshotResponseParams -> IO (Id NSNumber)
imageCodec mtrCameraAVStreamManagementClusterCaptureSnapshotResponseParams =
  sendMessage mtrCameraAVStreamManagementClusterCaptureSnapshotResponseParams imageCodecSelector

-- | @- setImageCodec:@
setImageCodec :: (IsMTRCameraAVStreamManagementClusterCaptureSnapshotResponseParams mtrCameraAVStreamManagementClusterCaptureSnapshotResponseParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterCaptureSnapshotResponseParams -> value -> IO ()
setImageCodec mtrCameraAVStreamManagementClusterCaptureSnapshotResponseParams value =
  sendMessage mtrCameraAVStreamManagementClusterCaptureSnapshotResponseParams setImageCodecSelector (toNSNumber value)

-- | @- resolution@
resolution :: IsMTRCameraAVStreamManagementClusterCaptureSnapshotResponseParams mtrCameraAVStreamManagementClusterCaptureSnapshotResponseParams => mtrCameraAVStreamManagementClusterCaptureSnapshotResponseParams -> IO (Id MTRCameraAVStreamManagementClusterVideoResolutionStruct)
resolution mtrCameraAVStreamManagementClusterCaptureSnapshotResponseParams =
  sendMessage mtrCameraAVStreamManagementClusterCaptureSnapshotResponseParams resolutionSelector

-- | @- setResolution:@
setResolution :: (IsMTRCameraAVStreamManagementClusterCaptureSnapshotResponseParams mtrCameraAVStreamManagementClusterCaptureSnapshotResponseParams, IsMTRCameraAVStreamManagementClusterVideoResolutionStruct value) => mtrCameraAVStreamManagementClusterCaptureSnapshotResponseParams -> value -> IO ()
setResolution mtrCameraAVStreamManagementClusterCaptureSnapshotResponseParams value =
  sendMessage mtrCameraAVStreamManagementClusterCaptureSnapshotResponseParams setResolutionSelector (toMTRCameraAVStreamManagementClusterVideoResolutionStruct value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRCameraAVStreamManagementClusterCaptureSnapshotResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @data@
dataSelector :: Selector '[] (Id NSData)
dataSelector = mkSelector "data"

-- | @Selector@ for @setData:@
setDataSelector :: Selector '[Id NSData] ()
setDataSelector = mkSelector "setData:"

-- | @Selector@ for @imageCodec@
imageCodecSelector :: Selector '[] (Id NSNumber)
imageCodecSelector = mkSelector "imageCodec"

-- | @Selector@ for @setImageCodec:@
setImageCodecSelector :: Selector '[Id NSNumber] ()
setImageCodecSelector = mkSelector "setImageCodec:"

-- | @Selector@ for @resolution@
resolutionSelector :: Selector '[] (Id MTRCameraAVStreamManagementClusterVideoResolutionStruct)
resolutionSelector = mkSelector "resolution"

-- | @Selector@ for @setResolution:@
setResolutionSelector :: Selector '[Id MTRCameraAVStreamManagementClusterVideoResolutionStruct] ()
setResolutionSelector = mkSelector "setResolution:"

