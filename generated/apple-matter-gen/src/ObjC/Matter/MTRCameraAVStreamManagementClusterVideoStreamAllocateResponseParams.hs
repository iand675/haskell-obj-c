{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCameraAVStreamManagementClusterVideoStreamAllocateResponseParams@.
module ObjC.Matter.MTRCameraAVStreamManagementClusterVideoStreamAllocateResponseParams
  ( MTRCameraAVStreamManagementClusterVideoStreamAllocateResponseParams
  , IsMTRCameraAVStreamManagementClusterVideoStreamAllocateResponseParams(..)
  , initWithResponseValue_error
  , videoStreamID
  , setVideoStreamID
  , initWithResponseValue_errorSelector
  , setVideoStreamIDSelector
  , videoStreamIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTRCameraAVStreamManagementClusterVideoStreamAllocateResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRCameraAVStreamManagementClusterVideoStreamAllocateResponseParams mtrCameraAVStreamManagementClusterVideoStreamAllocateResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrCameraAVStreamManagementClusterVideoStreamAllocateResponseParams -> responseValue -> error_ -> IO (Id MTRCameraAVStreamManagementClusterVideoStreamAllocateResponseParams)
initWithResponseValue_error mtrCameraAVStreamManagementClusterVideoStreamAllocateResponseParams responseValue error_ =
  sendOwnedMessage mtrCameraAVStreamManagementClusterVideoStreamAllocateResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- videoStreamID@
videoStreamID :: IsMTRCameraAVStreamManagementClusterVideoStreamAllocateResponseParams mtrCameraAVStreamManagementClusterVideoStreamAllocateResponseParams => mtrCameraAVStreamManagementClusterVideoStreamAllocateResponseParams -> IO (Id NSNumber)
videoStreamID mtrCameraAVStreamManagementClusterVideoStreamAllocateResponseParams =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamAllocateResponseParams videoStreamIDSelector

-- | @- setVideoStreamID:@
setVideoStreamID :: (IsMTRCameraAVStreamManagementClusterVideoStreamAllocateResponseParams mtrCameraAVStreamManagementClusterVideoStreamAllocateResponseParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoStreamAllocateResponseParams -> value -> IO ()
setVideoStreamID mtrCameraAVStreamManagementClusterVideoStreamAllocateResponseParams value =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamAllocateResponseParams setVideoStreamIDSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRCameraAVStreamManagementClusterVideoStreamAllocateResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @videoStreamID@
videoStreamIDSelector :: Selector '[] (Id NSNumber)
videoStreamIDSelector = mkSelector "videoStreamID"

-- | @Selector@ for @setVideoStreamID:@
setVideoStreamIDSelector :: Selector '[Id NSNumber] ()
setVideoStreamIDSelector = mkSelector "setVideoStreamID:"

