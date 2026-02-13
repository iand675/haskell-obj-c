{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCameraAVStreamManagementClusterAudioStreamAllocateResponseParams@.
module ObjC.Matter.MTRCameraAVStreamManagementClusterAudioStreamAllocateResponseParams
  ( MTRCameraAVStreamManagementClusterAudioStreamAllocateResponseParams
  , IsMTRCameraAVStreamManagementClusterAudioStreamAllocateResponseParams(..)
  , initWithResponseValue_error
  , audioStreamID
  , setAudioStreamID
  , audioStreamIDSelector
  , initWithResponseValue_errorSelector
  , setAudioStreamIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTRCameraAVStreamManagementClusterAudioStreamAllocateResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRCameraAVStreamManagementClusterAudioStreamAllocateResponseParams mtrCameraAVStreamManagementClusterAudioStreamAllocateResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrCameraAVStreamManagementClusterAudioStreamAllocateResponseParams -> responseValue -> error_ -> IO (Id MTRCameraAVStreamManagementClusterAudioStreamAllocateResponseParams)
initWithResponseValue_error mtrCameraAVStreamManagementClusterAudioStreamAllocateResponseParams responseValue error_ =
  sendOwnedMessage mtrCameraAVStreamManagementClusterAudioStreamAllocateResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- audioStreamID@
audioStreamID :: IsMTRCameraAVStreamManagementClusterAudioStreamAllocateResponseParams mtrCameraAVStreamManagementClusterAudioStreamAllocateResponseParams => mtrCameraAVStreamManagementClusterAudioStreamAllocateResponseParams -> IO (Id NSNumber)
audioStreamID mtrCameraAVStreamManagementClusterAudioStreamAllocateResponseParams =
  sendMessage mtrCameraAVStreamManagementClusterAudioStreamAllocateResponseParams audioStreamIDSelector

-- | @- setAudioStreamID:@
setAudioStreamID :: (IsMTRCameraAVStreamManagementClusterAudioStreamAllocateResponseParams mtrCameraAVStreamManagementClusterAudioStreamAllocateResponseParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterAudioStreamAllocateResponseParams -> value -> IO ()
setAudioStreamID mtrCameraAVStreamManagementClusterAudioStreamAllocateResponseParams value =
  sendMessage mtrCameraAVStreamManagementClusterAudioStreamAllocateResponseParams setAudioStreamIDSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRCameraAVStreamManagementClusterAudioStreamAllocateResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @audioStreamID@
audioStreamIDSelector :: Selector '[] (Id NSNumber)
audioStreamIDSelector = mkSelector "audioStreamID"

-- | @Selector@ for @setAudioStreamID:@
setAudioStreamIDSelector :: Selector '[Id NSNumber] ()
setAudioStreamIDSelector = mkSelector "setAudioStreamID:"

