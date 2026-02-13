{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCameraAVStreamManagementClusterSnapshotStreamAllocateResponseParams@.
module ObjC.Matter.MTRCameraAVStreamManagementClusterSnapshotStreamAllocateResponseParams
  ( MTRCameraAVStreamManagementClusterSnapshotStreamAllocateResponseParams
  , IsMTRCameraAVStreamManagementClusterSnapshotStreamAllocateResponseParams(..)
  , initWithResponseValue_error
  , snapshotStreamID
  , setSnapshotStreamID
  , initWithResponseValue_errorSelector
  , setSnapshotStreamIDSelector
  , snapshotStreamIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTRCameraAVStreamManagementClusterSnapshotStreamAllocateResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRCameraAVStreamManagementClusterSnapshotStreamAllocateResponseParams mtrCameraAVStreamManagementClusterSnapshotStreamAllocateResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrCameraAVStreamManagementClusterSnapshotStreamAllocateResponseParams -> responseValue -> error_ -> IO (Id MTRCameraAVStreamManagementClusterSnapshotStreamAllocateResponseParams)
initWithResponseValue_error mtrCameraAVStreamManagementClusterSnapshotStreamAllocateResponseParams responseValue error_ =
  sendOwnedMessage mtrCameraAVStreamManagementClusterSnapshotStreamAllocateResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- snapshotStreamID@
snapshotStreamID :: IsMTRCameraAVStreamManagementClusterSnapshotStreamAllocateResponseParams mtrCameraAVStreamManagementClusterSnapshotStreamAllocateResponseParams => mtrCameraAVStreamManagementClusterSnapshotStreamAllocateResponseParams -> IO (Id NSNumber)
snapshotStreamID mtrCameraAVStreamManagementClusterSnapshotStreamAllocateResponseParams =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotStreamAllocateResponseParams snapshotStreamIDSelector

-- | @- setSnapshotStreamID:@
setSnapshotStreamID :: (IsMTRCameraAVStreamManagementClusterSnapshotStreamAllocateResponseParams mtrCameraAVStreamManagementClusterSnapshotStreamAllocateResponseParams, IsNSNumber value) => mtrCameraAVStreamManagementClusterSnapshotStreamAllocateResponseParams -> value -> IO ()
setSnapshotStreamID mtrCameraAVStreamManagementClusterSnapshotStreamAllocateResponseParams value =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotStreamAllocateResponseParams setSnapshotStreamIDSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRCameraAVStreamManagementClusterSnapshotStreamAllocateResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @snapshotStreamID@
snapshotStreamIDSelector :: Selector '[] (Id NSNumber)
snapshotStreamIDSelector = mkSelector "snapshotStreamID"

-- | @Selector@ for @setSnapshotStreamID:@
setSnapshotStreamIDSelector :: Selector '[Id NSNumber] ()
setSnapshotStreamIDSelector = mkSelector "setSnapshotStreamID:"

