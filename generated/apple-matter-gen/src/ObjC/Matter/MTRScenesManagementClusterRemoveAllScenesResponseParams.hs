{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRScenesManagementClusterRemoveAllScenesResponseParams@.
module ObjC.Matter.MTRScenesManagementClusterRemoveAllScenesResponseParams
  ( MTRScenesManagementClusterRemoveAllScenesResponseParams
  , IsMTRScenesManagementClusterRemoveAllScenesResponseParams(..)
  , initWithResponseValue_error
  , status
  , setStatus
  , groupID
  , setGroupID
  , groupIDSelector
  , initWithResponseValue_errorSelector
  , setGroupIDSelector
  , setStatusSelector
  , statusSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTRScenesManagementClusterRemoveAllScenesResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRScenesManagementClusterRemoveAllScenesResponseParams mtrScenesManagementClusterRemoveAllScenesResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrScenesManagementClusterRemoveAllScenesResponseParams -> responseValue -> error_ -> IO (Id MTRScenesManagementClusterRemoveAllScenesResponseParams)
initWithResponseValue_error mtrScenesManagementClusterRemoveAllScenesResponseParams responseValue error_ =
  sendOwnedMessage mtrScenesManagementClusterRemoveAllScenesResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- status@
status :: IsMTRScenesManagementClusterRemoveAllScenesResponseParams mtrScenesManagementClusterRemoveAllScenesResponseParams => mtrScenesManagementClusterRemoveAllScenesResponseParams -> IO (Id NSNumber)
status mtrScenesManagementClusterRemoveAllScenesResponseParams =
  sendMessage mtrScenesManagementClusterRemoveAllScenesResponseParams statusSelector

-- | @- setStatus:@
setStatus :: (IsMTRScenesManagementClusterRemoveAllScenesResponseParams mtrScenesManagementClusterRemoveAllScenesResponseParams, IsNSNumber value) => mtrScenesManagementClusterRemoveAllScenesResponseParams -> value -> IO ()
setStatus mtrScenesManagementClusterRemoveAllScenesResponseParams value =
  sendMessage mtrScenesManagementClusterRemoveAllScenesResponseParams setStatusSelector (toNSNumber value)

-- | @- groupID@
groupID :: IsMTRScenesManagementClusterRemoveAllScenesResponseParams mtrScenesManagementClusterRemoveAllScenesResponseParams => mtrScenesManagementClusterRemoveAllScenesResponseParams -> IO (Id NSNumber)
groupID mtrScenesManagementClusterRemoveAllScenesResponseParams =
  sendMessage mtrScenesManagementClusterRemoveAllScenesResponseParams groupIDSelector

-- | @- setGroupID:@
setGroupID :: (IsMTRScenesManagementClusterRemoveAllScenesResponseParams mtrScenesManagementClusterRemoveAllScenesResponseParams, IsNSNumber value) => mtrScenesManagementClusterRemoveAllScenesResponseParams -> value -> IO ()
setGroupID mtrScenesManagementClusterRemoveAllScenesResponseParams value =
  sendMessage mtrScenesManagementClusterRemoveAllScenesResponseParams setGroupIDSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRScenesManagementClusterRemoveAllScenesResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @status@
statusSelector :: Selector '[] (Id NSNumber)
statusSelector = mkSelector "status"

-- | @Selector@ for @setStatus:@
setStatusSelector :: Selector '[Id NSNumber] ()
setStatusSelector = mkSelector "setStatus:"

-- | @Selector@ for @groupID@
groupIDSelector :: Selector '[] (Id NSNumber)
groupIDSelector = mkSelector "groupID"

-- | @Selector@ for @setGroupID:@
setGroupIDSelector :: Selector '[Id NSNumber] ()
setGroupIDSelector = mkSelector "setGroupID:"

