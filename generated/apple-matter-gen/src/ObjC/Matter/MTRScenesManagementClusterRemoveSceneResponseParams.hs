{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRScenesManagementClusterRemoveSceneResponseParams@.
module ObjC.Matter.MTRScenesManagementClusterRemoveSceneResponseParams
  ( MTRScenesManagementClusterRemoveSceneResponseParams
  , IsMTRScenesManagementClusterRemoveSceneResponseParams(..)
  , initWithResponseValue_error
  , status
  , setStatus
  , groupID
  , setGroupID
  , sceneID
  , setSceneID
  , groupIDSelector
  , initWithResponseValue_errorSelector
  , sceneIDSelector
  , setGroupIDSelector
  , setSceneIDSelector
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

-- | Initialize an MTRScenesManagementClusterRemoveSceneResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRScenesManagementClusterRemoveSceneResponseParams mtrScenesManagementClusterRemoveSceneResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrScenesManagementClusterRemoveSceneResponseParams -> responseValue -> error_ -> IO (Id MTRScenesManagementClusterRemoveSceneResponseParams)
initWithResponseValue_error mtrScenesManagementClusterRemoveSceneResponseParams responseValue error_ =
  sendOwnedMessage mtrScenesManagementClusterRemoveSceneResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- status@
status :: IsMTRScenesManagementClusterRemoveSceneResponseParams mtrScenesManagementClusterRemoveSceneResponseParams => mtrScenesManagementClusterRemoveSceneResponseParams -> IO (Id NSNumber)
status mtrScenesManagementClusterRemoveSceneResponseParams =
  sendMessage mtrScenesManagementClusterRemoveSceneResponseParams statusSelector

-- | @- setStatus:@
setStatus :: (IsMTRScenesManagementClusterRemoveSceneResponseParams mtrScenesManagementClusterRemoveSceneResponseParams, IsNSNumber value) => mtrScenesManagementClusterRemoveSceneResponseParams -> value -> IO ()
setStatus mtrScenesManagementClusterRemoveSceneResponseParams value =
  sendMessage mtrScenesManagementClusterRemoveSceneResponseParams setStatusSelector (toNSNumber value)

-- | @- groupID@
groupID :: IsMTRScenesManagementClusterRemoveSceneResponseParams mtrScenesManagementClusterRemoveSceneResponseParams => mtrScenesManagementClusterRemoveSceneResponseParams -> IO (Id NSNumber)
groupID mtrScenesManagementClusterRemoveSceneResponseParams =
  sendMessage mtrScenesManagementClusterRemoveSceneResponseParams groupIDSelector

-- | @- setGroupID:@
setGroupID :: (IsMTRScenesManagementClusterRemoveSceneResponseParams mtrScenesManagementClusterRemoveSceneResponseParams, IsNSNumber value) => mtrScenesManagementClusterRemoveSceneResponseParams -> value -> IO ()
setGroupID mtrScenesManagementClusterRemoveSceneResponseParams value =
  sendMessage mtrScenesManagementClusterRemoveSceneResponseParams setGroupIDSelector (toNSNumber value)

-- | @- sceneID@
sceneID :: IsMTRScenesManagementClusterRemoveSceneResponseParams mtrScenesManagementClusterRemoveSceneResponseParams => mtrScenesManagementClusterRemoveSceneResponseParams -> IO (Id NSNumber)
sceneID mtrScenesManagementClusterRemoveSceneResponseParams =
  sendMessage mtrScenesManagementClusterRemoveSceneResponseParams sceneIDSelector

-- | @- setSceneID:@
setSceneID :: (IsMTRScenesManagementClusterRemoveSceneResponseParams mtrScenesManagementClusterRemoveSceneResponseParams, IsNSNumber value) => mtrScenesManagementClusterRemoveSceneResponseParams -> value -> IO ()
setSceneID mtrScenesManagementClusterRemoveSceneResponseParams value =
  sendMessage mtrScenesManagementClusterRemoveSceneResponseParams setSceneIDSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRScenesManagementClusterRemoveSceneResponseParams)
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

-- | @Selector@ for @sceneID@
sceneIDSelector :: Selector '[] (Id NSNumber)
sceneIDSelector = mkSelector "sceneID"

-- | @Selector@ for @setSceneID:@
setSceneIDSelector :: Selector '[Id NSNumber] ()
setSceneIDSelector = mkSelector "setSceneID:"

