{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRScenesManagementClusterGetSceneMembershipResponseParams@.
module ObjC.Matter.MTRScenesManagementClusterGetSceneMembershipResponseParams
  ( MTRScenesManagementClusterGetSceneMembershipResponseParams
  , IsMTRScenesManagementClusterGetSceneMembershipResponseParams(..)
  , initWithResponseValue_error
  , status
  , setStatus
  , capacity
  , setCapacity
  , groupID
  , setGroupID
  , sceneList
  , setSceneList
  , capacitySelector
  , groupIDSelector
  , initWithResponseValue_errorSelector
  , sceneListSelector
  , setCapacitySelector
  , setGroupIDSelector
  , setSceneListSelector
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

-- | Initialize an MTRScenesManagementClusterGetSceneMembershipResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRScenesManagementClusterGetSceneMembershipResponseParams mtrScenesManagementClusterGetSceneMembershipResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrScenesManagementClusterGetSceneMembershipResponseParams -> responseValue -> error_ -> IO (Id MTRScenesManagementClusterGetSceneMembershipResponseParams)
initWithResponseValue_error mtrScenesManagementClusterGetSceneMembershipResponseParams responseValue error_ =
  sendOwnedMessage mtrScenesManagementClusterGetSceneMembershipResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- status@
status :: IsMTRScenesManagementClusterGetSceneMembershipResponseParams mtrScenesManagementClusterGetSceneMembershipResponseParams => mtrScenesManagementClusterGetSceneMembershipResponseParams -> IO (Id NSNumber)
status mtrScenesManagementClusterGetSceneMembershipResponseParams =
  sendMessage mtrScenesManagementClusterGetSceneMembershipResponseParams statusSelector

-- | @- setStatus:@
setStatus :: (IsMTRScenesManagementClusterGetSceneMembershipResponseParams mtrScenesManagementClusterGetSceneMembershipResponseParams, IsNSNumber value) => mtrScenesManagementClusterGetSceneMembershipResponseParams -> value -> IO ()
setStatus mtrScenesManagementClusterGetSceneMembershipResponseParams value =
  sendMessage mtrScenesManagementClusterGetSceneMembershipResponseParams setStatusSelector (toNSNumber value)

-- | @- capacity@
capacity :: IsMTRScenesManagementClusterGetSceneMembershipResponseParams mtrScenesManagementClusterGetSceneMembershipResponseParams => mtrScenesManagementClusterGetSceneMembershipResponseParams -> IO (Id NSNumber)
capacity mtrScenesManagementClusterGetSceneMembershipResponseParams =
  sendMessage mtrScenesManagementClusterGetSceneMembershipResponseParams capacitySelector

-- | @- setCapacity:@
setCapacity :: (IsMTRScenesManagementClusterGetSceneMembershipResponseParams mtrScenesManagementClusterGetSceneMembershipResponseParams, IsNSNumber value) => mtrScenesManagementClusterGetSceneMembershipResponseParams -> value -> IO ()
setCapacity mtrScenesManagementClusterGetSceneMembershipResponseParams value =
  sendMessage mtrScenesManagementClusterGetSceneMembershipResponseParams setCapacitySelector (toNSNumber value)

-- | @- groupID@
groupID :: IsMTRScenesManagementClusterGetSceneMembershipResponseParams mtrScenesManagementClusterGetSceneMembershipResponseParams => mtrScenesManagementClusterGetSceneMembershipResponseParams -> IO (Id NSNumber)
groupID mtrScenesManagementClusterGetSceneMembershipResponseParams =
  sendMessage mtrScenesManagementClusterGetSceneMembershipResponseParams groupIDSelector

-- | @- setGroupID:@
setGroupID :: (IsMTRScenesManagementClusterGetSceneMembershipResponseParams mtrScenesManagementClusterGetSceneMembershipResponseParams, IsNSNumber value) => mtrScenesManagementClusterGetSceneMembershipResponseParams -> value -> IO ()
setGroupID mtrScenesManagementClusterGetSceneMembershipResponseParams value =
  sendMessage mtrScenesManagementClusterGetSceneMembershipResponseParams setGroupIDSelector (toNSNumber value)

-- | @- sceneList@
sceneList :: IsMTRScenesManagementClusterGetSceneMembershipResponseParams mtrScenesManagementClusterGetSceneMembershipResponseParams => mtrScenesManagementClusterGetSceneMembershipResponseParams -> IO (Id NSArray)
sceneList mtrScenesManagementClusterGetSceneMembershipResponseParams =
  sendMessage mtrScenesManagementClusterGetSceneMembershipResponseParams sceneListSelector

-- | @- setSceneList:@
setSceneList :: (IsMTRScenesManagementClusterGetSceneMembershipResponseParams mtrScenesManagementClusterGetSceneMembershipResponseParams, IsNSArray value) => mtrScenesManagementClusterGetSceneMembershipResponseParams -> value -> IO ()
setSceneList mtrScenesManagementClusterGetSceneMembershipResponseParams value =
  sendMessage mtrScenesManagementClusterGetSceneMembershipResponseParams setSceneListSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRScenesManagementClusterGetSceneMembershipResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @status@
statusSelector :: Selector '[] (Id NSNumber)
statusSelector = mkSelector "status"

-- | @Selector@ for @setStatus:@
setStatusSelector :: Selector '[Id NSNumber] ()
setStatusSelector = mkSelector "setStatus:"

-- | @Selector@ for @capacity@
capacitySelector :: Selector '[] (Id NSNumber)
capacitySelector = mkSelector "capacity"

-- | @Selector@ for @setCapacity:@
setCapacitySelector :: Selector '[Id NSNumber] ()
setCapacitySelector = mkSelector "setCapacity:"

-- | @Selector@ for @groupID@
groupIDSelector :: Selector '[] (Id NSNumber)
groupIDSelector = mkSelector "groupID"

-- | @Selector@ for @setGroupID:@
setGroupIDSelector :: Selector '[Id NSNumber] ()
setGroupIDSelector = mkSelector "setGroupID:"

-- | @Selector@ for @sceneList@
sceneListSelector :: Selector '[] (Id NSArray)
sceneListSelector = mkSelector "sceneList"

-- | @Selector@ for @setSceneList:@
setSceneListSelector :: Selector '[Id NSArray] ()
setSceneListSelector = mkSelector "setSceneList:"

