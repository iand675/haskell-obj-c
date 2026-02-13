{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRScenesManagementClusterStoreSceneResponseParams@.
module ObjC.Matter.MTRScenesManagementClusterStoreSceneResponseParams
  ( MTRScenesManagementClusterStoreSceneResponseParams
  , IsMTRScenesManagementClusterStoreSceneResponseParams(..)
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

-- | Initialize an MTRScenesManagementClusterStoreSceneResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRScenesManagementClusterStoreSceneResponseParams mtrScenesManagementClusterStoreSceneResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrScenesManagementClusterStoreSceneResponseParams -> responseValue -> error_ -> IO (Id MTRScenesManagementClusterStoreSceneResponseParams)
initWithResponseValue_error mtrScenesManagementClusterStoreSceneResponseParams responseValue error_ =
  sendOwnedMessage mtrScenesManagementClusterStoreSceneResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- status@
status :: IsMTRScenesManagementClusterStoreSceneResponseParams mtrScenesManagementClusterStoreSceneResponseParams => mtrScenesManagementClusterStoreSceneResponseParams -> IO (Id NSNumber)
status mtrScenesManagementClusterStoreSceneResponseParams =
  sendMessage mtrScenesManagementClusterStoreSceneResponseParams statusSelector

-- | @- setStatus:@
setStatus :: (IsMTRScenesManagementClusterStoreSceneResponseParams mtrScenesManagementClusterStoreSceneResponseParams, IsNSNumber value) => mtrScenesManagementClusterStoreSceneResponseParams -> value -> IO ()
setStatus mtrScenesManagementClusterStoreSceneResponseParams value =
  sendMessage mtrScenesManagementClusterStoreSceneResponseParams setStatusSelector (toNSNumber value)

-- | @- groupID@
groupID :: IsMTRScenesManagementClusterStoreSceneResponseParams mtrScenesManagementClusterStoreSceneResponseParams => mtrScenesManagementClusterStoreSceneResponseParams -> IO (Id NSNumber)
groupID mtrScenesManagementClusterStoreSceneResponseParams =
  sendMessage mtrScenesManagementClusterStoreSceneResponseParams groupIDSelector

-- | @- setGroupID:@
setGroupID :: (IsMTRScenesManagementClusterStoreSceneResponseParams mtrScenesManagementClusterStoreSceneResponseParams, IsNSNumber value) => mtrScenesManagementClusterStoreSceneResponseParams -> value -> IO ()
setGroupID mtrScenesManagementClusterStoreSceneResponseParams value =
  sendMessage mtrScenesManagementClusterStoreSceneResponseParams setGroupIDSelector (toNSNumber value)

-- | @- sceneID@
sceneID :: IsMTRScenesManagementClusterStoreSceneResponseParams mtrScenesManagementClusterStoreSceneResponseParams => mtrScenesManagementClusterStoreSceneResponseParams -> IO (Id NSNumber)
sceneID mtrScenesManagementClusterStoreSceneResponseParams =
  sendMessage mtrScenesManagementClusterStoreSceneResponseParams sceneIDSelector

-- | @- setSceneID:@
setSceneID :: (IsMTRScenesManagementClusterStoreSceneResponseParams mtrScenesManagementClusterStoreSceneResponseParams, IsNSNumber value) => mtrScenesManagementClusterStoreSceneResponseParams -> value -> IO ()
setSceneID mtrScenesManagementClusterStoreSceneResponseParams value =
  sendMessage mtrScenesManagementClusterStoreSceneResponseParams setSceneIDSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRScenesManagementClusterStoreSceneResponseParams)
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

