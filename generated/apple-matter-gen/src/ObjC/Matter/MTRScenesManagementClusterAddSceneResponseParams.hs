{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRScenesManagementClusterAddSceneResponseParams@.
module ObjC.Matter.MTRScenesManagementClusterAddSceneResponseParams
  ( MTRScenesManagementClusterAddSceneResponseParams
  , IsMTRScenesManagementClusterAddSceneResponseParams(..)
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

-- | Initialize an MTRScenesManagementClusterAddSceneResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRScenesManagementClusterAddSceneResponseParams mtrScenesManagementClusterAddSceneResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrScenesManagementClusterAddSceneResponseParams -> responseValue -> error_ -> IO (Id MTRScenesManagementClusterAddSceneResponseParams)
initWithResponseValue_error mtrScenesManagementClusterAddSceneResponseParams responseValue error_ =
  sendOwnedMessage mtrScenesManagementClusterAddSceneResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- status@
status :: IsMTRScenesManagementClusterAddSceneResponseParams mtrScenesManagementClusterAddSceneResponseParams => mtrScenesManagementClusterAddSceneResponseParams -> IO (Id NSNumber)
status mtrScenesManagementClusterAddSceneResponseParams =
  sendMessage mtrScenesManagementClusterAddSceneResponseParams statusSelector

-- | @- setStatus:@
setStatus :: (IsMTRScenesManagementClusterAddSceneResponseParams mtrScenesManagementClusterAddSceneResponseParams, IsNSNumber value) => mtrScenesManagementClusterAddSceneResponseParams -> value -> IO ()
setStatus mtrScenesManagementClusterAddSceneResponseParams value =
  sendMessage mtrScenesManagementClusterAddSceneResponseParams setStatusSelector (toNSNumber value)

-- | @- groupID@
groupID :: IsMTRScenesManagementClusterAddSceneResponseParams mtrScenesManagementClusterAddSceneResponseParams => mtrScenesManagementClusterAddSceneResponseParams -> IO (Id NSNumber)
groupID mtrScenesManagementClusterAddSceneResponseParams =
  sendMessage mtrScenesManagementClusterAddSceneResponseParams groupIDSelector

-- | @- setGroupID:@
setGroupID :: (IsMTRScenesManagementClusterAddSceneResponseParams mtrScenesManagementClusterAddSceneResponseParams, IsNSNumber value) => mtrScenesManagementClusterAddSceneResponseParams -> value -> IO ()
setGroupID mtrScenesManagementClusterAddSceneResponseParams value =
  sendMessage mtrScenesManagementClusterAddSceneResponseParams setGroupIDSelector (toNSNumber value)

-- | @- sceneID@
sceneID :: IsMTRScenesManagementClusterAddSceneResponseParams mtrScenesManagementClusterAddSceneResponseParams => mtrScenesManagementClusterAddSceneResponseParams -> IO (Id NSNumber)
sceneID mtrScenesManagementClusterAddSceneResponseParams =
  sendMessage mtrScenesManagementClusterAddSceneResponseParams sceneIDSelector

-- | @- setSceneID:@
setSceneID :: (IsMTRScenesManagementClusterAddSceneResponseParams mtrScenesManagementClusterAddSceneResponseParams, IsNSNumber value) => mtrScenesManagementClusterAddSceneResponseParams -> value -> IO ()
setSceneID mtrScenesManagementClusterAddSceneResponseParams value =
  sendMessage mtrScenesManagementClusterAddSceneResponseParams setSceneIDSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRScenesManagementClusterAddSceneResponseParams)
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

