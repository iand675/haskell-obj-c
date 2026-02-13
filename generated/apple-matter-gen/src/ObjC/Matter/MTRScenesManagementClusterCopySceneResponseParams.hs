{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRScenesManagementClusterCopySceneResponseParams@.
module ObjC.Matter.MTRScenesManagementClusterCopySceneResponseParams
  ( MTRScenesManagementClusterCopySceneResponseParams
  , IsMTRScenesManagementClusterCopySceneResponseParams(..)
  , initWithResponseValue_error
  , status
  , setStatus
  , groupIdentifierFrom
  , setGroupIdentifierFrom
  , sceneIdentifierFrom
  , setSceneIdentifierFrom
  , groupIdentifierFromSelector
  , initWithResponseValue_errorSelector
  , sceneIdentifierFromSelector
  , setGroupIdentifierFromSelector
  , setSceneIdentifierFromSelector
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

-- | Initialize an MTRScenesManagementClusterCopySceneResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRScenesManagementClusterCopySceneResponseParams mtrScenesManagementClusterCopySceneResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrScenesManagementClusterCopySceneResponseParams -> responseValue -> error_ -> IO (Id MTRScenesManagementClusterCopySceneResponseParams)
initWithResponseValue_error mtrScenesManagementClusterCopySceneResponseParams responseValue error_ =
  sendOwnedMessage mtrScenesManagementClusterCopySceneResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- status@
status :: IsMTRScenesManagementClusterCopySceneResponseParams mtrScenesManagementClusterCopySceneResponseParams => mtrScenesManagementClusterCopySceneResponseParams -> IO (Id NSNumber)
status mtrScenesManagementClusterCopySceneResponseParams =
  sendMessage mtrScenesManagementClusterCopySceneResponseParams statusSelector

-- | @- setStatus:@
setStatus :: (IsMTRScenesManagementClusterCopySceneResponseParams mtrScenesManagementClusterCopySceneResponseParams, IsNSNumber value) => mtrScenesManagementClusterCopySceneResponseParams -> value -> IO ()
setStatus mtrScenesManagementClusterCopySceneResponseParams value =
  sendMessage mtrScenesManagementClusterCopySceneResponseParams setStatusSelector (toNSNumber value)

-- | @- groupIdentifierFrom@
groupIdentifierFrom :: IsMTRScenesManagementClusterCopySceneResponseParams mtrScenesManagementClusterCopySceneResponseParams => mtrScenesManagementClusterCopySceneResponseParams -> IO (Id NSNumber)
groupIdentifierFrom mtrScenesManagementClusterCopySceneResponseParams =
  sendMessage mtrScenesManagementClusterCopySceneResponseParams groupIdentifierFromSelector

-- | @- setGroupIdentifierFrom:@
setGroupIdentifierFrom :: (IsMTRScenesManagementClusterCopySceneResponseParams mtrScenesManagementClusterCopySceneResponseParams, IsNSNumber value) => mtrScenesManagementClusterCopySceneResponseParams -> value -> IO ()
setGroupIdentifierFrom mtrScenesManagementClusterCopySceneResponseParams value =
  sendMessage mtrScenesManagementClusterCopySceneResponseParams setGroupIdentifierFromSelector (toNSNumber value)

-- | @- sceneIdentifierFrom@
sceneIdentifierFrom :: IsMTRScenesManagementClusterCopySceneResponseParams mtrScenesManagementClusterCopySceneResponseParams => mtrScenesManagementClusterCopySceneResponseParams -> IO (Id NSNumber)
sceneIdentifierFrom mtrScenesManagementClusterCopySceneResponseParams =
  sendMessage mtrScenesManagementClusterCopySceneResponseParams sceneIdentifierFromSelector

-- | @- setSceneIdentifierFrom:@
setSceneIdentifierFrom :: (IsMTRScenesManagementClusterCopySceneResponseParams mtrScenesManagementClusterCopySceneResponseParams, IsNSNumber value) => mtrScenesManagementClusterCopySceneResponseParams -> value -> IO ()
setSceneIdentifierFrom mtrScenesManagementClusterCopySceneResponseParams value =
  sendMessage mtrScenesManagementClusterCopySceneResponseParams setSceneIdentifierFromSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRScenesManagementClusterCopySceneResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @status@
statusSelector :: Selector '[] (Id NSNumber)
statusSelector = mkSelector "status"

-- | @Selector@ for @setStatus:@
setStatusSelector :: Selector '[Id NSNumber] ()
setStatusSelector = mkSelector "setStatus:"

-- | @Selector@ for @groupIdentifierFrom@
groupIdentifierFromSelector :: Selector '[] (Id NSNumber)
groupIdentifierFromSelector = mkSelector "groupIdentifierFrom"

-- | @Selector@ for @setGroupIdentifierFrom:@
setGroupIdentifierFromSelector :: Selector '[Id NSNumber] ()
setGroupIdentifierFromSelector = mkSelector "setGroupIdentifierFrom:"

-- | @Selector@ for @sceneIdentifierFrom@
sceneIdentifierFromSelector :: Selector '[] (Id NSNumber)
sceneIdentifierFromSelector = mkSelector "sceneIdentifierFrom"

-- | @Selector@ for @setSceneIdentifierFrom:@
setSceneIdentifierFromSelector :: Selector '[Id NSNumber] ()
setSceneIdentifierFromSelector = mkSelector "setSceneIdentifierFrom:"

