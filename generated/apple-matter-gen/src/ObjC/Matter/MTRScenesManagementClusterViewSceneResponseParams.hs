{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRScenesManagementClusterViewSceneResponseParams@.
module ObjC.Matter.MTRScenesManagementClusterViewSceneResponseParams
  ( MTRScenesManagementClusterViewSceneResponseParams
  , IsMTRScenesManagementClusterViewSceneResponseParams(..)
  , initWithResponseValue_error
  , status
  , setStatus
  , groupID
  , setGroupID
  , sceneID
  , setSceneID
  , transitionTime
  , setTransitionTime
  , sceneName
  , setSceneName
  , extensionFieldSetStructs
  , setExtensionFieldSetStructs
  , extensionFieldSetStructsSelector
  , groupIDSelector
  , initWithResponseValue_errorSelector
  , sceneIDSelector
  , sceneNameSelector
  , setExtensionFieldSetStructsSelector
  , setGroupIDSelector
  , setSceneIDSelector
  , setSceneNameSelector
  , setStatusSelector
  , setTransitionTimeSelector
  , statusSelector
  , transitionTimeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTRScenesManagementClusterViewSceneResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRScenesManagementClusterViewSceneResponseParams mtrScenesManagementClusterViewSceneResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrScenesManagementClusterViewSceneResponseParams -> responseValue -> error_ -> IO (Id MTRScenesManagementClusterViewSceneResponseParams)
initWithResponseValue_error mtrScenesManagementClusterViewSceneResponseParams responseValue error_ =
  sendOwnedMessage mtrScenesManagementClusterViewSceneResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- status@
status :: IsMTRScenesManagementClusterViewSceneResponseParams mtrScenesManagementClusterViewSceneResponseParams => mtrScenesManagementClusterViewSceneResponseParams -> IO (Id NSNumber)
status mtrScenesManagementClusterViewSceneResponseParams =
  sendMessage mtrScenesManagementClusterViewSceneResponseParams statusSelector

-- | @- setStatus:@
setStatus :: (IsMTRScenesManagementClusterViewSceneResponseParams mtrScenesManagementClusterViewSceneResponseParams, IsNSNumber value) => mtrScenesManagementClusterViewSceneResponseParams -> value -> IO ()
setStatus mtrScenesManagementClusterViewSceneResponseParams value =
  sendMessage mtrScenesManagementClusterViewSceneResponseParams setStatusSelector (toNSNumber value)

-- | @- groupID@
groupID :: IsMTRScenesManagementClusterViewSceneResponseParams mtrScenesManagementClusterViewSceneResponseParams => mtrScenesManagementClusterViewSceneResponseParams -> IO (Id NSNumber)
groupID mtrScenesManagementClusterViewSceneResponseParams =
  sendMessage mtrScenesManagementClusterViewSceneResponseParams groupIDSelector

-- | @- setGroupID:@
setGroupID :: (IsMTRScenesManagementClusterViewSceneResponseParams mtrScenesManagementClusterViewSceneResponseParams, IsNSNumber value) => mtrScenesManagementClusterViewSceneResponseParams -> value -> IO ()
setGroupID mtrScenesManagementClusterViewSceneResponseParams value =
  sendMessage mtrScenesManagementClusterViewSceneResponseParams setGroupIDSelector (toNSNumber value)

-- | @- sceneID@
sceneID :: IsMTRScenesManagementClusterViewSceneResponseParams mtrScenesManagementClusterViewSceneResponseParams => mtrScenesManagementClusterViewSceneResponseParams -> IO (Id NSNumber)
sceneID mtrScenesManagementClusterViewSceneResponseParams =
  sendMessage mtrScenesManagementClusterViewSceneResponseParams sceneIDSelector

-- | @- setSceneID:@
setSceneID :: (IsMTRScenesManagementClusterViewSceneResponseParams mtrScenesManagementClusterViewSceneResponseParams, IsNSNumber value) => mtrScenesManagementClusterViewSceneResponseParams -> value -> IO ()
setSceneID mtrScenesManagementClusterViewSceneResponseParams value =
  sendMessage mtrScenesManagementClusterViewSceneResponseParams setSceneIDSelector (toNSNumber value)

-- | @- transitionTime@
transitionTime :: IsMTRScenesManagementClusterViewSceneResponseParams mtrScenesManagementClusterViewSceneResponseParams => mtrScenesManagementClusterViewSceneResponseParams -> IO (Id NSNumber)
transitionTime mtrScenesManagementClusterViewSceneResponseParams =
  sendMessage mtrScenesManagementClusterViewSceneResponseParams transitionTimeSelector

-- | @- setTransitionTime:@
setTransitionTime :: (IsMTRScenesManagementClusterViewSceneResponseParams mtrScenesManagementClusterViewSceneResponseParams, IsNSNumber value) => mtrScenesManagementClusterViewSceneResponseParams -> value -> IO ()
setTransitionTime mtrScenesManagementClusterViewSceneResponseParams value =
  sendMessage mtrScenesManagementClusterViewSceneResponseParams setTransitionTimeSelector (toNSNumber value)

-- | @- sceneName@
sceneName :: IsMTRScenesManagementClusterViewSceneResponseParams mtrScenesManagementClusterViewSceneResponseParams => mtrScenesManagementClusterViewSceneResponseParams -> IO (Id NSString)
sceneName mtrScenesManagementClusterViewSceneResponseParams =
  sendMessage mtrScenesManagementClusterViewSceneResponseParams sceneNameSelector

-- | @- setSceneName:@
setSceneName :: (IsMTRScenesManagementClusterViewSceneResponseParams mtrScenesManagementClusterViewSceneResponseParams, IsNSString value) => mtrScenesManagementClusterViewSceneResponseParams -> value -> IO ()
setSceneName mtrScenesManagementClusterViewSceneResponseParams value =
  sendMessage mtrScenesManagementClusterViewSceneResponseParams setSceneNameSelector (toNSString value)

-- | @- extensionFieldSetStructs@
extensionFieldSetStructs :: IsMTRScenesManagementClusterViewSceneResponseParams mtrScenesManagementClusterViewSceneResponseParams => mtrScenesManagementClusterViewSceneResponseParams -> IO (Id NSArray)
extensionFieldSetStructs mtrScenesManagementClusterViewSceneResponseParams =
  sendMessage mtrScenesManagementClusterViewSceneResponseParams extensionFieldSetStructsSelector

-- | @- setExtensionFieldSetStructs:@
setExtensionFieldSetStructs :: (IsMTRScenesManagementClusterViewSceneResponseParams mtrScenesManagementClusterViewSceneResponseParams, IsNSArray value) => mtrScenesManagementClusterViewSceneResponseParams -> value -> IO ()
setExtensionFieldSetStructs mtrScenesManagementClusterViewSceneResponseParams value =
  sendMessage mtrScenesManagementClusterViewSceneResponseParams setExtensionFieldSetStructsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRScenesManagementClusterViewSceneResponseParams)
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

-- | @Selector@ for @transitionTime@
transitionTimeSelector :: Selector '[] (Id NSNumber)
transitionTimeSelector = mkSelector "transitionTime"

-- | @Selector@ for @setTransitionTime:@
setTransitionTimeSelector :: Selector '[Id NSNumber] ()
setTransitionTimeSelector = mkSelector "setTransitionTime:"

-- | @Selector@ for @sceneName@
sceneNameSelector :: Selector '[] (Id NSString)
sceneNameSelector = mkSelector "sceneName"

-- | @Selector@ for @setSceneName:@
setSceneNameSelector :: Selector '[Id NSString] ()
setSceneNameSelector = mkSelector "setSceneName:"

-- | @Selector@ for @extensionFieldSetStructs@
extensionFieldSetStructsSelector :: Selector '[] (Id NSArray)
extensionFieldSetStructsSelector = mkSelector "extensionFieldSetStructs"

-- | @Selector@ for @setExtensionFieldSetStructs:@
setExtensionFieldSetStructsSelector :: Selector '[Id NSArray] ()
setExtensionFieldSetStructsSelector = mkSelector "setExtensionFieldSetStructs:"

