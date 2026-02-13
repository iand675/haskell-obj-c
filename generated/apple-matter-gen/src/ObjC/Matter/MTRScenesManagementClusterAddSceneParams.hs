{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRScenesManagementClusterAddSceneParams@.
module ObjC.Matter.MTRScenesManagementClusterAddSceneParams
  ( MTRScenesManagementClusterAddSceneParams
  , IsMTRScenesManagementClusterAddSceneParams(..)
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
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , extensionFieldSetStructsSelector
  , groupIDSelector
  , sceneIDSelector
  , sceneNameSelector
  , serverSideProcessingTimeoutSelector
  , setExtensionFieldSetStructsSelector
  , setGroupIDSelector
  , setSceneIDSelector
  , setSceneNameSelector
  , setServerSideProcessingTimeoutSelector
  , setTimedInvokeTimeoutMsSelector
  , setTransitionTimeSelector
  , timedInvokeTimeoutMsSelector
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

-- | @- groupID@
groupID :: IsMTRScenesManagementClusterAddSceneParams mtrScenesManagementClusterAddSceneParams => mtrScenesManagementClusterAddSceneParams -> IO (Id NSNumber)
groupID mtrScenesManagementClusterAddSceneParams =
  sendMessage mtrScenesManagementClusterAddSceneParams groupIDSelector

-- | @- setGroupID:@
setGroupID :: (IsMTRScenesManagementClusterAddSceneParams mtrScenesManagementClusterAddSceneParams, IsNSNumber value) => mtrScenesManagementClusterAddSceneParams -> value -> IO ()
setGroupID mtrScenesManagementClusterAddSceneParams value =
  sendMessage mtrScenesManagementClusterAddSceneParams setGroupIDSelector (toNSNumber value)

-- | @- sceneID@
sceneID :: IsMTRScenesManagementClusterAddSceneParams mtrScenesManagementClusterAddSceneParams => mtrScenesManagementClusterAddSceneParams -> IO (Id NSNumber)
sceneID mtrScenesManagementClusterAddSceneParams =
  sendMessage mtrScenesManagementClusterAddSceneParams sceneIDSelector

-- | @- setSceneID:@
setSceneID :: (IsMTRScenesManagementClusterAddSceneParams mtrScenesManagementClusterAddSceneParams, IsNSNumber value) => mtrScenesManagementClusterAddSceneParams -> value -> IO ()
setSceneID mtrScenesManagementClusterAddSceneParams value =
  sendMessage mtrScenesManagementClusterAddSceneParams setSceneIDSelector (toNSNumber value)

-- | @- transitionTime@
transitionTime :: IsMTRScenesManagementClusterAddSceneParams mtrScenesManagementClusterAddSceneParams => mtrScenesManagementClusterAddSceneParams -> IO (Id NSNumber)
transitionTime mtrScenesManagementClusterAddSceneParams =
  sendMessage mtrScenesManagementClusterAddSceneParams transitionTimeSelector

-- | @- setTransitionTime:@
setTransitionTime :: (IsMTRScenesManagementClusterAddSceneParams mtrScenesManagementClusterAddSceneParams, IsNSNumber value) => mtrScenesManagementClusterAddSceneParams -> value -> IO ()
setTransitionTime mtrScenesManagementClusterAddSceneParams value =
  sendMessage mtrScenesManagementClusterAddSceneParams setTransitionTimeSelector (toNSNumber value)

-- | @- sceneName@
sceneName :: IsMTRScenesManagementClusterAddSceneParams mtrScenesManagementClusterAddSceneParams => mtrScenesManagementClusterAddSceneParams -> IO (Id NSString)
sceneName mtrScenesManagementClusterAddSceneParams =
  sendMessage mtrScenesManagementClusterAddSceneParams sceneNameSelector

-- | @- setSceneName:@
setSceneName :: (IsMTRScenesManagementClusterAddSceneParams mtrScenesManagementClusterAddSceneParams, IsNSString value) => mtrScenesManagementClusterAddSceneParams -> value -> IO ()
setSceneName mtrScenesManagementClusterAddSceneParams value =
  sendMessage mtrScenesManagementClusterAddSceneParams setSceneNameSelector (toNSString value)

-- | @- extensionFieldSetStructs@
extensionFieldSetStructs :: IsMTRScenesManagementClusterAddSceneParams mtrScenesManagementClusterAddSceneParams => mtrScenesManagementClusterAddSceneParams -> IO (Id NSArray)
extensionFieldSetStructs mtrScenesManagementClusterAddSceneParams =
  sendMessage mtrScenesManagementClusterAddSceneParams extensionFieldSetStructsSelector

-- | @- setExtensionFieldSetStructs:@
setExtensionFieldSetStructs :: (IsMTRScenesManagementClusterAddSceneParams mtrScenesManagementClusterAddSceneParams, IsNSArray value) => mtrScenesManagementClusterAddSceneParams -> value -> IO ()
setExtensionFieldSetStructs mtrScenesManagementClusterAddSceneParams value =
  sendMessage mtrScenesManagementClusterAddSceneParams setExtensionFieldSetStructsSelector (toNSArray value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRScenesManagementClusterAddSceneParams mtrScenesManagementClusterAddSceneParams => mtrScenesManagementClusterAddSceneParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrScenesManagementClusterAddSceneParams =
  sendMessage mtrScenesManagementClusterAddSceneParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRScenesManagementClusterAddSceneParams mtrScenesManagementClusterAddSceneParams, IsNSNumber value) => mtrScenesManagementClusterAddSceneParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrScenesManagementClusterAddSceneParams value =
  sendMessage mtrScenesManagementClusterAddSceneParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRScenesManagementClusterAddSceneParams mtrScenesManagementClusterAddSceneParams => mtrScenesManagementClusterAddSceneParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrScenesManagementClusterAddSceneParams =
  sendMessage mtrScenesManagementClusterAddSceneParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRScenesManagementClusterAddSceneParams mtrScenesManagementClusterAddSceneParams, IsNSNumber value) => mtrScenesManagementClusterAddSceneParams -> value -> IO ()
setServerSideProcessingTimeout mtrScenesManagementClusterAddSceneParams value =
  sendMessage mtrScenesManagementClusterAddSceneParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

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

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector '[] (Id NSNumber)
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector '[Id NSNumber] ()
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

-- | @Selector@ for @serverSideProcessingTimeout@
serverSideProcessingTimeoutSelector :: Selector '[] (Id NSNumber)
serverSideProcessingTimeoutSelector = mkSelector "serverSideProcessingTimeout"

-- | @Selector@ for @setServerSideProcessingTimeout:@
setServerSideProcessingTimeoutSelector :: Selector '[Id NSNumber] ()
setServerSideProcessingTimeoutSelector = mkSelector "setServerSideProcessingTimeout:"

