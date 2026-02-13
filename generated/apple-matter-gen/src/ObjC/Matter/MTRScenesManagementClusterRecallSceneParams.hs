{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRScenesManagementClusterRecallSceneParams@.
module ObjC.Matter.MTRScenesManagementClusterRecallSceneParams
  ( MTRScenesManagementClusterRecallSceneParams
  , IsMTRScenesManagementClusterRecallSceneParams(..)
  , groupID
  , setGroupID
  , sceneID
  , setSceneID
  , transitionTime
  , setTransitionTime
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , groupIDSelector
  , sceneIDSelector
  , serverSideProcessingTimeoutSelector
  , setGroupIDSelector
  , setSceneIDSelector
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
groupID :: IsMTRScenesManagementClusterRecallSceneParams mtrScenesManagementClusterRecallSceneParams => mtrScenesManagementClusterRecallSceneParams -> IO (Id NSNumber)
groupID mtrScenesManagementClusterRecallSceneParams =
  sendMessage mtrScenesManagementClusterRecallSceneParams groupIDSelector

-- | @- setGroupID:@
setGroupID :: (IsMTRScenesManagementClusterRecallSceneParams mtrScenesManagementClusterRecallSceneParams, IsNSNumber value) => mtrScenesManagementClusterRecallSceneParams -> value -> IO ()
setGroupID mtrScenesManagementClusterRecallSceneParams value =
  sendMessage mtrScenesManagementClusterRecallSceneParams setGroupIDSelector (toNSNumber value)

-- | @- sceneID@
sceneID :: IsMTRScenesManagementClusterRecallSceneParams mtrScenesManagementClusterRecallSceneParams => mtrScenesManagementClusterRecallSceneParams -> IO (Id NSNumber)
sceneID mtrScenesManagementClusterRecallSceneParams =
  sendMessage mtrScenesManagementClusterRecallSceneParams sceneIDSelector

-- | @- setSceneID:@
setSceneID :: (IsMTRScenesManagementClusterRecallSceneParams mtrScenesManagementClusterRecallSceneParams, IsNSNumber value) => mtrScenesManagementClusterRecallSceneParams -> value -> IO ()
setSceneID mtrScenesManagementClusterRecallSceneParams value =
  sendMessage mtrScenesManagementClusterRecallSceneParams setSceneIDSelector (toNSNumber value)

-- | @- transitionTime@
transitionTime :: IsMTRScenesManagementClusterRecallSceneParams mtrScenesManagementClusterRecallSceneParams => mtrScenesManagementClusterRecallSceneParams -> IO (Id NSNumber)
transitionTime mtrScenesManagementClusterRecallSceneParams =
  sendMessage mtrScenesManagementClusterRecallSceneParams transitionTimeSelector

-- | @- setTransitionTime:@
setTransitionTime :: (IsMTRScenesManagementClusterRecallSceneParams mtrScenesManagementClusterRecallSceneParams, IsNSNumber value) => mtrScenesManagementClusterRecallSceneParams -> value -> IO ()
setTransitionTime mtrScenesManagementClusterRecallSceneParams value =
  sendMessage mtrScenesManagementClusterRecallSceneParams setTransitionTimeSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRScenesManagementClusterRecallSceneParams mtrScenesManagementClusterRecallSceneParams => mtrScenesManagementClusterRecallSceneParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrScenesManagementClusterRecallSceneParams =
  sendMessage mtrScenesManagementClusterRecallSceneParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRScenesManagementClusterRecallSceneParams mtrScenesManagementClusterRecallSceneParams, IsNSNumber value) => mtrScenesManagementClusterRecallSceneParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrScenesManagementClusterRecallSceneParams value =
  sendMessage mtrScenesManagementClusterRecallSceneParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRScenesManagementClusterRecallSceneParams mtrScenesManagementClusterRecallSceneParams => mtrScenesManagementClusterRecallSceneParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrScenesManagementClusterRecallSceneParams =
  sendMessage mtrScenesManagementClusterRecallSceneParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRScenesManagementClusterRecallSceneParams mtrScenesManagementClusterRecallSceneParams, IsNSNumber value) => mtrScenesManagementClusterRecallSceneParams -> value -> IO ()
setServerSideProcessingTimeout mtrScenesManagementClusterRecallSceneParams value =
  sendMessage mtrScenesManagementClusterRecallSceneParams setServerSideProcessingTimeoutSelector (toNSNumber value)

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

