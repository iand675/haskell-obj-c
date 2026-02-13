{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRScenesManagementClusterCopySceneParams@.
module ObjC.Matter.MTRScenesManagementClusterCopySceneParams
  ( MTRScenesManagementClusterCopySceneParams
  , IsMTRScenesManagementClusterCopySceneParams(..)
  , mode
  , setMode
  , groupIdentifierFrom
  , setGroupIdentifierFrom
  , sceneIdentifierFrom
  , setSceneIdentifierFrom
  , groupIdentifierTo
  , setGroupIdentifierTo
  , sceneIdentifierTo
  , setSceneIdentifierTo
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , groupIdentifierFromSelector
  , groupIdentifierToSelector
  , modeSelector
  , sceneIdentifierFromSelector
  , sceneIdentifierToSelector
  , serverSideProcessingTimeoutSelector
  , setGroupIdentifierFromSelector
  , setGroupIdentifierToSelector
  , setModeSelector
  , setSceneIdentifierFromSelector
  , setSceneIdentifierToSelector
  , setServerSideProcessingTimeoutSelector
  , setTimedInvokeTimeoutMsSelector
  , timedInvokeTimeoutMsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- mode@
mode :: IsMTRScenesManagementClusterCopySceneParams mtrScenesManagementClusterCopySceneParams => mtrScenesManagementClusterCopySceneParams -> IO (Id NSNumber)
mode mtrScenesManagementClusterCopySceneParams =
  sendMessage mtrScenesManagementClusterCopySceneParams modeSelector

-- | @- setMode:@
setMode :: (IsMTRScenesManagementClusterCopySceneParams mtrScenesManagementClusterCopySceneParams, IsNSNumber value) => mtrScenesManagementClusterCopySceneParams -> value -> IO ()
setMode mtrScenesManagementClusterCopySceneParams value =
  sendMessage mtrScenesManagementClusterCopySceneParams setModeSelector (toNSNumber value)

-- | @- groupIdentifierFrom@
groupIdentifierFrom :: IsMTRScenesManagementClusterCopySceneParams mtrScenesManagementClusterCopySceneParams => mtrScenesManagementClusterCopySceneParams -> IO (Id NSNumber)
groupIdentifierFrom mtrScenesManagementClusterCopySceneParams =
  sendMessage mtrScenesManagementClusterCopySceneParams groupIdentifierFromSelector

-- | @- setGroupIdentifierFrom:@
setGroupIdentifierFrom :: (IsMTRScenesManagementClusterCopySceneParams mtrScenesManagementClusterCopySceneParams, IsNSNumber value) => mtrScenesManagementClusterCopySceneParams -> value -> IO ()
setGroupIdentifierFrom mtrScenesManagementClusterCopySceneParams value =
  sendMessage mtrScenesManagementClusterCopySceneParams setGroupIdentifierFromSelector (toNSNumber value)

-- | @- sceneIdentifierFrom@
sceneIdentifierFrom :: IsMTRScenesManagementClusterCopySceneParams mtrScenesManagementClusterCopySceneParams => mtrScenesManagementClusterCopySceneParams -> IO (Id NSNumber)
sceneIdentifierFrom mtrScenesManagementClusterCopySceneParams =
  sendMessage mtrScenesManagementClusterCopySceneParams sceneIdentifierFromSelector

-- | @- setSceneIdentifierFrom:@
setSceneIdentifierFrom :: (IsMTRScenesManagementClusterCopySceneParams mtrScenesManagementClusterCopySceneParams, IsNSNumber value) => mtrScenesManagementClusterCopySceneParams -> value -> IO ()
setSceneIdentifierFrom mtrScenesManagementClusterCopySceneParams value =
  sendMessage mtrScenesManagementClusterCopySceneParams setSceneIdentifierFromSelector (toNSNumber value)

-- | @- groupIdentifierTo@
groupIdentifierTo :: IsMTRScenesManagementClusterCopySceneParams mtrScenesManagementClusterCopySceneParams => mtrScenesManagementClusterCopySceneParams -> IO (Id NSNumber)
groupIdentifierTo mtrScenesManagementClusterCopySceneParams =
  sendMessage mtrScenesManagementClusterCopySceneParams groupIdentifierToSelector

-- | @- setGroupIdentifierTo:@
setGroupIdentifierTo :: (IsMTRScenesManagementClusterCopySceneParams mtrScenesManagementClusterCopySceneParams, IsNSNumber value) => mtrScenesManagementClusterCopySceneParams -> value -> IO ()
setGroupIdentifierTo mtrScenesManagementClusterCopySceneParams value =
  sendMessage mtrScenesManagementClusterCopySceneParams setGroupIdentifierToSelector (toNSNumber value)

-- | @- sceneIdentifierTo@
sceneIdentifierTo :: IsMTRScenesManagementClusterCopySceneParams mtrScenesManagementClusterCopySceneParams => mtrScenesManagementClusterCopySceneParams -> IO (Id NSNumber)
sceneIdentifierTo mtrScenesManagementClusterCopySceneParams =
  sendMessage mtrScenesManagementClusterCopySceneParams sceneIdentifierToSelector

-- | @- setSceneIdentifierTo:@
setSceneIdentifierTo :: (IsMTRScenesManagementClusterCopySceneParams mtrScenesManagementClusterCopySceneParams, IsNSNumber value) => mtrScenesManagementClusterCopySceneParams -> value -> IO ()
setSceneIdentifierTo mtrScenesManagementClusterCopySceneParams value =
  sendMessage mtrScenesManagementClusterCopySceneParams setSceneIdentifierToSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRScenesManagementClusterCopySceneParams mtrScenesManagementClusterCopySceneParams => mtrScenesManagementClusterCopySceneParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrScenesManagementClusterCopySceneParams =
  sendMessage mtrScenesManagementClusterCopySceneParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRScenesManagementClusterCopySceneParams mtrScenesManagementClusterCopySceneParams, IsNSNumber value) => mtrScenesManagementClusterCopySceneParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrScenesManagementClusterCopySceneParams value =
  sendMessage mtrScenesManagementClusterCopySceneParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRScenesManagementClusterCopySceneParams mtrScenesManagementClusterCopySceneParams => mtrScenesManagementClusterCopySceneParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrScenesManagementClusterCopySceneParams =
  sendMessage mtrScenesManagementClusterCopySceneParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRScenesManagementClusterCopySceneParams mtrScenesManagementClusterCopySceneParams, IsNSNumber value) => mtrScenesManagementClusterCopySceneParams -> value -> IO ()
setServerSideProcessingTimeout mtrScenesManagementClusterCopySceneParams value =
  sendMessage mtrScenesManagementClusterCopySceneParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @mode@
modeSelector :: Selector '[] (Id NSNumber)
modeSelector = mkSelector "mode"

-- | @Selector@ for @setMode:@
setModeSelector :: Selector '[Id NSNumber] ()
setModeSelector = mkSelector "setMode:"

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

-- | @Selector@ for @groupIdentifierTo@
groupIdentifierToSelector :: Selector '[] (Id NSNumber)
groupIdentifierToSelector = mkSelector "groupIdentifierTo"

-- | @Selector@ for @setGroupIdentifierTo:@
setGroupIdentifierToSelector :: Selector '[Id NSNumber] ()
setGroupIdentifierToSelector = mkSelector "setGroupIdentifierTo:"

-- | @Selector@ for @sceneIdentifierTo@
sceneIdentifierToSelector :: Selector '[] (Id NSNumber)
sceneIdentifierToSelector = mkSelector "sceneIdentifierTo"

-- | @Selector@ for @setSceneIdentifierTo:@
setSceneIdentifierToSelector :: Selector '[Id NSNumber] ()
setSceneIdentifierToSelector = mkSelector "setSceneIdentifierTo:"

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

