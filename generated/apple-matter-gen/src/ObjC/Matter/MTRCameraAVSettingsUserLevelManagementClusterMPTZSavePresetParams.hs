{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams@.
module ObjC.Matter.MTRCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams
  ( MTRCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams
  , IsMTRCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams(..)
  , presetID
  , setPresetID
  , name
  , setName
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , nameSelector
  , presetIDSelector
  , serverSideProcessingTimeoutSelector
  , setNameSelector
  , setPresetIDSelector
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

-- | @- presetID@
presetID :: IsMTRCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams => mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams -> IO (Id NSNumber)
presetID mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams =
  sendMessage mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams presetIDSelector

-- | @- setPresetID:@
setPresetID :: (IsMTRCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams, IsNSNumber value) => mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams -> value -> IO ()
setPresetID mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams value =
  sendMessage mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams setPresetIDSelector (toNSNumber value)

-- | @- name@
name :: IsMTRCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams => mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams -> IO (Id NSString)
name mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams =
  sendMessage mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams nameSelector

-- | @- setName:@
setName :: (IsMTRCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams, IsNSString value) => mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams -> value -> IO ()
setName mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams value =
  sendMessage mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams setNameSelector (toNSString value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams => mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams =
  sendMessage mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams, IsNSNumber value) => mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams value =
  sendMessage mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams => mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams =
  sendMessage mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams, IsNSNumber value) => mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams -> value -> IO ()
setServerSideProcessingTimeout mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams value =
  sendMessage mtrCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @presetID@
presetIDSelector :: Selector '[] (Id NSNumber)
presetIDSelector = mkSelector "presetID"

-- | @Selector@ for @setPresetID:@
setPresetIDSelector :: Selector '[Id NSNumber] ()
setPresetIDSelector = mkSelector "setPresetID:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

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

