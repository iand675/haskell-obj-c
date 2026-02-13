{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROTASoftwareUpdateProviderClusterApplyUpdateRequestParams@.
module ObjC.Matter.MTROTASoftwareUpdateProviderClusterApplyUpdateRequestParams
  ( MTROTASoftwareUpdateProviderClusterApplyUpdateRequestParams
  , IsMTROTASoftwareUpdateProviderClusterApplyUpdateRequestParams(..)
  , updateToken
  , setUpdateToken
  , newVersion
  , setNewVersion
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , newVersionSelector
  , serverSideProcessingTimeoutSelector
  , setNewVersionSelector
  , setServerSideProcessingTimeoutSelector
  , setTimedInvokeTimeoutMsSelector
  , setUpdateTokenSelector
  , timedInvokeTimeoutMsSelector
  , updateTokenSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- updateToken@
updateToken :: IsMTROTASoftwareUpdateProviderClusterApplyUpdateRequestParams mtrotaSoftwareUpdateProviderClusterApplyUpdateRequestParams => mtrotaSoftwareUpdateProviderClusterApplyUpdateRequestParams -> IO (Id NSData)
updateToken mtrotaSoftwareUpdateProviderClusterApplyUpdateRequestParams =
  sendMessage mtrotaSoftwareUpdateProviderClusterApplyUpdateRequestParams updateTokenSelector

-- | @- setUpdateToken:@
setUpdateToken :: (IsMTROTASoftwareUpdateProviderClusterApplyUpdateRequestParams mtrotaSoftwareUpdateProviderClusterApplyUpdateRequestParams, IsNSData value) => mtrotaSoftwareUpdateProviderClusterApplyUpdateRequestParams -> value -> IO ()
setUpdateToken mtrotaSoftwareUpdateProviderClusterApplyUpdateRequestParams value =
  sendMessage mtrotaSoftwareUpdateProviderClusterApplyUpdateRequestParams setUpdateTokenSelector (toNSData value)

-- | @- newVersion@
newVersion :: IsMTROTASoftwareUpdateProviderClusterApplyUpdateRequestParams mtrotaSoftwareUpdateProviderClusterApplyUpdateRequestParams => mtrotaSoftwareUpdateProviderClusterApplyUpdateRequestParams -> IO (Id NSNumber)
newVersion mtrotaSoftwareUpdateProviderClusterApplyUpdateRequestParams =
  sendOwnedMessage mtrotaSoftwareUpdateProviderClusterApplyUpdateRequestParams newVersionSelector

-- | @- setNewVersion:@
setNewVersion :: (IsMTROTASoftwareUpdateProviderClusterApplyUpdateRequestParams mtrotaSoftwareUpdateProviderClusterApplyUpdateRequestParams, IsNSNumber value) => mtrotaSoftwareUpdateProviderClusterApplyUpdateRequestParams -> value -> IO ()
setNewVersion mtrotaSoftwareUpdateProviderClusterApplyUpdateRequestParams value =
  sendMessage mtrotaSoftwareUpdateProviderClusterApplyUpdateRequestParams setNewVersionSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTROTASoftwareUpdateProviderClusterApplyUpdateRequestParams mtrotaSoftwareUpdateProviderClusterApplyUpdateRequestParams => mtrotaSoftwareUpdateProviderClusterApplyUpdateRequestParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrotaSoftwareUpdateProviderClusterApplyUpdateRequestParams =
  sendMessage mtrotaSoftwareUpdateProviderClusterApplyUpdateRequestParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTROTASoftwareUpdateProviderClusterApplyUpdateRequestParams mtrotaSoftwareUpdateProviderClusterApplyUpdateRequestParams, IsNSNumber value) => mtrotaSoftwareUpdateProviderClusterApplyUpdateRequestParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrotaSoftwareUpdateProviderClusterApplyUpdateRequestParams value =
  sendMessage mtrotaSoftwareUpdateProviderClusterApplyUpdateRequestParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTROTASoftwareUpdateProviderClusterApplyUpdateRequestParams mtrotaSoftwareUpdateProviderClusterApplyUpdateRequestParams => mtrotaSoftwareUpdateProviderClusterApplyUpdateRequestParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrotaSoftwareUpdateProviderClusterApplyUpdateRequestParams =
  sendMessage mtrotaSoftwareUpdateProviderClusterApplyUpdateRequestParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTROTASoftwareUpdateProviderClusterApplyUpdateRequestParams mtrotaSoftwareUpdateProviderClusterApplyUpdateRequestParams, IsNSNumber value) => mtrotaSoftwareUpdateProviderClusterApplyUpdateRequestParams -> value -> IO ()
setServerSideProcessingTimeout mtrotaSoftwareUpdateProviderClusterApplyUpdateRequestParams value =
  sendMessage mtrotaSoftwareUpdateProviderClusterApplyUpdateRequestParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @updateToken@
updateTokenSelector :: Selector '[] (Id NSData)
updateTokenSelector = mkSelector "updateToken"

-- | @Selector@ for @setUpdateToken:@
setUpdateTokenSelector :: Selector '[Id NSData] ()
setUpdateTokenSelector = mkSelector "setUpdateToken:"

-- | @Selector@ for @newVersion@
newVersionSelector :: Selector '[] (Id NSNumber)
newVersionSelector = mkSelector "newVersion"

-- | @Selector@ for @setNewVersion:@
setNewVersionSelector :: Selector '[Id NSNumber] ()
setNewVersionSelector = mkSelector "setNewVersion:"

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

