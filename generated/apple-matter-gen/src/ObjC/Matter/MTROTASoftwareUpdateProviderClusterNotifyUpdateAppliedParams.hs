{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROTASoftwareUpdateProviderClusterNotifyUpdateAppliedParams@.
module ObjC.Matter.MTROTASoftwareUpdateProviderClusterNotifyUpdateAppliedParams
  ( MTROTASoftwareUpdateProviderClusterNotifyUpdateAppliedParams
  , IsMTROTASoftwareUpdateProviderClusterNotifyUpdateAppliedParams(..)
  , updateToken
  , setUpdateToken
  , softwareVersion
  , setSoftwareVersion
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , serverSideProcessingTimeoutSelector
  , setServerSideProcessingTimeoutSelector
  , setSoftwareVersionSelector
  , setTimedInvokeTimeoutMsSelector
  , setUpdateTokenSelector
  , softwareVersionSelector
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
updateToken :: IsMTROTASoftwareUpdateProviderClusterNotifyUpdateAppliedParams mtrotaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams => mtrotaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams -> IO (Id NSData)
updateToken mtrotaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams =
  sendMessage mtrotaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams updateTokenSelector

-- | @- setUpdateToken:@
setUpdateToken :: (IsMTROTASoftwareUpdateProviderClusterNotifyUpdateAppliedParams mtrotaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams, IsNSData value) => mtrotaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams -> value -> IO ()
setUpdateToken mtrotaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams value =
  sendMessage mtrotaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams setUpdateTokenSelector (toNSData value)

-- | @- softwareVersion@
softwareVersion :: IsMTROTASoftwareUpdateProviderClusterNotifyUpdateAppliedParams mtrotaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams => mtrotaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams -> IO (Id NSNumber)
softwareVersion mtrotaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams =
  sendMessage mtrotaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams softwareVersionSelector

-- | @- setSoftwareVersion:@
setSoftwareVersion :: (IsMTROTASoftwareUpdateProviderClusterNotifyUpdateAppliedParams mtrotaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams, IsNSNumber value) => mtrotaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams -> value -> IO ()
setSoftwareVersion mtrotaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams value =
  sendMessage mtrotaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams setSoftwareVersionSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTROTASoftwareUpdateProviderClusterNotifyUpdateAppliedParams mtrotaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams => mtrotaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrotaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams =
  sendMessage mtrotaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTROTASoftwareUpdateProviderClusterNotifyUpdateAppliedParams mtrotaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams, IsNSNumber value) => mtrotaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrotaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams value =
  sendMessage mtrotaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTROTASoftwareUpdateProviderClusterNotifyUpdateAppliedParams mtrotaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams => mtrotaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrotaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams =
  sendMessage mtrotaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTROTASoftwareUpdateProviderClusterNotifyUpdateAppliedParams mtrotaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams, IsNSNumber value) => mtrotaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams -> value -> IO ()
setServerSideProcessingTimeout mtrotaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams value =
  sendMessage mtrotaSoftwareUpdateProviderClusterNotifyUpdateAppliedParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @updateToken@
updateTokenSelector :: Selector '[] (Id NSData)
updateTokenSelector = mkSelector "updateToken"

-- | @Selector@ for @setUpdateToken:@
setUpdateTokenSelector :: Selector '[Id NSData] ()
setUpdateTokenSelector = mkSelector "setUpdateToken:"

-- | @Selector@ for @softwareVersion@
softwareVersionSelector :: Selector '[] (Id NSNumber)
softwareVersionSelector = mkSelector "softwareVersion"

-- | @Selector@ for @setSoftwareVersion:@
setSoftwareVersionSelector :: Selector '[Id NSNumber] ()
setSoftwareVersionSelector = mkSelector "setSoftwareVersion:"

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

