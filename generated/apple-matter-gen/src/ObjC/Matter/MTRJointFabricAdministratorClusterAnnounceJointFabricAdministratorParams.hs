{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRJointFabricAdministratorClusterAnnounceJointFabricAdministratorParams@.
module ObjC.Matter.MTRJointFabricAdministratorClusterAnnounceJointFabricAdministratorParams
  ( MTRJointFabricAdministratorClusterAnnounceJointFabricAdministratorParams
  , IsMTRJointFabricAdministratorClusterAnnounceJointFabricAdministratorParams(..)
  , endpointID
  , setEndpointID
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , endpointIDSelector
  , serverSideProcessingTimeoutSelector
  , setEndpointIDSelector
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

-- | @- endpointID@
endpointID :: IsMTRJointFabricAdministratorClusterAnnounceJointFabricAdministratorParams mtrJointFabricAdministratorClusterAnnounceJointFabricAdministratorParams => mtrJointFabricAdministratorClusterAnnounceJointFabricAdministratorParams -> IO (Id NSNumber)
endpointID mtrJointFabricAdministratorClusterAnnounceJointFabricAdministratorParams =
  sendMessage mtrJointFabricAdministratorClusterAnnounceJointFabricAdministratorParams endpointIDSelector

-- | @- setEndpointID:@
setEndpointID :: (IsMTRJointFabricAdministratorClusterAnnounceJointFabricAdministratorParams mtrJointFabricAdministratorClusterAnnounceJointFabricAdministratorParams, IsNSNumber value) => mtrJointFabricAdministratorClusterAnnounceJointFabricAdministratorParams -> value -> IO ()
setEndpointID mtrJointFabricAdministratorClusterAnnounceJointFabricAdministratorParams value =
  sendMessage mtrJointFabricAdministratorClusterAnnounceJointFabricAdministratorParams setEndpointIDSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRJointFabricAdministratorClusterAnnounceJointFabricAdministratorParams mtrJointFabricAdministratorClusterAnnounceJointFabricAdministratorParams => mtrJointFabricAdministratorClusterAnnounceJointFabricAdministratorParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrJointFabricAdministratorClusterAnnounceJointFabricAdministratorParams =
  sendMessage mtrJointFabricAdministratorClusterAnnounceJointFabricAdministratorParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRJointFabricAdministratorClusterAnnounceJointFabricAdministratorParams mtrJointFabricAdministratorClusterAnnounceJointFabricAdministratorParams, IsNSNumber value) => mtrJointFabricAdministratorClusterAnnounceJointFabricAdministratorParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrJointFabricAdministratorClusterAnnounceJointFabricAdministratorParams value =
  sendMessage mtrJointFabricAdministratorClusterAnnounceJointFabricAdministratorParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRJointFabricAdministratorClusterAnnounceJointFabricAdministratorParams mtrJointFabricAdministratorClusterAnnounceJointFabricAdministratorParams => mtrJointFabricAdministratorClusterAnnounceJointFabricAdministratorParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrJointFabricAdministratorClusterAnnounceJointFabricAdministratorParams =
  sendMessage mtrJointFabricAdministratorClusterAnnounceJointFabricAdministratorParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRJointFabricAdministratorClusterAnnounceJointFabricAdministratorParams mtrJointFabricAdministratorClusterAnnounceJointFabricAdministratorParams, IsNSNumber value) => mtrJointFabricAdministratorClusterAnnounceJointFabricAdministratorParams -> value -> IO ()
setServerSideProcessingTimeout mtrJointFabricAdministratorClusterAnnounceJointFabricAdministratorParams value =
  sendMessage mtrJointFabricAdministratorClusterAnnounceJointFabricAdministratorParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @endpointID@
endpointIDSelector :: Selector '[] (Id NSNumber)
endpointIDSelector = mkSelector "endpointID"

-- | @Selector@ for @setEndpointID:@
setEndpointIDSelector :: Selector '[Id NSNumber] ()
setEndpointIDSelector = mkSelector "setEndpointID:"

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

