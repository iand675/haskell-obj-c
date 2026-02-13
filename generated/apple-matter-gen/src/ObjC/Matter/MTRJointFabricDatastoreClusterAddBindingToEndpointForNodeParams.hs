{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRJointFabricDatastoreClusterAddBindingToEndpointForNodeParams@.
module ObjC.Matter.MTRJointFabricDatastoreClusterAddBindingToEndpointForNodeParams
  ( MTRJointFabricDatastoreClusterAddBindingToEndpointForNodeParams
  , IsMTRJointFabricDatastoreClusterAddBindingToEndpointForNodeParams(..)
  , nodeID
  , setNodeID
  , endpointID
  , setEndpointID
  , binding
  , setBinding
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , bindingSelector
  , endpointIDSelector
  , nodeIDSelector
  , serverSideProcessingTimeoutSelector
  , setBindingSelector
  , setEndpointIDSelector
  , setNodeIDSelector
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

-- | @- nodeID@
nodeID :: IsMTRJointFabricDatastoreClusterAddBindingToEndpointForNodeParams mtrJointFabricDatastoreClusterAddBindingToEndpointForNodeParams => mtrJointFabricDatastoreClusterAddBindingToEndpointForNodeParams -> IO (Id NSNumber)
nodeID mtrJointFabricDatastoreClusterAddBindingToEndpointForNodeParams =
  sendMessage mtrJointFabricDatastoreClusterAddBindingToEndpointForNodeParams nodeIDSelector

-- | @- setNodeID:@
setNodeID :: (IsMTRJointFabricDatastoreClusterAddBindingToEndpointForNodeParams mtrJointFabricDatastoreClusterAddBindingToEndpointForNodeParams, IsNSNumber value) => mtrJointFabricDatastoreClusterAddBindingToEndpointForNodeParams -> value -> IO ()
setNodeID mtrJointFabricDatastoreClusterAddBindingToEndpointForNodeParams value =
  sendMessage mtrJointFabricDatastoreClusterAddBindingToEndpointForNodeParams setNodeIDSelector (toNSNumber value)

-- | @- endpointID@
endpointID :: IsMTRJointFabricDatastoreClusterAddBindingToEndpointForNodeParams mtrJointFabricDatastoreClusterAddBindingToEndpointForNodeParams => mtrJointFabricDatastoreClusterAddBindingToEndpointForNodeParams -> IO (Id NSNumber)
endpointID mtrJointFabricDatastoreClusterAddBindingToEndpointForNodeParams =
  sendMessage mtrJointFabricDatastoreClusterAddBindingToEndpointForNodeParams endpointIDSelector

-- | @- setEndpointID:@
setEndpointID :: (IsMTRJointFabricDatastoreClusterAddBindingToEndpointForNodeParams mtrJointFabricDatastoreClusterAddBindingToEndpointForNodeParams, IsNSNumber value) => mtrJointFabricDatastoreClusterAddBindingToEndpointForNodeParams -> value -> IO ()
setEndpointID mtrJointFabricDatastoreClusterAddBindingToEndpointForNodeParams value =
  sendMessage mtrJointFabricDatastoreClusterAddBindingToEndpointForNodeParams setEndpointIDSelector (toNSNumber value)

-- | @- binding@
binding :: IsMTRJointFabricDatastoreClusterAddBindingToEndpointForNodeParams mtrJointFabricDatastoreClusterAddBindingToEndpointForNodeParams => mtrJointFabricDatastoreClusterAddBindingToEndpointForNodeParams -> IO (Id MTRJointFabricDatastoreClusterDatastoreBindingTargetStruct)
binding mtrJointFabricDatastoreClusterAddBindingToEndpointForNodeParams =
  sendMessage mtrJointFabricDatastoreClusterAddBindingToEndpointForNodeParams bindingSelector

-- | @- setBinding:@
setBinding :: (IsMTRJointFabricDatastoreClusterAddBindingToEndpointForNodeParams mtrJointFabricDatastoreClusterAddBindingToEndpointForNodeParams, IsMTRJointFabricDatastoreClusterDatastoreBindingTargetStruct value) => mtrJointFabricDatastoreClusterAddBindingToEndpointForNodeParams -> value -> IO ()
setBinding mtrJointFabricDatastoreClusterAddBindingToEndpointForNodeParams value =
  sendMessage mtrJointFabricDatastoreClusterAddBindingToEndpointForNodeParams setBindingSelector (toMTRJointFabricDatastoreClusterDatastoreBindingTargetStruct value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRJointFabricDatastoreClusterAddBindingToEndpointForNodeParams mtrJointFabricDatastoreClusterAddBindingToEndpointForNodeParams => mtrJointFabricDatastoreClusterAddBindingToEndpointForNodeParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrJointFabricDatastoreClusterAddBindingToEndpointForNodeParams =
  sendMessage mtrJointFabricDatastoreClusterAddBindingToEndpointForNodeParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRJointFabricDatastoreClusterAddBindingToEndpointForNodeParams mtrJointFabricDatastoreClusterAddBindingToEndpointForNodeParams, IsNSNumber value) => mtrJointFabricDatastoreClusterAddBindingToEndpointForNodeParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrJointFabricDatastoreClusterAddBindingToEndpointForNodeParams value =
  sendMessage mtrJointFabricDatastoreClusterAddBindingToEndpointForNodeParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRJointFabricDatastoreClusterAddBindingToEndpointForNodeParams mtrJointFabricDatastoreClusterAddBindingToEndpointForNodeParams => mtrJointFabricDatastoreClusterAddBindingToEndpointForNodeParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrJointFabricDatastoreClusterAddBindingToEndpointForNodeParams =
  sendMessage mtrJointFabricDatastoreClusterAddBindingToEndpointForNodeParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRJointFabricDatastoreClusterAddBindingToEndpointForNodeParams mtrJointFabricDatastoreClusterAddBindingToEndpointForNodeParams, IsNSNumber value) => mtrJointFabricDatastoreClusterAddBindingToEndpointForNodeParams -> value -> IO ()
setServerSideProcessingTimeout mtrJointFabricDatastoreClusterAddBindingToEndpointForNodeParams value =
  sendMessage mtrJointFabricDatastoreClusterAddBindingToEndpointForNodeParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeID@
nodeIDSelector :: Selector '[] (Id NSNumber)
nodeIDSelector = mkSelector "nodeID"

-- | @Selector@ for @setNodeID:@
setNodeIDSelector :: Selector '[Id NSNumber] ()
setNodeIDSelector = mkSelector "setNodeID:"

-- | @Selector@ for @endpointID@
endpointIDSelector :: Selector '[] (Id NSNumber)
endpointIDSelector = mkSelector "endpointID"

-- | @Selector@ for @setEndpointID:@
setEndpointIDSelector :: Selector '[Id NSNumber] ()
setEndpointIDSelector = mkSelector "setEndpointID:"

-- | @Selector@ for @binding@
bindingSelector :: Selector '[] (Id MTRJointFabricDatastoreClusterDatastoreBindingTargetStruct)
bindingSelector = mkSelector "binding"

-- | @Selector@ for @setBinding:@
setBindingSelector :: Selector '[Id MTRJointFabricDatastoreClusterDatastoreBindingTargetStruct] ()
setBindingSelector = mkSelector "setBinding:"

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

