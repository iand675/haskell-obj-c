{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams@.
module ObjC.Matter.MTRJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams
  ( MTRJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams
  , IsMTRJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams(..)
  , nodeID
  , setNodeID
  , endpointID
  , setEndpointID
  , groupID
  , setGroupID
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , endpointIDSelector
  , groupIDSelector
  , nodeIDSelector
  , serverSideProcessingTimeoutSelector
  , setEndpointIDSelector
  , setGroupIDSelector
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
nodeID :: IsMTRJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams => mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams -> IO (Id NSNumber)
nodeID mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams =
  sendMessage mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams nodeIDSelector

-- | @- setNodeID:@
setNodeID :: (IsMTRJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams, IsNSNumber value) => mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams -> value -> IO ()
setNodeID mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams value =
  sendMessage mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams setNodeIDSelector (toNSNumber value)

-- | @- endpointID@
endpointID :: IsMTRJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams => mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams -> IO (Id NSNumber)
endpointID mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams =
  sendMessage mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams endpointIDSelector

-- | @- setEndpointID:@
setEndpointID :: (IsMTRJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams, IsNSNumber value) => mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams -> value -> IO ()
setEndpointID mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams value =
  sendMessage mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams setEndpointIDSelector (toNSNumber value)

-- | @- groupID@
groupID :: IsMTRJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams => mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams -> IO (Id NSNumber)
groupID mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams =
  sendMessage mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams groupIDSelector

-- | @- setGroupID:@
setGroupID :: (IsMTRJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams, IsNSNumber value) => mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams -> value -> IO ()
setGroupID mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams value =
  sendMessage mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams setGroupIDSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams => mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams =
  sendMessage mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams, IsNSNumber value) => mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams value =
  sendMessage mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams => mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams =
  sendMessage mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams, IsNSNumber value) => mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams -> value -> IO ()
setServerSideProcessingTimeout mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams value =
  sendMessage mtrJointFabricDatastoreClusterAddGroupIDToEndpointForNodeParams setServerSideProcessingTimeoutSelector (toNSNumber value)

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

-- | @Selector@ for @groupID@
groupIDSelector :: Selector '[] (Id NSNumber)
groupIDSelector = mkSelector "groupID"

-- | @Selector@ for @setGroupID:@
setGroupIDSelector :: Selector '[Id NSNumber] ()
setGroupIDSelector = mkSelector "setGroupID:"

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

