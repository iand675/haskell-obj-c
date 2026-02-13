{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams@.
module ObjC.Matter.MTRJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams
  ( MTRJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams
  , IsMTRJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams(..)
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
nodeID :: IsMTRJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams mtrJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams => mtrJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams -> IO (Id NSNumber)
nodeID mtrJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams =
  sendMessage mtrJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams nodeIDSelector

-- | @- setNodeID:@
setNodeID :: (IsMTRJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams mtrJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams, IsNSNumber value) => mtrJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams -> value -> IO ()
setNodeID mtrJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams value =
  sendMessage mtrJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams setNodeIDSelector (toNSNumber value)

-- | @- endpointID@
endpointID :: IsMTRJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams mtrJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams => mtrJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams -> IO (Id NSNumber)
endpointID mtrJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams =
  sendMessage mtrJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams endpointIDSelector

-- | @- setEndpointID:@
setEndpointID :: (IsMTRJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams mtrJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams, IsNSNumber value) => mtrJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams -> value -> IO ()
setEndpointID mtrJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams value =
  sendMessage mtrJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams setEndpointIDSelector (toNSNumber value)

-- | @- groupID@
groupID :: IsMTRJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams mtrJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams => mtrJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams -> IO (Id NSNumber)
groupID mtrJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams =
  sendMessage mtrJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams groupIDSelector

-- | @- setGroupID:@
setGroupID :: (IsMTRJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams mtrJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams, IsNSNumber value) => mtrJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams -> value -> IO ()
setGroupID mtrJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams value =
  sendMessage mtrJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams setGroupIDSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams mtrJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams => mtrJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams =
  sendMessage mtrJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams mtrJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams, IsNSNumber value) => mtrJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams value =
  sendMessage mtrJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams mtrJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams => mtrJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams =
  sendMessage mtrJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams mtrJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams, IsNSNumber value) => mtrJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams -> value -> IO ()
setServerSideProcessingTimeout mtrJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams value =
  sendMessage mtrJointFabricDatastoreClusterRemoveGroupIDFromEndpointForNodeParams setServerSideProcessingTimeoutSelector (toNSNumber value)

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

