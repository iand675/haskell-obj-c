{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRJointFabricDatastoreClusterRemoveACLFromNodeParams@.
module ObjC.Matter.MTRJointFabricDatastoreClusterRemoveACLFromNodeParams
  ( MTRJointFabricDatastoreClusterRemoveACLFromNodeParams
  , IsMTRJointFabricDatastoreClusterRemoveACLFromNodeParams(..)
  , listID
  , setListID
  , nodeID
  , setNodeID
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , listIDSelector
  , nodeIDSelector
  , serverSideProcessingTimeoutSelector
  , setListIDSelector
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

-- | @- listID@
listID :: IsMTRJointFabricDatastoreClusterRemoveACLFromNodeParams mtrJointFabricDatastoreClusterRemoveACLFromNodeParams => mtrJointFabricDatastoreClusterRemoveACLFromNodeParams -> IO (Id NSNumber)
listID mtrJointFabricDatastoreClusterRemoveACLFromNodeParams =
  sendMessage mtrJointFabricDatastoreClusterRemoveACLFromNodeParams listIDSelector

-- | @- setListID:@
setListID :: (IsMTRJointFabricDatastoreClusterRemoveACLFromNodeParams mtrJointFabricDatastoreClusterRemoveACLFromNodeParams, IsNSNumber value) => mtrJointFabricDatastoreClusterRemoveACLFromNodeParams -> value -> IO ()
setListID mtrJointFabricDatastoreClusterRemoveACLFromNodeParams value =
  sendMessage mtrJointFabricDatastoreClusterRemoveACLFromNodeParams setListIDSelector (toNSNumber value)

-- | @- nodeID@
nodeID :: IsMTRJointFabricDatastoreClusterRemoveACLFromNodeParams mtrJointFabricDatastoreClusterRemoveACLFromNodeParams => mtrJointFabricDatastoreClusterRemoveACLFromNodeParams -> IO (Id NSNumber)
nodeID mtrJointFabricDatastoreClusterRemoveACLFromNodeParams =
  sendMessage mtrJointFabricDatastoreClusterRemoveACLFromNodeParams nodeIDSelector

-- | @- setNodeID:@
setNodeID :: (IsMTRJointFabricDatastoreClusterRemoveACLFromNodeParams mtrJointFabricDatastoreClusterRemoveACLFromNodeParams, IsNSNumber value) => mtrJointFabricDatastoreClusterRemoveACLFromNodeParams -> value -> IO ()
setNodeID mtrJointFabricDatastoreClusterRemoveACLFromNodeParams value =
  sendMessage mtrJointFabricDatastoreClusterRemoveACLFromNodeParams setNodeIDSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRJointFabricDatastoreClusterRemoveACLFromNodeParams mtrJointFabricDatastoreClusterRemoveACLFromNodeParams => mtrJointFabricDatastoreClusterRemoveACLFromNodeParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrJointFabricDatastoreClusterRemoveACLFromNodeParams =
  sendMessage mtrJointFabricDatastoreClusterRemoveACLFromNodeParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRJointFabricDatastoreClusterRemoveACLFromNodeParams mtrJointFabricDatastoreClusterRemoveACLFromNodeParams, IsNSNumber value) => mtrJointFabricDatastoreClusterRemoveACLFromNodeParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrJointFabricDatastoreClusterRemoveACLFromNodeParams value =
  sendMessage mtrJointFabricDatastoreClusterRemoveACLFromNodeParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRJointFabricDatastoreClusterRemoveACLFromNodeParams mtrJointFabricDatastoreClusterRemoveACLFromNodeParams => mtrJointFabricDatastoreClusterRemoveACLFromNodeParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrJointFabricDatastoreClusterRemoveACLFromNodeParams =
  sendMessage mtrJointFabricDatastoreClusterRemoveACLFromNodeParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRJointFabricDatastoreClusterRemoveACLFromNodeParams mtrJointFabricDatastoreClusterRemoveACLFromNodeParams, IsNSNumber value) => mtrJointFabricDatastoreClusterRemoveACLFromNodeParams -> value -> IO ()
setServerSideProcessingTimeout mtrJointFabricDatastoreClusterRemoveACLFromNodeParams value =
  sendMessage mtrJointFabricDatastoreClusterRemoveACLFromNodeParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @listID@
listIDSelector :: Selector '[] (Id NSNumber)
listIDSelector = mkSelector "listID"

-- | @Selector@ for @setListID:@
setListIDSelector :: Selector '[Id NSNumber] ()
setListIDSelector = mkSelector "setListID:"

-- | @Selector@ for @nodeID@
nodeIDSelector :: Selector '[] (Id NSNumber)
nodeIDSelector = mkSelector "nodeID"

-- | @Selector@ for @setNodeID:@
setNodeIDSelector :: Selector '[Id NSNumber] ()
setNodeIDSelector = mkSelector "setNodeID:"

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

