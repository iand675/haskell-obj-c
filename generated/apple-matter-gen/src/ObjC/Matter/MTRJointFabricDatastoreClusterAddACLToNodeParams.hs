{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRJointFabricDatastoreClusterAddACLToNodeParams@.
module ObjC.Matter.MTRJointFabricDatastoreClusterAddACLToNodeParams
  ( MTRJointFabricDatastoreClusterAddACLToNodeParams
  , IsMTRJointFabricDatastoreClusterAddACLToNodeParams(..)
  , nodeID
  , setNodeID
  , aclEntry
  , setAclEntry
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , aclEntrySelector
  , nodeIDSelector
  , serverSideProcessingTimeoutSelector
  , setAclEntrySelector
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
nodeID :: IsMTRJointFabricDatastoreClusterAddACLToNodeParams mtrJointFabricDatastoreClusterAddACLToNodeParams => mtrJointFabricDatastoreClusterAddACLToNodeParams -> IO (Id NSNumber)
nodeID mtrJointFabricDatastoreClusterAddACLToNodeParams =
  sendMessage mtrJointFabricDatastoreClusterAddACLToNodeParams nodeIDSelector

-- | @- setNodeID:@
setNodeID :: (IsMTRJointFabricDatastoreClusterAddACLToNodeParams mtrJointFabricDatastoreClusterAddACLToNodeParams, IsNSNumber value) => mtrJointFabricDatastoreClusterAddACLToNodeParams -> value -> IO ()
setNodeID mtrJointFabricDatastoreClusterAddACLToNodeParams value =
  sendMessage mtrJointFabricDatastoreClusterAddACLToNodeParams setNodeIDSelector (toNSNumber value)

-- | @- aclEntry@
aclEntry :: IsMTRJointFabricDatastoreClusterAddACLToNodeParams mtrJointFabricDatastoreClusterAddACLToNodeParams => mtrJointFabricDatastoreClusterAddACLToNodeParams -> IO (Id MTRJointFabricDatastoreClusterDatastoreAccessControlEntryStruct)
aclEntry mtrJointFabricDatastoreClusterAddACLToNodeParams =
  sendMessage mtrJointFabricDatastoreClusterAddACLToNodeParams aclEntrySelector

-- | @- setAclEntry:@
setAclEntry :: (IsMTRJointFabricDatastoreClusterAddACLToNodeParams mtrJointFabricDatastoreClusterAddACLToNodeParams, IsMTRJointFabricDatastoreClusterDatastoreAccessControlEntryStruct value) => mtrJointFabricDatastoreClusterAddACLToNodeParams -> value -> IO ()
setAclEntry mtrJointFabricDatastoreClusterAddACLToNodeParams value =
  sendMessage mtrJointFabricDatastoreClusterAddACLToNodeParams setAclEntrySelector (toMTRJointFabricDatastoreClusterDatastoreAccessControlEntryStruct value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRJointFabricDatastoreClusterAddACLToNodeParams mtrJointFabricDatastoreClusterAddACLToNodeParams => mtrJointFabricDatastoreClusterAddACLToNodeParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrJointFabricDatastoreClusterAddACLToNodeParams =
  sendMessage mtrJointFabricDatastoreClusterAddACLToNodeParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRJointFabricDatastoreClusterAddACLToNodeParams mtrJointFabricDatastoreClusterAddACLToNodeParams, IsNSNumber value) => mtrJointFabricDatastoreClusterAddACLToNodeParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrJointFabricDatastoreClusterAddACLToNodeParams value =
  sendMessage mtrJointFabricDatastoreClusterAddACLToNodeParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRJointFabricDatastoreClusterAddACLToNodeParams mtrJointFabricDatastoreClusterAddACLToNodeParams => mtrJointFabricDatastoreClusterAddACLToNodeParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrJointFabricDatastoreClusterAddACLToNodeParams =
  sendMessage mtrJointFabricDatastoreClusterAddACLToNodeParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRJointFabricDatastoreClusterAddACLToNodeParams mtrJointFabricDatastoreClusterAddACLToNodeParams, IsNSNumber value) => mtrJointFabricDatastoreClusterAddACLToNodeParams -> value -> IO ()
setServerSideProcessingTimeout mtrJointFabricDatastoreClusterAddACLToNodeParams value =
  sendMessage mtrJointFabricDatastoreClusterAddACLToNodeParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeID@
nodeIDSelector :: Selector '[] (Id NSNumber)
nodeIDSelector = mkSelector "nodeID"

-- | @Selector@ for @setNodeID:@
setNodeIDSelector :: Selector '[Id NSNumber] ()
setNodeIDSelector = mkSelector "setNodeID:"

-- | @Selector@ for @aclEntry@
aclEntrySelector :: Selector '[] (Id MTRJointFabricDatastoreClusterDatastoreAccessControlEntryStruct)
aclEntrySelector = mkSelector "aclEntry"

-- | @Selector@ for @setAclEntry:@
setAclEntrySelector :: Selector '[Id MTRJointFabricDatastoreClusterDatastoreAccessControlEntryStruct] ()
setAclEntrySelector = mkSelector "setAclEntry:"

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

