{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRJointFabricDatastoreClusterAddPendingNodeParams@.
module ObjC.Matter.MTRJointFabricDatastoreClusterAddPendingNodeParams
  ( MTRJointFabricDatastoreClusterAddPendingNodeParams
  , IsMTRJointFabricDatastoreClusterAddPendingNodeParams(..)
  , nodeID
  , setNodeID
  , friendlyName
  , setFriendlyName
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , friendlyNameSelector
  , nodeIDSelector
  , serverSideProcessingTimeoutSelector
  , setFriendlyNameSelector
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
nodeID :: IsMTRJointFabricDatastoreClusterAddPendingNodeParams mtrJointFabricDatastoreClusterAddPendingNodeParams => mtrJointFabricDatastoreClusterAddPendingNodeParams -> IO (Id NSNumber)
nodeID mtrJointFabricDatastoreClusterAddPendingNodeParams =
  sendMessage mtrJointFabricDatastoreClusterAddPendingNodeParams nodeIDSelector

-- | @- setNodeID:@
setNodeID :: (IsMTRJointFabricDatastoreClusterAddPendingNodeParams mtrJointFabricDatastoreClusterAddPendingNodeParams, IsNSNumber value) => mtrJointFabricDatastoreClusterAddPendingNodeParams -> value -> IO ()
setNodeID mtrJointFabricDatastoreClusterAddPendingNodeParams value =
  sendMessage mtrJointFabricDatastoreClusterAddPendingNodeParams setNodeIDSelector (toNSNumber value)

-- | @- friendlyName@
friendlyName :: IsMTRJointFabricDatastoreClusterAddPendingNodeParams mtrJointFabricDatastoreClusterAddPendingNodeParams => mtrJointFabricDatastoreClusterAddPendingNodeParams -> IO (Id NSString)
friendlyName mtrJointFabricDatastoreClusterAddPendingNodeParams =
  sendMessage mtrJointFabricDatastoreClusterAddPendingNodeParams friendlyNameSelector

-- | @- setFriendlyName:@
setFriendlyName :: (IsMTRJointFabricDatastoreClusterAddPendingNodeParams mtrJointFabricDatastoreClusterAddPendingNodeParams, IsNSString value) => mtrJointFabricDatastoreClusterAddPendingNodeParams -> value -> IO ()
setFriendlyName mtrJointFabricDatastoreClusterAddPendingNodeParams value =
  sendMessage mtrJointFabricDatastoreClusterAddPendingNodeParams setFriendlyNameSelector (toNSString value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRJointFabricDatastoreClusterAddPendingNodeParams mtrJointFabricDatastoreClusterAddPendingNodeParams => mtrJointFabricDatastoreClusterAddPendingNodeParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrJointFabricDatastoreClusterAddPendingNodeParams =
  sendMessage mtrJointFabricDatastoreClusterAddPendingNodeParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRJointFabricDatastoreClusterAddPendingNodeParams mtrJointFabricDatastoreClusterAddPendingNodeParams, IsNSNumber value) => mtrJointFabricDatastoreClusterAddPendingNodeParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrJointFabricDatastoreClusterAddPendingNodeParams value =
  sendMessage mtrJointFabricDatastoreClusterAddPendingNodeParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRJointFabricDatastoreClusterAddPendingNodeParams mtrJointFabricDatastoreClusterAddPendingNodeParams => mtrJointFabricDatastoreClusterAddPendingNodeParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrJointFabricDatastoreClusterAddPendingNodeParams =
  sendMessage mtrJointFabricDatastoreClusterAddPendingNodeParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRJointFabricDatastoreClusterAddPendingNodeParams mtrJointFabricDatastoreClusterAddPendingNodeParams, IsNSNumber value) => mtrJointFabricDatastoreClusterAddPendingNodeParams -> value -> IO ()
setServerSideProcessingTimeout mtrJointFabricDatastoreClusterAddPendingNodeParams value =
  sendMessage mtrJointFabricDatastoreClusterAddPendingNodeParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeID@
nodeIDSelector :: Selector '[] (Id NSNumber)
nodeIDSelector = mkSelector "nodeID"

-- | @Selector@ for @setNodeID:@
setNodeIDSelector :: Selector '[Id NSNumber] ()
setNodeIDSelector = mkSelector "setNodeID:"

-- | @Selector@ for @friendlyName@
friendlyNameSelector :: Selector '[] (Id NSString)
friendlyNameSelector = mkSelector "friendlyName"

-- | @Selector@ for @setFriendlyName:@
setFriendlyNameSelector :: Selector '[Id NSString] ()
setFriendlyNameSelector = mkSelector "setFriendlyName:"

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

