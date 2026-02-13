{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRJointFabricDatastoreClusterUpdateNodeParams@.
module ObjC.Matter.MTRJointFabricDatastoreClusterUpdateNodeParams
  ( MTRJointFabricDatastoreClusterUpdateNodeParams
  , IsMTRJointFabricDatastoreClusterUpdateNodeParams(..)
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
nodeID :: IsMTRJointFabricDatastoreClusterUpdateNodeParams mtrJointFabricDatastoreClusterUpdateNodeParams => mtrJointFabricDatastoreClusterUpdateNodeParams -> IO (Id NSNumber)
nodeID mtrJointFabricDatastoreClusterUpdateNodeParams =
  sendMessage mtrJointFabricDatastoreClusterUpdateNodeParams nodeIDSelector

-- | @- setNodeID:@
setNodeID :: (IsMTRJointFabricDatastoreClusterUpdateNodeParams mtrJointFabricDatastoreClusterUpdateNodeParams, IsNSNumber value) => mtrJointFabricDatastoreClusterUpdateNodeParams -> value -> IO ()
setNodeID mtrJointFabricDatastoreClusterUpdateNodeParams value =
  sendMessage mtrJointFabricDatastoreClusterUpdateNodeParams setNodeIDSelector (toNSNumber value)

-- | @- friendlyName@
friendlyName :: IsMTRJointFabricDatastoreClusterUpdateNodeParams mtrJointFabricDatastoreClusterUpdateNodeParams => mtrJointFabricDatastoreClusterUpdateNodeParams -> IO (Id NSString)
friendlyName mtrJointFabricDatastoreClusterUpdateNodeParams =
  sendMessage mtrJointFabricDatastoreClusterUpdateNodeParams friendlyNameSelector

-- | @- setFriendlyName:@
setFriendlyName :: (IsMTRJointFabricDatastoreClusterUpdateNodeParams mtrJointFabricDatastoreClusterUpdateNodeParams, IsNSString value) => mtrJointFabricDatastoreClusterUpdateNodeParams -> value -> IO ()
setFriendlyName mtrJointFabricDatastoreClusterUpdateNodeParams value =
  sendMessage mtrJointFabricDatastoreClusterUpdateNodeParams setFriendlyNameSelector (toNSString value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRJointFabricDatastoreClusterUpdateNodeParams mtrJointFabricDatastoreClusterUpdateNodeParams => mtrJointFabricDatastoreClusterUpdateNodeParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrJointFabricDatastoreClusterUpdateNodeParams =
  sendMessage mtrJointFabricDatastoreClusterUpdateNodeParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRJointFabricDatastoreClusterUpdateNodeParams mtrJointFabricDatastoreClusterUpdateNodeParams, IsNSNumber value) => mtrJointFabricDatastoreClusterUpdateNodeParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrJointFabricDatastoreClusterUpdateNodeParams value =
  sendMessage mtrJointFabricDatastoreClusterUpdateNodeParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRJointFabricDatastoreClusterUpdateNodeParams mtrJointFabricDatastoreClusterUpdateNodeParams => mtrJointFabricDatastoreClusterUpdateNodeParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrJointFabricDatastoreClusterUpdateNodeParams =
  sendMessage mtrJointFabricDatastoreClusterUpdateNodeParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRJointFabricDatastoreClusterUpdateNodeParams mtrJointFabricDatastoreClusterUpdateNodeParams, IsNSNumber value) => mtrJointFabricDatastoreClusterUpdateNodeParams -> value -> IO ()
setServerSideProcessingTimeout mtrJointFabricDatastoreClusterUpdateNodeParams value =
  sendMessage mtrJointFabricDatastoreClusterUpdateNodeParams setServerSideProcessingTimeoutSelector (toNSNumber value)

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

