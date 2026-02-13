{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRJointFabricDatastoreClusterUpdateEndpointForNodeParams@.
module ObjC.Matter.MTRJointFabricDatastoreClusterUpdateEndpointForNodeParams
  ( MTRJointFabricDatastoreClusterUpdateEndpointForNodeParams
  , IsMTRJointFabricDatastoreClusterUpdateEndpointForNodeParams(..)
  , endpointID
  , setEndpointID
  , nodeID
  , setNodeID
  , friendlyName
  , setFriendlyName
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , endpointIDSelector
  , friendlyNameSelector
  , nodeIDSelector
  , serverSideProcessingTimeoutSelector
  , setEndpointIDSelector
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

-- | @- endpointID@
endpointID :: IsMTRJointFabricDatastoreClusterUpdateEndpointForNodeParams mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams => mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams -> IO (Id NSNumber)
endpointID mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams =
  sendMessage mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams endpointIDSelector

-- | @- setEndpointID:@
setEndpointID :: (IsMTRJointFabricDatastoreClusterUpdateEndpointForNodeParams mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams, IsNSNumber value) => mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams -> value -> IO ()
setEndpointID mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams value =
  sendMessage mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams setEndpointIDSelector (toNSNumber value)

-- | @- nodeID@
nodeID :: IsMTRJointFabricDatastoreClusterUpdateEndpointForNodeParams mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams => mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams -> IO (Id NSNumber)
nodeID mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams =
  sendMessage mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams nodeIDSelector

-- | @- setNodeID:@
setNodeID :: (IsMTRJointFabricDatastoreClusterUpdateEndpointForNodeParams mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams, IsNSNumber value) => mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams -> value -> IO ()
setNodeID mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams value =
  sendMessage mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams setNodeIDSelector (toNSNumber value)

-- | @- friendlyName@
friendlyName :: IsMTRJointFabricDatastoreClusterUpdateEndpointForNodeParams mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams => mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams -> IO (Id NSString)
friendlyName mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams =
  sendMessage mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams friendlyNameSelector

-- | @- setFriendlyName:@
setFriendlyName :: (IsMTRJointFabricDatastoreClusterUpdateEndpointForNodeParams mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams, IsNSString value) => mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams -> value -> IO ()
setFriendlyName mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams value =
  sendMessage mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams setFriendlyNameSelector (toNSString value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRJointFabricDatastoreClusterUpdateEndpointForNodeParams mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams => mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams =
  sendMessage mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRJointFabricDatastoreClusterUpdateEndpointForNodeParams mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams, IsNSNumber value) => mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams value =
  sendMessage mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRJointFabricDatastoreClusterUpdateEndpointForNodeParams mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams => mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams =
  sendMessage mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRJointFabricDatastoreClusterUpdateEndpointForNodeParams mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams, IsNSNumber value) => mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams -> value -> IO ()
setServerSideProcessingTimeout mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams value =
  sendMessage mtrJointFabricDatastoreClusterUpdateEndpointForNodeParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @endpointID@
endpointIDSelector :: Selector '[] (Id NSNumber)
endpointIDSelector = mkSelector "endpointID"

-- | @Selector@ for @setEndpointID:@
setEndpointIDSelector :: Selector '[Id NSNumber] ()
setEndpointIDSelector = mkSelector "setEndpointID:"

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

