{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRJointFabricDatastoreClusterRemoveAdminParams@.
module ObjC.Matter.MTRJointFabricDatastoreClusterRemoveAdminParams
  ( MTRJointFabricDatastoreClusterRemoveAdminParams
  , IsMTRJointFabricDatastoreClusterRemoveAdminParams(..)
  , nodeID
  , setNodeID
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , nodeIDSelector
  , serverSideProcessingTimeoutSelector
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
nodeID :: IsMTRJointFabricDatastoreClusterRemoveAdminParams mtrJointFabricDatastoreClusterRemoveAdminParams => mtrJointFabricDatastoreClusterRemoveAdminParams -> IO (Id NSNumber)
nodeID mtrJointFabricDatastoreClusterRemoveAdminParams =
  sendMessage mtrJointFabricDatastoreClusterRemoveAdminParams nodeIDSelector

-- | @- setNodeID:@
setNodeID :: (IsMTRJointFabricDatastoreClusterRemoveAdminParams mtrJointFabricDatastoreClusterRemoveAdminParams, IsNSNumber value) => mtrJointFabricDatastoreClusterRemoveAdminParams -> value -> IO ()
setNodeID mtrJointFabricDatastoreClusterRemoveAdminParams value =
  sendMessage mtrJointFabricDatastoreClusterRemoveAdminParams setNodeIDSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRJointFabricDatastoreClusterRemoveAdminParams mtrJointFabricDatastoreClusterRemoveAdminParams => mtrJointFabricDatastoreClusterRemoveAdminParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrJointFabricDatastoreClusterRemoveAdminParams =
  sendMessage mtrJointFabricDatastoreClusterRemoveAdminParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRJointFabricDatastoreClusterRemoveAdminParams mtrJointFabricDatastoreClusterRemoveAdminParams, IsNSNumber value) => mtrJointFabricDatastoreClusterRemoveAdminParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrJointFabricDatastoreClusterRemoveAdminParams value =
  sendMessage mtrJointFabricDatastoreClusterRemoveAdminParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRJointFabricDatastoreClusterRemoveAdminParams mtrJointFabricDatastoreClusterRemoveAdminParams => mtrJointFabricDatastoreClusterRemoveAdminParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrJointFabricDatastoreClusterRemoveAdminParams =
  sendMessage mtrJointFabricDatastoreClusterRemoveAdminParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRJointFabricDatastoreClusterRemoveAdminParams mtrJointFabricDatastoreClusterRemoveAdminParams, IsNSNumber value) => mtrJointFabricDatastoreClusterRemoveAdminParams -> value -> IO ()
setServerSideProcessingTimeout mtrJointFabricDatastoreClusterRemoveAdminParams value =
  sendMessage mtrJointFabricDatastoreClusterRemoveAdminParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

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

