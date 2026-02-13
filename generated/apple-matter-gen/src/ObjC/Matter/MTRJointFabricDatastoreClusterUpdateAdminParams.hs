{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRJointFabricDatastoreClusterUpdateAdminParams@.
module ObjC.Matter.MTRJointFabricDatastoreClusterUpdateAdminParams
  ( MTRJointFabricDatastoreClusterUpdateAdminParams
  , IsMTRJointFabricDatastoreClusterUpdateAdminParams(..)
  , nodeID
  , setNodeID
  , friendlyName
  , setFriendlyName
  , icac
  , setIcac
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , friendlyNameSelector
  , icacSelector
  , nodeIDSelector
  , serverSideProcessingTimeoutSelector
  , setFriendlyNameSelector
  , setIcacSelector
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
nodeID :: IsMTRJointFabricDatastoreClusterUpdateAdminParams mtrJointFabricDatastoreClusterUpdateAdminParams => mtrJointFabricDatastoreClusterUpdateAdminParams -> IO (Id NSNumber)
nodeID mtrJointFabricDatastoreClusterUpdateAdminParams =
  sendMessage mtrJointFabricDatastoreClusterUpdateAdminParams nodeIDSelector

-- | @- setNodeID:@
setNodeID :: (IsMTRJointFabricDatastoreClusterUpdateAdminParams mtrJointFabricDatastoreClusterUpdateAdminParams, IsNSNumber value) => mtrJointFabricDatastoreClusterUpdateAdminParams -> value -> IO ()
setNodeID mtrJointFabricDatastoreClusterUpdateAdminParams value =
  sendMessage mtrJointFabricDatastoreClusterUpdateAdminParams setNodeIDSelector (toNSNumber value)

-- | @- friendlyName@
friendlyName :: IsMTRJointFabricDatastoreClusterUpdateAdminParams mtrJointFabricDatastoreClusterUpdateAdminParams => mtrJointFabricDatastoreClusterUpdateAdminParams -> IO (Id NSString)
friendlyName mtrJointFabricDatastoreClusterUpdateAdminParams =
  sendMessage mtrJointFabricDatastoreClusterUpdateAdminParams friendlyNameSelector

-- | @- setFriendlyName:@
setFriendlyName :: (IsMTRJointFabricDatastoreClusterUpdateAdminParams mtrJointFabricDatastoreClusterUpdateAdminParams, IsNSString value) => mtrJointFabricDatastoreClusterUpdateAdminParams -> value -> IO ()
setFriendlyName mtrJointFabricDatastoreClusterUpdateAdminParams value =
  sendMessage mtrJointFabricDatastoreClusterUpdateAdminParams setFriendlyNameSelector (toNSString value)

-- | @- icac@
icac :: IsMTRJointFabricDatastoreClusterUpdateAdminParams mtrJointFabricDatastoreClusterUpdateAdminParams => mtrJointFabricDatastoreClusterUpdateAdminParams -> IO (Id NSData)
icac mtrJointFabricDatastoreClusterUpdateAdminParams =
  sendMessage mtrJointFabricDatastoreClusterUpdateAdminParams icacSelector

-- | @- setIcac:@
setIcac :: (IsMTRJointFabricDatastoreClusterUpdateAdminParams mtrJointFabricDatastoreClusterUpdateAdminParams, IsNSData value) => mtrJointFabricDatastoreClusterUpdateAdminParams -> value -> IO ()
setIcac mtrJointFabricDatastoreClusterUpdateAdminParams value =
  sendMessage mtrJointFabricDatastoreClusterUpdateAdminParams setIcacSelector (toNSData value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRJointFabricDatastoreClusterUpdateAdminParams mtrJointFabricDatastoreClusterUpdateAdminParams => mtrJointFabricDatastoreClusterUpdateAdminParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrJointFabricDatastoreClusterUpdateAdminParams =
  sendMessage mtrJointFabricDatastoreClusterUpdateAdminParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRJointFabricDatastoreClusterUpdateAdminParams mtrJointFabricDatastoreClusterUpdateAdminParams, IsNSNumber value) => mtrJointFabricDatastoreClusterUpdateAdminParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrJointFabricDatastoreClusterUpdateAdminParams value =
  sendMessage mtrJointFabricDatastoreClusterUpdateAdminParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRJointFabricDatastoreClusterUpdateAdminParams mtrJointFabricDatastoreClusterUpdateAdminParams => mtrJointFabricDatastoreClusterUpdateAdminParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrJointFabricDatastoreClusterUpdateAdminParams =
  sendMessage mtrJointFabricDatastoreClusterUpdateAdminParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRJointFabricDatastoreClusterUpdateAdminParams mtrJointFabricDatastoreClusterUpdateAdminParams, IsNSNumber value) => mtrJointFabricDatastoreClusterUpdateAdminParams -> value -> IO ()
setServerSideProcessingTimeout mtrJointFabricDatastoreClusterUpdateAdminParams value =
  sendMessage mtrJointFabricDatastoreClusterUpdateAdminParams setServerSideProcessingTimeoutSelector (toNSNumber value)

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

-- | @Selector@ for @icac@
icacSelector :: Selector '[] (Id NSData)
icacSelector = mkSelector "icac"

-- | @Selector@ for @setIcac:@
setIcacSelector :: Selector '[Id NSData] ()
setIcacSelector = mkSelector "setIcac:"

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

