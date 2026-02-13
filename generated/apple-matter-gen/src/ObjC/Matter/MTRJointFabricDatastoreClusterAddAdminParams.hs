{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRJointFabricDatastoreClusterAddAdminParams@.
module ObjC.Matter.MTRJointFabricDatastoreClusterAddAdminParams
  ( MTRJointFabricDatastoreClusterAddAdminParams
  , IsMTRJointFabricDatastoreClusterAddAdminParams(..)
  , nodeID
  , setNodeID
  , friendlyName
  , setFriendlyName
  , vendorID
  , setVendorID
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
  , setVendorIDSelector
  , timedInvokeTimeoutMsSelector
  , vendorIDSelector


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
nodeID :: IsMTRJointFabricDatastoreClusterAddAdminParams mtrJointFabricDatastoreClusterAddAdminParams => mtrJointFabricDatastoreClusterAddAdminParams -> IO (Id NSNumber)
nodeID mtrJointFabricDatastoreClusterAddAdminParams =
  sendMessage mtrJointFabricDatastoreClusterAddAdminParams nodeIDSelector

-- | @- setNodeID:@
setNodeID :: (IsMTRJointFabricDatastoreClusterAddAdminParams mtrJointFabricDatastoreClusterAddAdminParams, IsNSNumber value) => mtrJointFabricDatastoreClusterAddAdminParams -> value -> IO ()
setNodeID mtrJointFabricDatastoreClusterAddAdminParams value =
  sendMessage mtrJointFabricDatastoreClusterAddAdminParams setNodeIDSelector (toNSNumber value)

-- | @- friendlyName@
friendlyName :: IsMTRJointFabricDatastoreClusterAddAdminParams mtrJointFabricDatastoreClusterAddAdminParams => mtrJointFabricDatastoreClusterAddAdminParams -> IO (Id NSString)
friendlyName mtrJointFabricDatastoreClusterAddAdminParams =
  sendMessage mtrJointFabricDatastoreClusterAddAdminParams friendlyNameSelector

-- | @- setFriendlyName:@
setFriendlyName :: (IsMTRJointFabricDatastoreClusterAddAdminParams mtrJointFabricDatastoreClusterAddAdminParams, IsNSString value) => mtrJointFabricDatastoreClusterAddAdminParams -> value -> IO ()
setFriendlyName mtrJointFabricDatastoreClusterAddAdminParams value =
  sendMessage mtrJointFabricDatastoreClusterAddAdminParams setFriendlyNameSelector (toNSString value)

-- | @- vendorID@
vendorID :: IsMTRJointFabricDatastoreClusterAddAdminParams mtrJointFabricDatastoreClusterAddAdminParams => mtrJointFabricDatastoreClusterAddAdminParams -> IO (Id NSNumber)
vendorID mtrJointFabricDatastoreClusterAddAdminParams =
  sendMessage mtrJointFabricDatastoreClusterAddAdminParams vendorIDSelector

-- | @- setVendorID:@
setVendorID :: (IsMTRJointFabricDatastoreClusterAddAdminParams mtrJointFabricDatastoreClusterAddAdminParams, IsNSNumber value) => mtrJointFabricDatastoreClusterAddAdminParams -> value -> IO ()
setVendorID mtrJointFabricDatastoreClusterAddAdminParams value =
  sendMessage mtrJointFabricDatastoreClusterAddAdminParams setVendorIDSelector (toNSNumber value)

-- | @- icac@
icac :: IsMTRJointFabricDatastoreClusterAddAdminParams mtrJointFabricDatastoreClusterAddAdminParams => mtrJointFabricDatastoreClusterAddAdminParams -> IO (Id NSData)
icac mtrJointFabricDatastoreClusterAddAdminParams =
  sendMessage mtrJointFabricDatastoreClusterAddAdminParams icacSelector

-- | @- setIcac:@
setIcac :: (IsMTRJointFabricDatastoreClusterAddAdminParams mtrJointFabricDatastoreClusterAddAdminParams, IsNSData value) => mtrJointFabricDatastoreClusterAddAdminParams -> value -> IO ()
setIcac mtrJointFabricDatastoreClusterAddAdminParams value =
  sendMessage mtrJointFabricDatastoreClusterAddAdminParams setIcacSelector (toNSData value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRJointFabricDatastoreClusterAddAdminParams mtrJointFabricDatastoreClusterAddAdminParams => mtrJointFabricDatastoreClusterAddAdminParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrJointFabricDatastoreClusterAddAdminParams =
  sendMessage mtrJointFabricDatastoreClusterAddAdminParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRJointFabricDatastoreClusterAddAdminParams mtrJointFabricDatastoreClusterAddAdminParams, IsNSNumber value) => mtrJointFabricDatastoreClusterAddAdminParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrJointFabricDatastoreClusterAddAdminParams value =
  sendMessage mtrJointFabricDatastoreClusterAddAdminParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRJointFabricDatastoreClusterAddAdminParams mtrJointFabricDatastoreClusterAddAdminParams => mtrJointFabricDatastoreClusterAddAdminParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrJointFabricDatastoreClusterAddAdminParams =
  sendMessage mtrJointFabricDatastoreClusterAddAdminParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRJointFabricDatastoreClusterAddAdminParams mtrJointFabricDatastoreClusterAddAdminParams, IsNSNumber value) => mtrJointFabricDatastoreClusterAddAdminParams -> value -> IO ()
setServerSideProcessingTimeout mtrJointFabricDatastoreClusterAddAdminParams value =
  sendMessage mtrJointFabricDatastoreClusterAddAdminParams setServerSideProcessingTimeoutSelector (toNSNumber value)

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

-- | @Selector@ for @vendorID@
vendorIDSelector :: Selector '[] (Id NSNumber)
vendorIDSelector = mkSelector "vendorID"

-- | @Selector@ for @setVendorID:@
setVendorIDSelector :: Selector '[Id NSNumber] ()
setVendorIDSelector = mkSelector "setVendorID:"

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

