{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRJointFabricDatastoreClusterAddGroupParams@.
module ObjC.Matter.MTRJointFabricDatastoreClusterAddGroupParams
  ( MTRJointFabricDatastoreClusterAddGroupParams
  , IsMTRJointFabricDatastoreClusterAddGroupParams(..)
  , groupID
  , setGroupID
  , friendlyName
  , setFriendlyName
  , groupKeySetID
  , setGroupKeySetID
  , groupCAT
  , setGroupCAT
  , groupCATVersion
  , setGroupCATVersion
  , groupPermission
  , setGroupPermission
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , friendlyNameSelector
  , groupCATSelector
  , groupCATVersionSelector
  , groupIDSelector
  , groupKeySetIDSelector
  , groupPermissionSelector
  , serverSideProcessingTimeoutSelector
  , setFriendlyNameSelector
  , setGroupCATSelector
  , setGroupCATVersionSelector
  , setGroupIDSelector
  , setGroupKeySetIDSelector
  , setGroupPermissionSelector
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

-- | @- groupID@
groupID :: IsMTRJointFabricDatastoreClusterAddGroupParams mtrJointFabricDatastoreClusterAddGroupParams => mtrJointFabricDatastoreClusterAddGroupParams -> IO (Id NSNumber)
groupID mtrJointFabricDatastoreClusterAddGroupParams =
  sendMessage mtrJointFabricDatastoreClusterAddGroupParams groupIDSelector

-- | @- setGroupID:@
setGroupID :: (IsMTRJointFabricDatastoreClusterAddGroupParams mtrJointFabricDatastoreClusterAddGroupParams, IsNSNumber value) => mtrJointFabricDatastoreClusterAddGroupParams -> value -> IO ()
setGroupID mtrJointFabricDatastoreClusterAddGroupParams value =
  sendMessage mtrJointFabricDatastoreClusterAddGroupParams setGroupIDSelector (toNSNumber value)

-- | @- friendlyName@
friendlyName :: IsMTRJointFabricDatastoreClusterAddGroupParams mtrJointFabricDatastoreClusterAddGroupParams => mtrJointFabricDatastoreClusterAddGroupParams -> IO (Id NSString)
friendlyName mtrJointFabricDatastoreClusterAddGroupParams =
  sendMessage mtrJointFabricDatastoreClusterAddGroupParams friendlyNameSelector

-- | @- setFriendlyName:@
setFriendlyName :: (IsMTRJointFabricDatastoreClusterAddGroupParams mtrJointFabricDatastoreClusterAddGroupParams, IsNSString value) => mtrJointFabricDatastoreClusterAddGroupParams -> value -> IO ()
setFriendlyName mtrJointFabricDatastoreClusterAddGroupParams value =
  sendMessage mtrJointFabricDatastoreClusterAddGroupParams setFriendlyNameSelector (toNSString value)

-- | @- groupKeySetID@
groupKeySetID :: IsMTRJointFabricDatastoreClusterAddGroupParams mtrJointFabricDatastoreClusterAddGroupParams => mtrJointFabricDatastoreClusterAddGroupParams -> IO (Id NSNumber)
groupKeySetID mtrJointFabricDatastoreClusterAddGroupParams =
  sendMessage mtrJointFabricDatastoreClusterAddGroupParams groupKeySetIDSelector

-- | @- setGroupKeySetID:@
setGroupKeySetID :: (IsMTRJointFabricDatastoreClusterAddGroupParams mtrJointFabricDatastoreClusterAddGroupParams, IsNSNumber value) => mtrJointFabricDatastoreClusterAddGroupParams -> value -> IO ()
setGroupKeySetID mtrJointFabricDatastoreClusterAddGroupParams value =
  sendMessage mtrJointFabricDatastoreClusterAddGroupParams setGroupKeySetIDSelector (toNSNumber value)

-- | @- groupCAT@
groupCAT :: IsMTRJointFabricDatastoreClusterAddGroupParams mtrJointFabricDatastoreClusterAddGroupParams => mtrJointFabricDatastoreClusterAddGroupParams -> IO (Id NSNumber)
groupCAT mtrJointFabricDatastoreClusterAddGroupParams =
  sendMessage mtrJointFabricDatastoreClusterAddGroupParams groupCATSelector

-- | @- setGroupCAT:@
setGroupCAT :: (IsMTRJointFabricDatastoreClusterAddGroupParams mtrJointFabricDatastoreClusterAddGroupParams, IsNSNumber value) => mtrJointFabricDatastoreClusterAddGroupParams -> value -> IO ()
setGroupCAT mtrJointFabricDatastoreClusterAddGroupParams value =
  sendMessage mtrJointFabricDatastoreClusterAddGroupParams setGroupCATSelector (toNSNumber value)

-- | @- groupCATVersion@
groupCATVersion :: IsMTRJointFabricDatastoreClusterAddGroupParams mtrJointFabricDatastoreClusterAddGroupParams => mtrJointFabricDatastoreClusterAddGroupParams -> IO (Id NSNumber)
groupCATVersion mtrJointFabricDatastoreClusterAddGroupParams =
  sendMessage mtrJointFabricDatastoreClusterAddGroupParams groupCATVersionSelector

-- | @- setGroupCATVersion:@
setGroupCATVersion :: (IsMTRJointFabricDatastoreClusterAddGroupParams mtrJointFabricDatastoreClusterAddGroupParams, IsNSNumber value) => mtrJointFabricDatastoreClusterAddGroupParams -> value -> IO ()
setGroupCATVersion mtrJointFabricDatastoreClusterAddGroupParams value =
  sendMessage mtrJointFabricDatastoreClusterAddGroupParams setGroupCATVersionSelector (toNSNumber value)

-- | @- groupPermission@
groupPermission :: IsMTRJointFabricDatastoreClusterAddGroupParams mtrJointFabricDatastoreClusterAddGroupParams => mtrJointFabricDatastoreClusterAddGroupParams -> IO (Id NSNumber)
groupPermission mtrJointFabricDatastoreClusterAddGroupParams =
  sendMessage mtrJointFabricDatastoreClusterAddGroupParams groupPermissionSelector

-- | @- setGroupPermission:@
setGroupPermission :: (IsMTRJointFabricDatastoreClusterAddGroupParams mtrJointFabricDatastoreClusterAddGroupParams, IsNSNumber value) => mtrJointFabricDatastoreClusterAddGroupParams -> value -> IO ()
setGroupPermission mtrJointFabricDatastoreClusterAddGroupParams value =
  sendMessage mtrJointFabricDatastoreClusterAddGroupParams setGroupPermissionSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRJointFabricDatastoreClusterAddGroupParams mtrJointFabricDatastoreClusterAddGroupParams => mtrJointFabricDatastoreClusterAddGroupParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrJointFabricDatastoreClusterAddGroupParams =
  sendMessage mtrJointFabricDatastoreClusterAddGroupParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRJointFabricDatastoreClusterAddGroupParams mtrJointFabricDatastoreClusterAddGroupParams, IsNSNumber value) => mtrJointFabricDatastoreClusterAddGroupParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrJointFabricDatastoreClusterAddGroupParams value =
  sendMessage mtrJointFabricDatastoreClusterAddGroupParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRJointFabricDatastoreClusterAddGroupParams mtrJointFabricDatastoreClusterAddGroupParams => mtrJointFabricDatastoreClusterAddGroupParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrJointFabricDatastoreClusterAddGroupParams =
  sendMessage mtrJointFabricDatastoreClusterAddGroupParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRJointFabricDatastoreClusterAddGroupParams mtrJointFabricDatastoreClusterAddGroupParams, IsNSNumber value) => mtrJointFabricDatastoreClusterAddGroupParams -> value -> IO ()
setServerSideProcessingTimeout mtrJointFabricDatastoreClusterAddGroupParams value =
  sendMessage mtrJointFabricDatastoreClusterAddGroupParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @groupID@
groupIDSelector :: Selector '[] (Id NSNumber)
groupIDSelector = mkSelector "groupID"

-- | @Selector@ for @setGroupID:@
setGroupIDSelector :: Selector '[Id NSNumber] ()
setGroupIDSelector = mkSelector "setGroupID:"

-- | @Selector@ for @friendlyName@
friendlyNameSelector :: Selector '[] (Id NSString)
friendlyNameSelector = mkSelector "friendlyName"

-- | @Selector@ for @setFriendlyName:@
setFriendlyNameSelector :: Selector '[Id NSString] ()
setFriendlyNameSelector = mkSelector "setFriendlyName:"

-- | @Selector@ for @groupKeySetID@
groupKeySetIDSelector :: Selector '[] (Id NSNumber)
groupKeySetIDSelector = mkSelector "groupKeySetID"

-- | @Selector@ for @setGroupKeySetID:@
setGroupKeySetIDSelector :: Selector '[Id NSNumber] ()
setGroupKeySetIDSelector = mkSelector "setGroupKeySetID:"

-- | @Selector@ for @groupCAT@
groupCATSelector :: Selector '[] (Id NSNumber)
groupCATSelector = mkSelector "groupCAT"

-- | @Selector@ for @setGroupCAT:@
setGroupCATSelector :: Selector '[Id NSNumber] ()
setGroupCATSelector = mkSelector "setGroupCAT:"

-- | @Selector@ for @groupCATVersion@
groupCATVersionSelector :: Selector '[] (Id NSNumber)
groupCATVersionSelector = mkSelector "groupCATVersion"

-- | @Selector@ for @setGroupCATVersion:@
setGroupCATVersionSelector :: Selector '[Id NSNumber] ()
setGroupCATVersionSelector = mkSelector "setGroupCATVersion:"

-- | @Selector@ for @groupPermission@
groupPermissionSelector :: Selector '[] (Id NSNumber)
groupPermissionSelector = mkSelector "groupPermission"

-- | @Selector@ for @setGroupPermission:@
setGroupPermissionSelector :: Selector '[Id NSNumber] ()
setGroupPermissionSelector = mkSelector "setGroupPermission:"

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

