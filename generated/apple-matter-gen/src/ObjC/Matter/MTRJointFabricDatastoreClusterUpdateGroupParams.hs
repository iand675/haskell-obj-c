{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRJointFabricDatastoreClusterUpdateGroupParams@.
module ObjC.Matter.MTRJointFabricDatastoreClusterUpdateGroupParams
  ( MTRJointFabricDatastoreClusterUpdateGroupParams
  , IsMTRJointFabricDatastoreClusterUpdateGroupParams(..)
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
groupID :: IsMTRJointFabricDatastoreClusterUpdateGroupParams mtrJointFabricDatastoreClusterUpdateGroupParams => mtrJointFabricDatastoreClusterUpdateGroupParams -> IO (Id NSNumber)
groupID mtrJointFabricDatastoreClusterUpdateGroupParams =
  sendMessage mtrJointFabricDatastoreClusterUpdateGroupParams groupIDSelector

-- | @- setGroupID:@
setGroupID :: (IsMTRJointFabricDatastoreClusterUpdateGroupParams mtrJointFabricDatastoreClusterUpdateGroupParams, IsNSNumber value) => mtrJointFabricDatastoreClusterUpdateGroupParams -> value -> IO ()
setGroupID mtrJointFabricDatastoreClusterUpdateGroupParams value =
  sendMessage mtrJointFabricDatastoreClusterUpdateGroupParams setGroupIDSelector (toNSNumber value)

-- | @- friendlyName@
friendlyName :: IsMTRJointFabricDatastoreClusterUpdateGroupParams mtrJointFabricDatastoreClusterUpdateGroupParams => mtrJointFabricDatastoreClusterUpdateGroupParams -> IO (Id NSString)
friendlyName mtrJointFabricDatastoreClusterUpdateGroupParams =
  sendMessage mtrJointFabricDatastoreClusterUpdateGroupParams friendlyNameSelector

-- | @- setFriendlyName:@
setFriendlyName :: (IsMTRJointFabricDatastoreClusterUpdateGroupParams mtrJointFabricDatastoreClusterUpdateGroupParams, IsNSString value) => mtrJointFabricDatastoreClusterUpdateGroupParams -> value -> IO ()
setFriendlyName mtrJointFabricDatastoreClusterUpdateGroupParams value =
  sendMessage mtrJointFabricDatastoreClusterUpdateGroupParams setFriendlyNameSelector (toNSString value)

-- | @- groupKeySetID@
groupKeySetID :: IsMTRJointFabricDatastoreClusterUpdateGroupParams mtrJointFabricDatastoreClusterUpdateGroupParams => mtrJointFabricDatastoreClusterUpdateGroupParams -> IO (Id NSNumber)
groupKeySetID mtrJointFabricDatastoreClusterUpdateGroupParams =
  sendMessage mtrJointFabricDatastoreClusterUpdateGroupParams groupKeySetIDSelector

-- | @- setGroupKeySetID:@
setGroupKeySetID :: (IsMTRJointFabricDatastoreClusterUpdateGroupParams mtrJointFabricDatastoreClusterUpdateGroupParams, IsNSNumber value) => mtrJointFabricDatastoreClusterUpdateGroupParams -> value -> IO ()
setGroupKeySetID mtrJointFabricDatastoreClusterUpdateGroupParams value =
  sendMessage mtrJointFabricDatastoreClusterUpdateGroupParams setGroupKeySetIDSelector (toNSNumber value)

-- | @- groupCAT@
groupCAT :: IsMTRJointFabricDatastoreClusterUpdateGroupParams mtrJointFabricDatastoreClusterUpdateGroupParams => mtrJointFabricDatastoreClusterUpdateGroupParams -> IO (Id NSNumber)
groupCAT mtrJointFabricDatastoreClusterUpdateGroupParams =
  sendMessage mtrJointFabricDatastoreClusterUpdateGroupParams groupCATSelector

-- | @- setGroupCAT:@
setGroupCAT :: (IsMTRJointFabricDatastoreClusterUpdateGroupParams mtrJointFabricDatastoreClusterUpdateGroupParams, IsNSNumber value) => mtrJointFabricDatastoreClusterUpdateGroupParams -> value -> IO ()
setGroupCAT mtrJointFabricDatastoreClusterUpdateGroupParams value =
  sendMessage mtrJointFabricDatastoreClusterUpdateGroupParams setGroupCATSelector (toNSNumber value)

-- | @- groupCATVersion@
groupCATVersion :: IsMTRJointFabricDatastoreClusterUpdateGroupParams mtrJointFabricDatastoreClusterUpdateGroupParams => mtrJointFabricDatastoreClusterUpdateGroupParams -> IO (Id NSNumber)
groupCATVersion mtrJointFabricDatastoreClusterUpdateGroupParams =
  sendMessage mtrJointFabricDatastoreClusterUpdateGroupParams groupCATVersionSelector

-- | @- setGroupCATVersion:@
setGroupCATVersion :: (IsMTRJointFabricDatastoreClusterUpdateGroupParams mtrJointFabricDatastoreClusterUpdateGroupParams, IsNSNumber value) => mtrJointFabricDatastoreClusterUpdateGroupParams -> value -> IO ()
setGroupCATVersion mtrJointFabricDatastoreClusterUpdateGroupParams value =
  sendMessage mtrJointFabricDatastoreClusterUpdateGroupParams setGroupCATVersionSelector (toNSNumber value)

-- | @- groupPermission@
groupPermission :: IsMTRJointFabricDatastoreClusterUpdateGroupParams mtrJointFabricDatastoreClusterUpdateGroupParams => mtrJointFabricDatastoreClusterUpdateGroupParams -> IO (Id NSNumber)
groupPermission mtrJointFabricDatastoreClusterUpdateGroupParams =
  sendMessage mtrJointFabricDatastoreClusterUpdateGroupParams groupPermissionSelector

-- | @- setGroupPermission:@
setGroupPermission :: (IsMTRJointFabricDatastoreClusterUpdateGroupParams mtrJointFabricDatastoreClusterUpdateGroupParams, IsNSNumber value) => mtrJointFabricDatastoreClusterUpdateGroupParams -> value -> IO ()
setGroupPermission mtrJointFabricDatastoreClusterUpdateGroupParams value =
  sendMessage mtrJointFabricDatastoreClusterUpdateGroupParams setGroupPermissionSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRJointFabricDatastoreClusterUpdateGroupParams mtrJointFabricDatastoreClusterUpdateGroupParams => mtrJointFabricDatastoreClusterUpdateGroupParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrJointFabricDatastoreClusterUpdateGroupParams =
  sendMessage mtrJointFabricDatastoreClusterUpdateGroupParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRJointFabricDatastoreClusterUpdateGroupParams mtrJointFabricDatastoreClusterUpdateGroupParams, IsNSNumber value) => mtrJointFabricDatastoreClusterUpdateGroupParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrJointFabricDatastoreClusterUpdateGroupParams value =
  sendMessage mtrJointFabricDatastoreClusterUpdateGroupParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRJointFabricDatastoreClusterUpdateGroupParams mtrJointFabricDatastoreClusterUpdateGroupParams => mtrJointFabricDatastoreClusterUpdateGroupParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrJointFabricDatastoreClusterUpdateGroupParams =
  sendMessage mtrJointFabricDatastoreClusterUpdateGroupParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRJointFabricDatastoreClusterUpdateGroupParams mtrJointFabricDatastoreClusterUpdateGroupParams, IsNSNumber value) => mtrJointFabricDatastoreClusterUpdateGroupParams -> value -> IO ()
setServerSideProcessingTimeout mtrJointFabricDatastoreClusterUpdateGroupParams value =
  sendMessage mtrJointFabricDatastoreClusterUpdateGroupParams setServerSideProcessingTimeoutSelector (toNSNumber value)

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

