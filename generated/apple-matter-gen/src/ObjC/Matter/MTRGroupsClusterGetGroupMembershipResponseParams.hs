{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRGroupsClusterGetGroupMembershipResponseParams@.
module ObjC.Matter.MTRGroupsClusterGetGroupMembershipResponseParams
  ( MTRGroupsClusterGetGroupMembershipResponseParams
  , IsMTRGroupsClusterGetGroupMembershipResponseParams(..)
  , initWithResponseValue_error
  , capacity
  , setCapacity
  , groupList
  , setGroupList
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , capacitySelector
  , groupListSelector
  , initWithResponseValue_errorSelector
  , setCapacitySelector
  , setGroupListSelector
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

-- | Initialize an MTRGroupsClusterGetGroupMembershipResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRGroupsClusterGetGroupMembershipResponseParams mtrGroupsClusterGetGroupMembershipResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrGroupsClusterGetGroupMembershipResponseParams -> responseValue -> error_ -> IO (Id MTRGroupsClusterGetGroupMembershipResponseParams)
initWithResponseValue_error mtrGroupsClusterGetGroupMembershipResponseParams responseValue error_ =
  sendOwnedMessage mtrGroupsClusterGetGroupMembershipResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- capacity@
capacity :: IsMTRGroupsClusterGetGroupMembershipResponseParams mtrGroupsClusterGetGroupMembershipResponseParams => mtrGroupsClusterGetGroupMembershipResponseParams -> IO (Id NSNumber)
capacity mtrGroupsClusterGetGroupMembershipResponseParams =
  sendMessage mtrGroupsClusterGetGroupMembershipResponseParams capacitySelector

-- | @- setCapacity:@
setCapacity :: (IsMTRGroupsClusterGetGroupMembershipResponseParams mtrGroupsClusterGetGroupMembershipResponseParams, IsNSNumber value) => mtrGroupsClusterGetGroupMembershipResponseParams -> value -> IO ()
setCapacity mtrGroupsClusterGetGroupMembershipResponseParams value =
  sendMessage mtrGroupsClusterGetGroupMembershipResponseParams setCapacitySelector (toNSNumber value)

-- | @- groupList@
groupList :: IsMTRGroupsClusterGetGroupMembershipResponseParams mtrGroupsClusterGetGroupMembershipResponseParams => mtrGroupsClusterGetGroupMembershipResponseParams -> IO (Id NSArray)
groupList mtrGroupsClusterGetGroupMembershipResponseParams =
  sendMessage mtrGroupsClusterGetGroupMembershipResponseParams groupListSelector

-- | @- setGroupList:@
setGroupList :: (IsMTRGroupsClusterGetGroupMembershipResponseParams mtrGroupsClusterGetGroupMembershipResponseParams, IsNSArray value) => mtrGroupsClusterGetGroupMembershipResponseParams -> value -> IO ()
setGroupList mtrGroupsClusterGetGroupMembershipResponseParams value =
  sendMessage mtrGroupsClusterGetGroupMembershipResponseParams setGroupListSelector (toNSArray value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRGroupsClusterGetGroupMembershipResponseParams mtrGroupsClusterGetGroupMembershipResponseParams => mtrGroupsClusterGetGroupMembershipResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrGroupsClusterGetGroupMembershipResponseParams =
  sendMessage mtrGroupsClusterGetGroupMembershipResponseParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRGroupsClusterGetGroupMembershipResponseParams mtrGroupsClusterGetGroupMembershipResponseParams, IsNSNumber value) => mtrGroupsClusterGetGroupMembershipResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrGroupsClusterGetGroupMembershipResponseParams value =
  sendMessage mtrGroupsClusterGetGroupMembershipResponseParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRGroupsClusterGetGroupMembershipResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @capacity@
capacitySelector :: Selector '[] (Id NSNumber)
capacitySelector = mkSelector "capacity"

-- | @Selector@ for @setCapacity:@
setCapacitySelector :: Selector '[Id NSNumber] ()
setCapacitySelector = mkSelector "setCapacity:"

-- | @Selector@ for @groupList@
groupListSelector :: Selector '[] (Id NSArray)
groupListSelector = mkSelector "groupList"

-- | @Selector@ for @setGroupList:@
setGroupListSelector :: Selector '[Id NSArray] ()
setGroupListSelector = mkSelector "setGroupList:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector '[] (Id NSNumber)
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector '[Id NSNumber] ()
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

