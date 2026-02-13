{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRGroupsClusterViewGroupResponseParams@.
module ObjC.Matter.MTRGroupsClusterViewGroupResponseParams
  ( MTRGroupsClusterViewGroupResponseParams
  , IsMTRGroupsClusterViewGroupResponseParams(..)
  , initWithResponseValue_error
  , status
  , setStatus
  , groupID
  , setGroupID
  , groupName
  , setGroupName
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , groupId
  , setGroupId
  , groupIDSelector
  , groupIdSelector
  , groupNameSelector
  , initWithResponseValue_errorSelector
  , setGroupIDSelector
  , setGroupIdSelector
  , setGroupNameSelector
  , setStatusSelector
  , setTimedInvokeTimeoutMsSelector
  , statusSelector
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

-- | Initialize an MTRGroupsClusterViewGroupResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRGroupsClusterViewGroupResponseParams mtrGroupsClusterViewGroupResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrGroupsClusterViewGroupResponseParams -> responseValue -> error_ -> IO (Id MTRGroupsClusterViewGroupResponseParams)
initWithResponseValue_error mtrGroupsClusterViewGroupResponseParams responseValue error_ =
  sendOwnedMessage mtrGroupsClusterViewGroupResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- status@
status :: IsMTRGroupsClusterViewGroupResponseParams mtrGroupsClusterViewGroupResponseParams => mtrGroupsClusterViewGroupResponseParams -> IO (Id NSNumber)
status mtrGroupsClusterViewGroupResponseParams =
  sendMessage mtrGroupsClusterViewGroupResponseParams statusSelector

-- | @- setStatus:@
setStatus :: (IsMTRGroupsClusterViewGroupResponseParams mtrGroupsClusterViewGroupResponseParams, IsNSNumber value) => mtrGroupsClusterViewGroupResponseParams -> value -> IO ()
setStatus mtrGroupsClusterViewGroupResponseParams value =
  sendMessage mtrGroupsClusterViewGroupResponseParams setStatusSelector (toNSNumber value)

-- | @- groupID@
groupID :: IsMTRGroupsClusterViewGroupResponseParams mtrGroupsClusterViewGroupResponseParams => mtrGroupsClusterViewGroupResponseParams -> IO (Id NSNumber)
groupID mtrGroupsClusterViewGroupResponseParams =
  sendMessage mtrGroupsClusterViewGroupResponseParams groupIDSelector

-- | @- setGroupID:@
setGroupID :: (IsMTRGroupsClusterViewGroupResponseParams mtrGroupsClusterViewGroupResponseParams, IsNSNumber value) => mtrGroupsClusterViewGroupResponseParams -> value -> IO ()
setGroupID mtrGroupsClusterViewGroupResponseParams value =
  sendMessage mtrGroupsClusterViewGroupResponseParams setGroupIDSelector (toNSNumber value)

-- | @- groupName@
groupName :: IsMTRGroupsClusterViewGroupResponseParams mtrGroupsClusterViewGroupResponseParams => mtrGroupsClusterViewGroupResponseParams -> IO (Id NSString)
groupName mtrGroupsClusterViewGroupResponseParams =
  sendMessage mtrGroupsClusterViewGroupResponseParams groupNameSelector

-- | @- setGroupName:@
setGroupName :: (IsMTRGroupsClusterViewGroupResponseParams mtrGroupsClusterViewGroupResponseParams, IsNSString value) => mtrGroupsClusterViewGroupResponseParams -> value -> IO ()
setGroupName mtrGroupsClusterViewGroupResponseParams value =
  sendMessage mtrGroupsClusterViewGroupResponseParams setGroupNameSelector (toNSString value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRGroupsClusterViewGroupResponseParams mtrGroupsClusterViewGroupResponseParams => mtrGroupsClusterViewGroupResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrGroupsClusterViewGroupResponseParams =
  sendMessage mtrGroupsClusterViewGroupResponseParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRGroupsClusterViewGroupResponseParams mtrGroupsClusterViewGroupResponseParams, IsNSNumber value) => mtrGroupsClusterViewGroupResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrGroupsClusterViewGroupResponseParams value =
  sendMessage mtrGroupsClusterViewGroupResponseParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | @- groupId@
groupId :: IsMTRGroupsClusterViewGroupResponseParams mtrGroupsClusterViewGroupResponseParams => mtrGroupsClusterViewGroupResponseParams -> IO (Id NSNumber)
groupId mtrGroupsClusterViewGroupResponseParams =
  sendMessage mtrGroupsClusterViewGroupResponseParams groupIdSelector

-- | @- setGroupId:@
setGroupId :: (IsMTRGroupsClusterViewGroupResponseParams mtrGroupsClusterViewGroupResponseParams, IsNSNumber value) => mtrGroupsClusterViewGroupResponseParams -> value -> IO ()
setGroupId mtrGroupsClusterViewGroupResponseParams value =
  sendMessage mtrGroupsClusterViewGroupResponseParams setGroupIdSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRGroupsClusterViewGroupResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @status@
statusSelector :: Selector '[] (Id NSNumber)
statusSelector = mkSelector "status"

-- | @Selector@ for @setStatus:@
setStatusSelector :: Selector '[Id NSNumber] ()
setStatusSelector = mkSelector "setStatus:"

-- | @Selector@ for @groupID@
groupIDSelector :: Selector '[] (Id NSNumber)
groupIDSelector = mkSelector "groupID"

-- | @Selector@ for @setGroupID:@
setGroupIDSelector :: Selector '[Id NSNumber] ()
setGroupIDSelector = mkSelector "setGroupID:"

-- | @Selector@ for @groupName@
groupNameSelector :: Selector '[] (Id NSString)
groupNameSelector = mkSelector "groupName"

-- | @Selector@ for @setGroupName:@
setGroupNameSelector :: Selector '[Id NSString] ()
setGroupNameSelector = mkSelector "setGroupName:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector '[] (Id NSNumber)
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector '[Id NSNumber] ()
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

-- | @Selector@ for @groupId@
groupIdSelector :: Selector '[] (Id NSNumber)
groupIdSelector = mkSelector "groupId"

-- | @Selector@ for @setGroupId:@
setGroupIdSelector :: Selector '[Id NSNumber] ()
setGroupIdSelector = mkSelector "setGroupId:"

