{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRGroupsClusterRemoveGroupResponseParams@.
module ObjC.Matter.MTRGroupsClusterRemoveGroupResponseParams
  ( MTRGroupsClusterRemoveGroupResponseParams
  , IsMTRGroupsClusterRemoveGroupResponseParams(..)
  , initWithResponseValue_error
  , status
  , setStatus
  , groupID
  , setGroupID
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , groupId
  , setGroupId
  , groupIDSelector
  , groupIdSelector
  , initWithResponseValue_errorSelector
  , setGroupIDSelector
  , setGroupIdSelector
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

-- | Initialize an MTRGroupsClusterRemoveGroupResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRGroupsClusterRemoveGroupResponseParams mtrGroupsClusterRemoveGroupResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrGroupsClusterRemoveGroupResponseParams -> responseValue -> error_ -> IO (Id MTRGroupsClusterRemoveGroupResponseParams)
initWithResponseValue_error mtrGroupsClusterRemoveGroupResponseParams responseValue error_ =
  sendOwnedMessage mtrGroupsClusterRemoveGroupResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- status@
status :: IsMTRGroupsClusterRemoveGroupResponseParams mtrGroupsClusterRemoveGroupResponseParams => mtrGroupsClusterRemoveGroupResponseParams -> IO (Id NSNumber)
status mtrGroupsClusterRemoveGroupResponseParams =
  sendMessage mtrGroupsClusterRemoveGroupResponseParams statusSelector

-- | @- setStatus:@
setStatus :: (IsMTRGroupsClusterRemoveGroupResponseParams mtrGroupsClusterRemoveGroupResponseParams, IsNSNumber value) => mtrGroupsClusterRemoveGroupResponseParams -> value -> IO ()
setStatus mtrGroupsClusterRemoveGroupResponseParams value =
  sendMessage mtrGroupsClusterRemoveGroupResponseParams setStatusSelector (toNSNumber value)

-- | @- groupID@
groupID :: IsMTRGroupsClusterRemoveGroupResponseParams mtrGroupsClusterRemoveGroupResponseParams => mtrGroupsClusterRemoveGroupResponseParams -> IO (Id NSNumber)
groupID mtrGroupsClusterRemoveGroupResponseParams =
  sendMessage mtrGroupsClusterRemoveGroupResponseParams groupIDSelector

-- | @- setGroupID:@
setGroupID :: (IsMTRGroupsClusterRemoveGroupResponseParams mtrGroupsClusterRemoveGroupResponseParams, IsNSNumber value) => mtrGroupsClusterRemoveGroupResponseParams -> value -> IO ()
setGroupID mtrGroupsClusterRemoveGroupResponseParams value =
  sendMessage mtrGroupsClusterRemoveGroupResponseParams setGroupIDSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRGroupsClusterRemoveGroupResponseParams mtrGroupsClusterRemoveGroupResponseParams => mtrGroupsClusterRemoveGroupResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrGroupsClusterRemoveGroupResponseParams =
  sendMessage mtrGroupsClusterRemoveGroupResponseParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRGroupsClusterRemoveGroupResponseParams mtrGroupsClusterRemoveGroupResponseParams, IsNSNumber value) => mtrGroupsClusterRemoveGroupResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrGroupsClusterRemoveGroupResponseParams value =
  sendMessage mtrGroupsClusterRemoveGroupResponseParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | @- groupId@
groupId :: IsMTRGroupsClusterRemoveGroupResponseParams mtrGroupsClusterRemoveGroupResponseParams => mtrGroupsClusterRemoveGroupResponseParams -> IO (Id NSNumber)
groupId mtrGroupsClusterRemoveGroupResponseParams =
  sendMessage mtrGroupsClusterRemoveGroupResponseParams groupIdSelector

-- | @- setGroupId:@
setGroupId :: (IsMTRGroupsClusterRemoveGroupResponseParams mtrGroupsClusterRemoveGroupResponseParams, IsNSNumber value) => mtrGroupsClusterRemoveGroupResponseParams -> value -> IO ()
setGroupId mtrGroupsClusterRemoveGroupResponseParams value =
  sendMessage mtrGroupsClusterRemoveGroupResponseParams setGroupIdSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRGroupsClusterRemoveGroupResponseParams)
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

