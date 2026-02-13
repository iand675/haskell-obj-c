{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRGroupcastClusterLeaveGroupParams@.
module ObjC.Matter.MTRGroupcastClusterLeaveGroupParams
  ( MTRGroupcastClusterLeaveGroupParams
  , IsMTRGroupcastClusterLeaveGroupParams(..)
  , groupID
  , setGroupID
  , endpoints
  , setEndpoints
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , endpointsSelector
  , groupIDSelector
  , serverSideProcessingTimeoutSelector
  , setEndpointsSelector
  , setGroupIDSelector
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
groupID :: IsMTRGroupcastClusterLeaveGroupParams mtrGroupcastClusterLeaveGroupParams => mtrGroupcastClusterLeaveGroupParams -> IO (Id NSNumber)
groupID mtrGroupcastClusterLeaveGroupParams =
  sendMessage mtrGroupcastClusterLeaveGroupParams groupIDSelector

-- | @- setGroupID:@
setGroupID :: (IsMTRGroupcastClusterLeaveGroupParams mtrGroupcastClusterLeaveGroupParams, IsNSNumber value) => mtrGroupcastClusterLeaveGroupParams -> value -> IO ()
setGroupID mtrGroupcastClusterLeaveGroupParams value =
  sendMessage mtrGroupcastClusterLeaveGroupParams setGroupIDSelector (toNSNumber value)

-- | @- endpoints@
endpoints :: IsMTRGroupcastClusterLeaveGroupParams mtrGroupcastClusterLeaveGroupParams => mtrGroupcastClusterLeaveGroupParams -> IO (Id NSArray)
endpoints mtrGroupcastClusterLeaveGroupParams =
  sendMessage mtrGroupcastClusterLeaveGroupParams endpointsSelector

-- | @- setEndpoints:@
setEndpoints :: (IsMTRGroupcastClusterLeaveGroupParams mtrGroupcastClusterLeaveGroupParams, IsNSArray value) => mtrGroupcastClusterLeaveGroupParams -> value -> IO ()
setEndpoints mtrGroupcastClusterLeaveGroupParams value =
  sendMessage mtrGroupcastClusterLeaveGroupParams setEndpointsSelector (toNSArray value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRGroupcastClusterLeaveGroupParams mtrGroupcastClusterLeaveGroupParams => mtrGroupcastClusterLeaveGroupParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrGroupcastClusterLeaveGroupParams =
  sendMessage mtrGroupcastClusterLeaveGroupParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRGroupcastClusterLeaveGroupParams mtrGroupcastClusterLeaveGroupParams, IsNSNumber value) => mtrGroupcastClusterLeaveGroupParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrGroupcastClusterLeaveGroupParams value =
  sendMessage mtrGroupcastClusterLeaveGroupParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRGroupcastClusterLeaveGroupParams mtrGroupcastClusterLeaveGroupParams => mtrGroupcastClusterLeaveGroupParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrGroupcastClusterLeaveGroupParams =
  sendMessage mtrGroupcastClusterLeaveGroupParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRGroupcastClusterLeaveGroupParams mtrGroupcastClusterLeaveGroupParams, IsNSNumber value) => mtrGroupcastClusterLeaveGroupParams -> value -> IO ()
setServerSideProcessingTimeout mtrGroupcastClusterLeaveGroupParams value =
  sendMessage mtrGroupcastClusterLeaveGroupParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @groupID@
groupIDSelector :: Selector '[] (Id NSNumber)
groupIDSelector = mkSelector "groupID"

-- | @Selector@ for @setGroupID:@
setGroupIDSelector :: Selector '[Id NSNumber] ()
setGroupIDSelector = mkSelector "setGroupID:"

-- | @Selector@ for @endpoints@
endpointsSelector :: Selector '[] (Id NSArray)
endpointsSelector = mkSelector "endpoints"

-- | @Selector@ for @setEndpoints:@
setEndpointsSelector :: Selector '[Id NSArray] ()
setEndpointsSelector = mkSelector "setEndpoints:"

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

