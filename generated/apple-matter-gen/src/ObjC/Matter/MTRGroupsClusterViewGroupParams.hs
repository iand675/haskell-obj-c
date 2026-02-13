{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRGroupsClusterViewGroupParams@.
module ObjC.Matter.MTRGroupsClusterViewGroupParams
  ( MTRGroupsClusterViewGroupParams
  , IsMTRGroupsClusterViewGroupParams(..)
  , groupID
  , setGroupID
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , groupId
  , setGroupId
  , groupIDSelector
  , groupIdSelector
  , serverSideProcessingTimeoutSelector
  , setGroupIDSelector
  , setGroupIdSelector
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
groupID :: IsMTRGroupsClusterViewGroupParams mtrGroupsClusterViewGroupParams => mtrGroupsClusterViewGroupParams -> IO (Id NSNumber)
groupID mtrGroupsClusterViewGroupParams =
  sendMessage mtrGroupsClusterViewGroupParams groupIDSelector

-- | @- setGroupID:@
setGroupID :: (IsMTRGroupsClusterViewGroupParams mtrGroupsClusterViewGroupParams, IsNSNumber value) => mtrGroupsClusterViewGroupParams -> value -> IO ()
setGroupID mtrGroupsClusterViewGroupParams value =
  sendMessage mtrGroupsClusterViewGroupParams setGroupIDSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRGroupsClusterViewGroupParams mtrGroupsClusterViewGroupParams => mtrGroupsClusterViewGroupParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrGroupsClusterViewGroupParams =
  sendMessage mtrGroupsClusterViewGroupParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRGroupsClusterViewGroupParams mtrGroupsClusterViewGroupParams, IsNSNumber value) => mtrGroupsClusterViewGroupParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrGroupsClusterViewGroupParams value =
  sendMessage mtrGroupsClusterViewGroupParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRGroupsClusterViewGroupParams mtrGroupsClusterViewGroupParams => mtrGroupsClusterViewGroupParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrGroupsClusterViewGroupParams =
  sendMessage mtrGroupsClusterViewGroupParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRGroupsClusterViewGroupParams mtrGroupsClusterViewGroupParams, IsNSNumber value) => mtrGroupsClusterViewGroupParams -> value -> IO ()
setServerSideProcessingTimeout mtrGroupsClusterViewGroupParams value =
  sendMessage mtrGroupsClusterViewGroupParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- | @- groupId@
groupId :: IsMTRGroupsClusterViewGroupParams mtrGroupsClusterViewGroupParams => mtrGroupsClusterViewGroupParams -> IO (Id NSNumber)
groupId mtrGroupsClusterViewGroupParams =
  sendMessage mtrGroupsClusterViewGroupParams groupIdSelector

-- | @- setGroupId:@
setGroupId :: (IsMTRGroupsClusterViewGroupParams mtrGroupsClusterViewGroupParams, IsNSNumber value) => mtrGroupsClusterViewGroupParams -> value -> IO ()
setGroupId mtrGroupsClusterViewGroupParams value =
  sendMessage mtrGroupsClusterViewGroupParams setGroupIdSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

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

-- | @Selector@ for @serverSideProcessingTimeout@
serverSideProcessingTimeoutSelector :: Selector '[] (Id NSNumber)
serverSideProcessingTimeoutSelector = mkSelector "serverSideProcessingTimeout"

-- | @Selector@ for @setServerSideProcessingTimeout:@
setServerSideProcessingTimeoutSelector :: Selector '[Id NSNumber] ()
setServerSideProcessingTimeoutSelector = mkSelector "setServerSideProcessingTimeout:"

-- | @Selector@ for @groupId@
groupIdSelector :: Selector '[] (Id NSNumber)
groupIdSelector = mkSelector "groupId"

-- | @Selector@ for @setGroupId:@
setGroupIdSelector :: Selector '[Id NSNumber] ()
setGroupIdSelector = mkSelector "setGroupId:"

