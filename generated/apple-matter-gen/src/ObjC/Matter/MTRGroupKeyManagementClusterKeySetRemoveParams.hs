{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRGroupKeyManagementClusterKeySetRemoveParams@.
module ObjC.Matter.MTRGroupKeyManagementClusterKeySetRemoveParams
  ( MTRGroupKeyManagementClusterKeySetRemoveParams
  , IsMTRGroupKeyManagementClusterKeySetRemoveParams(..)
  , groupKeySetID
  , setGroupKeySetID
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , groupKeySetIDSelector
  , serverSideProcessingTimeoutSelector
  , setGroupKeySetIDSelector
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

-- | @- groupKeySetID@
groupKeySetID :: IsMTRGroupKeyManagementClusterKeySetRemoveParams mtrGroupKeyManagementClusterKeySetRemoveParams => mtrGroupKeyManagementClusterKeySetRemoveParams -> IO (Id NSNumber)
groupKeySetID mtrGroupKeyManagementClusterKeySetRemoveParams =
  sendMessage mtrGroupKeyManagementClusterKeySetRemoveParams groupKeySetIDSelector

-- | @- setGroupKeySetID:@
setGroupKeySetID :: (IsMTRGroupKeyManagementClusterKeySetRemoveParams mtrGroupKeyManagementClusterKeySetRemoveParams, IsNSNumber value) => mtrGroupKeyManagementClusterKeySetRemoveParams -> value -> IO ()
setGroupKeySetID mtrGroupKeyManagementClusterKeySetRemoveParams value =
  sendMessage mtrGroupKeyManagementClusterKeySetRemoveParams setGroupKeySetIDSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRGroupKeyManagementClusterKeySetRemoveParams mtrGroupKeyManagementClusterKeySetRemoveParams => mtrGroupKeyManagementClusterKeySetRemoveParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrGroupKeyManagementClusterKeySetRemoveParams =
  sendMessage mtrGroupKeyManagementClusterKeySetRemoveParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRGroupKeyManagementClusterKeySetRemoveParams mtrGroupKeyManagementClusterKeySetRemoveParams, IsNSNumber value) => mtrGroupKeyManagementClusterKeySetRemoveParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrGroupKeyManagementClusterKeySetRemoveParams value =
  sendMessage mtrGroupKeyManagementClusterKeySetRemoveParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRGroupKeyManagementClusterKeySetRemoveParams mtrGroupKeyManagementClusterKeySetRemoveParams => mtrGroupKeyManagementClusterKeySetRemoveParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrGroupKeyManagementClusterKeySetRemoveParams =
  sendMessage mtrGroupKeyManagementClusterKeySetRemoveParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRGroupKeyManagementClusterKeySetRemoveParams mtrGroupKeyManagementClusterKeySetRemoveParams, IsNSNumber value) => mtrGroupKeyManagementClusterKeySetRemoveParams -> value -> IO ()
setServerSideProcessingTimeout mtrGroupKeyManagementClusterKeySetRemoveParams value =
  sendMessage mtrGroupKeyManagementClusterKeySetRemoveParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @groupKeySetID@
groupKeySetIDSelector :: Selector '[] (Id NSNumber)
groupKeySetIDSelector = mkSelector "groupKeySetID"

-- | @Selector@ for @setGroupKeySetID:@
setGroupKeySetIDSelector :: Selector '[Id NSNumber] ()
setGroupKeySetIDSelector = mkSelector "setGroupKeySetID:"

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

