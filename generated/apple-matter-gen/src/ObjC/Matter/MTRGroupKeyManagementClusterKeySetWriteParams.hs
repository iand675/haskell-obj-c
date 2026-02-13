{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRGroupKeyManagementClusterKeySetWriteParams@.
module ObjC.Matter.MTRGroupKeyManagementClusterKeySetWriteParams
  ( MTRGroupKeyManagementClusterKeySetWriteParams
  , IsMTRGroupKeyManagementClusterKeySetWriteParams(..)
  , groupKeySet
  , setGroupKeySet
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , groupKeySetSelector
  , serverSideProcessingTimeoutSelector
  , setGroupKeySetSelector
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

-- | @- groupKeySet@
groupKeySet :: IsMTRGroupKeyManagementClusterKeySetWriteParams mtrGroupKeyManagementClusterKeySetWriteParams => mtrGroupKeyManagementClusterKeySetWriteParams -> IO (Id MTRGroupKeyManagementClusterGroupKeySetStruct)
groupKeySet mtrGroupKeyManagementClusterKeySetWriteParams =
  sendMessage mtrGroupKeyManagementClusterKeySetWriteParams groupKeySetSelector

-- | @- setGroupKeySet:@
setGroupKeySet :: (IsMTRGroupKeyManagementClusterKeySetWriteParams mtrGroupKeyManagementClusterKeySetWriteParams, IsMTRGroupKeyManagementClusterGroupKeySetStruct value) => mtrGroupKeyManagementClusterKeySetWriteParams -> value -> IO ()
setGroupKeySet mtrGroupKeyManagementClusterKeySetWriteParams value =
  sendMessage mtrGroupKeyManagementClusterKeySetWriteParams setGroupKeySetSelector (toMTRGroupKeyManagementClusterGroupKeySetStruct value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRGroupKeyManagementClusterKeySetWriteParams mtrGroupKeyManagementClusterKeySetWriteParams => mtrGroupKeyManagementClusterKeySetWriteParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrGroupKeyManagementClusterKeySetWriteParams =
  sendMessage mtrGroupKeyManagementClusterKeySetWriteParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRGroupKeyManagementClusterKeySetWriteParams mtrGroupKeyManagementClusterKeySetWriteParams, IsNSNumber value) => mtrGroupKeyManagementClusterKeySetWriteParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrGroupKeyManagementClusterKeySetWriteParams value =
  sendMessage mtrGroupKeyManagementClusterKeySetWriteParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRGroupKeyManagementClusterKeySetWriteParams mtrGroupKeyManagementClusterKeySetWriteParams => mtrGroupKeyManagementClusterKeySetWriteParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrGroupKeyManagementClusterKeySetWriteParams =
  sendMessage mtrGroupKeyManagementClusterKeySetWriteParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRGroupKeyManagementClusterKeySetWriteParams mtrGroupKeyManagementClusterKeySetWriteParams, IsNSNumber value) => mtrGroupKeyManagementClusterKeySetWriteParams -> value -> IO ()
setServerSideProcessingTimeout mtrGroupKeyManagementClusterKeySetWriteParams value =
  sendMessage mtrGroupKeyManagementClusterKeySetWriteParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @groupKeySet@
groupKeySetSelector :: Selector '[] (Id MTRGroupKeyManagementClusterGroupKeySetStruct)
groupKeySetSelector = mkSelector "groupKeySet"

-- | @Selector@ for @setGroupKeySet:@
setGroupKeySetSelector :: Selector '[Id MTRGroupKeyManagementClusterGroupKeySetStruct] ()
setGroupKeySetSelector = mkSelector "setGroupKeySet:"

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

