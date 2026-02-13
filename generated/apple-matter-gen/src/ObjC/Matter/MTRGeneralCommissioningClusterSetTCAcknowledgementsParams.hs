{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRGeneralCommissioningClusterSetTCAcknowledgementsParams@.
module ObjC.Matter.MTRGeneralCommissioningClusterSetTCAcknowledgementsParams
  ( MTRGeneralCommissioningClusterSetTCAcknowledgementsParams
  , IsMTRGeneralCommissioningClusterSetTCAcknowledgementsParams(..)
  , tcVersion
  , setTcVersion
  , tcUserResponse
  , setTcUserResponse
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , serverSideProcessingTimeoutSelector
  , setServerSideProcessingTimeoutSelector
  , setTcUserResponseSelector
  , setTcVersionSelector
  , setTimedInvokeTimeoutMsSelector
  , tcUserResponseSelector
  , tcVersionSelector
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

-- | @- tcVersion@
tcVersion :: IsMTRGeneralCommissioningClusterSetTCAcknowledgementsParams mtrGeneralCommissioningClusterSetTCAcknowledgementsParams => mtrGeneralCommissioningClusterSetTCAcknowledgementsParams -> IO (Id NSNumber)
tcVersion mtrGeneralCommissioningClusterSetTCAcknowledgementsParams =
  sendMessage mtrGeneralCommissioningClusterSetTCAcknowledgementsParams tcVersionSelector

-- | @- setTcVersion:@
setTcVersion :: (IsMTRGeneralCommissioningClusterSetTCAcknowledgementsParams mtrGeneralCommissioningClusterSetTCAcknowledgementsParams, IsNSNumber value) => mtrGeneralCommissioningClusterSetTCAcknowledgementsParams -> value -> IO ()
setTcVersion mtrGeneralCommissioningClusterSetTCAcknowledgementsParams value =
  sendMessage mtrGeneralCommissioningClusterSetTCAcknowledgementsParams setTcVersionSelector (toNSNumber value)

-- | @- tcUserResponse@
tcUserResponse :: IsMTRGeneralCommissioningClusterSetTCAcknowledgementsParams mtrGeneralCommissioningClusterSetTCAcknowledgementsParams => mtrGeneralCommissioningClusterSetTCAcknowledgementsParams -> IO (Id NSNumber)
tcUserResponse mtrGeneralCommissioningClusterSetTCAcknowledgementsParams =
  sendMessage mtrGeneralCommissioningClusterSetTCAcknowledgementsParams tcUserResponseSelector

-- | @- setTcUserResponse:@
setTcUserResponse :: (IsMTRGeneralCommissioningClusterSetTCAcknowledgementsParams mtrGeneralCommissioningClusterSetTCAcknowledgementsParams, IsNSNumber value) => mtrGeneralCommissioningClusterSetTCAcknowledgementsParams -> value -> IO ()
setTcUserResponse mtrGeneralCommissioningClusterSetTCAcknowledgementsParams value =
  sendMessage mtrGeneralCommissioningClusterSetTCAcknowledgementsParams setTcUserResponseSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRGeneralCommissioningClusterSetTCAcknowledgementsParams mtrGeneralCommissioningClusterSetTCAcknowledgementsParams => mtrGeneralCommissioningClusterSetTCAcknowledgementsParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrGeneralCommissioningClusterSetTCAcknowledgementsParams =
  sendMessage mtrGeneralCommissioningClusterSetTCAcknowledgementsParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRGeneralCommissioningClusterSetTCAcknowledgementsParams mtrGeneralCommissioningClusterSetTCAcknowledgementsParams, IsNSNumber value) => mtrGeneralCommissioningClusterSetTCAcknowledgementsParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrGeneralCommissioningClusterSetTCAcknowledgementsParams value =
  sendMessage mtrGeneralCommissioningClusterSetTCAcknowledgementsParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRGeneralCommissioningClusterSetTCAcknowledgementsParams mtrGeneralCommissioningClusterSetTCAcknowledgementsParams => mtrGeneralCommissioningClusterSetTCAcknowledgementsParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrGeneralCommissioningClusterSetTCAcknowledgementsParams =
  sendMessage mtrGeneralCommissioningClusterSetTCAcknowledgementsParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRGeneralCommissioningClusterSetTCAcknowledgementsParams mtrGeneralCommissioningClusterSetTCAcknowledgementsParams, IsNSNumber value) => mtrGeneralCommissioningClusterSetTCAcknowledgementsParams -> value -> IO ()
setServerSideProcessingTimeout mtrGeneralCommissioningClusterSetTCAcknowledgementsParams value =
  sendMessage mtrGeneralCommissioningClusterSetTCAcknowledgementsParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @tcVersion@
tcVersionSelector :: Selector '[] (Id NSNumber)
tcVersionSelector = mkSelector "tcVersion"

-- | @Selector@ for @setTcVersion:@
setTcVersionSelector :: Selector '[Id NSNumber] ()
setTcVersionSelector = mkSelector "setTcVersion:"

-- | @Selector@ for @tcUserResponse@
tcUserResponseSelector :: Selector '[] (Id NSNumber)
tcUserResponseSelector = mkSelector "tcUserResponse"

-- | @Selector@ for @setTcUserResponse:@
setTcUserResponseSelector :: Selector '[Id NSNumber] ()
setTcUserResponseSelector = mkSelector "setTcUserResponse:"

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

