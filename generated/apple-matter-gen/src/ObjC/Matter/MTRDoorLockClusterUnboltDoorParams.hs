{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDoorLockClusterUnboltDoorParams@.
module ObjC.Matter.MTRDoorLockClusterUnboltDoorParams
  ( MTRDoorLockClusterUnboltDoorParams
  , IsMTRDoorLockClusterUnboltDoorParams(..)
  , pinCode
  , setPinCode
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , pinCodeSelector
  , serverSideProcessingTimeoutSelector
  , setPinCodeSelector
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

-- | @- pinCode@
pinCode :: IsMTRDoorLockClusterUnboltDoorParams mtrDoorLockClusterUnboltDoorParams => mtrDoorLockClusterUnboltDoorParams -> IO (Id NSData)
pinCode mtrDoorLockClusterUnboltDoorParams =
  sendMessage mtrDoorLockClusterUnboltDoorParams pinCodeSelector

-- | @- setPinCode:@
setPinCode :: (IsMTRDoorLockClusterUnboltDoorParams mtrDoorLockClusterUnboltDoorParams, IsNSData value) => mtrDoorLockClusterUnboltDoorParams -> value -> IO ()
setPinCode mtrDoorLockClusterUnboltDoorParams value =
  sendMessage mtrDoorLockClusterUnboltDoorParams setPinCodeSelector (toNSData value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRDoorLockClusterUnboltDoorParams mtrDoorLockClusterUnboltDoorParams => mtrDoorLockClusterUnboltDoorParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrDoorLockClusterUnboltDoorParams =
  sendMessage mtrDoorLockClusterUnboltDoorParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRDoorLockClusterUnboltDoorParams mtrDoorLockClusterUnboltDoorParams, IsNSNumber value) => mtrDoorLockClusterUnboltDoorParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrDoorLockClusterUnboltDoorParams value =
  sendMessage mtrDoorLockClusterUnboltDoorParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRDoorLockClusterUnboltDoorParams mtrDoorLockClusterUnboltDoorParams => mtrDoorLockClusterUnboltDoorParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrDoorLockClusterUnboltDoorParams =
  sendMessage mtrDoorLockClusterUnboltDoorParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRDoorLockClusterUnboltDoorParams mtrDoorLockClusterUnboltDoorParams, IsNSNumber value) => mtrDoorLockClusterUnboltDoorParams -> value -> IO ()
setServerSideProcessingTimeout mtrDoorLockClusterUnboltDoorParams value =
  sendMessage mtrDoorLockClusterUnboltDoorParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @pinCode@
pinCodeSelector :: Selector '[] (Id NSData)
pinCodeSelector = mkSelector "pinCode"

-- | @Selector@ for @setPinCode:@
setPinCodeSelector :: Selector '[Id NSData] ()
setPinCodeSelector = mkSelector "setPinCode:"

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

