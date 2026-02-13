{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRUnitTestingClusterGlobalEchoRequestParams@.
module ObjC.Matter.MTRUnitTestingClusterGlobalEchoRequestParams
  ( MTRUnitTestingClusterGlobalEchoRequestParams
  , IsMTRUnitTestingClusterGlobalEchoRequestParams(..)
  , field1
  , setField1
  , field2
  , setField2
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , field1Selector
  , field2Selector
  , serverSideProcessingTimeoutSelector
  , setField1Selector
  , setField2Selector
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

-- | @- field1@
field1 :: IsMTRUnitTestingClusterGlobalEchoRequestParams mtrUnitTestingClusterGlobalEchoRequestParams => mtrUnitTestingClusterGlobalEchoRequestParams -> IO (Id MTRDataTypeTestGlobalStruct)
field1 mtrUnitTestingClusterGlobalEchoRequestParams =
  sendMessage mtrUnitTestingClusterGlobalEchoRequestParams field1Selector

-- | @- setField1:@
setField1 :: (IsMTRUnitTestingClusterGlobalEchoRequestParams mtrUnitTestingClusterGlobalEchoRequestParams, IsMTRDataTypeTestGlobalStruct value) => mtrUnitTestingClusterGlobalEchoRequestParams -> value -> IO ()
setField1 mtrUnitTestingClusterGlobalEchoRequestParams value =
  sendMessage mtrUnitTestingClusterGlobalEchoRequestParams setField1Selector (toMTRDataTypeTestGlobalStruct value)

-- | @- field2@
field2 :: IsMTRUnitTestingClusterGlobalEchoRequestParams mtrUnitTestingClusterGlobalEchoRequestParams => mtrUnitTestingClusterGlobalEchoRequestParams -> IO (Id NSNumber)
field2 mtrUnitTestingClusterGlobalEchoRequestParams =
  sendMessage mtrUnitTestingClusterGlobalEchoRequestParams field2Selector

-- | @- setField2:@
setField2 :: (IsMTRUnitTestingClusterGlobalEchoRequestParams mtrUnitTestingClusterGlobalEchoRequestParams, IsNSNumber value) => mtrUnitTestingClusterGlobalEchoRequestParams -> value -> IO ()
setField2 mtrUnitTestingClusterGlobalEchoRequestParams value =
  sendMessage mtrUnitTestingClusterGlobalEchoRequestParams setField2Selector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRUnitTestingClusterGlobalEchoRequestParams mtrUnitTestingClusterGlobalEchoRequestParams => mtrUnitTestingClusterGlobalEchoRequestParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrUnitTestingClusterGlobalEchoRequestParams =
  sendMessage mtrUnitTestingClusterGlobalEchoRequestParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRUnitTestingClusterGlobalEchoRequestParams mtrUnitTestingClusterGlobalEchoRequestParams, IsNSNumber value) => mtrUnitTestingClusterGlobalEchoRequestParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrUnitTestingClusterGlobalEchoRequestParams value =
  sendMessage mtrUnitTestingClusterGlobalEchoRequestParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRUnitTestingClusterGlobalEchoRequestParams mtrUnitTestingClusterGlobalEchoRequestParams => mtrUnitTestingClusterGlobalEchoRequestParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrUnitTestingClusterGlobalEchoRequestParams =
  sendMessage mtrUnitTestingClusterGlobalEchoRequestParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRUnitTestingClusterGlobalEchoRequestParams mtrUnitTestingClusterGlobalEchoRequestParams, IsNSNumber value) => mtrUnitTestingClusterGlobalEchoRequestParams -> value -> IO ()
setServerSideProcessingTimeout mtrUnitTestingClusterGlobalEchoRequestParams value =
  sendMessage mtrUnitTestingClusterGlobalEchoRequestParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @field1@
field1Selector :: Selector '[] (Id MTRDataTypeTestGlobalStruct)
field1Selector = mkSelector "field1"

-- | @Selector@ for @setField1:@
setField1Selector :: Selector '[Id MTRDataTypeTestGlobalStruct] ()
setField1Selector = mkSelector "setField1:"

-- | @Selector@ for @field2@
field2Selector :: Selector '[] (Id NSNumber)
field2Selector = mkSelector "field2"

-- | @Selector@ for @setField2:@
setField2Selector :: Selector '[Id NSNumber] ()
setField2Selector = mkSelector "setField2:"

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

