{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRGeneralDiagnosticsClusterPayloadTestRequestParams@.
module ObjC.Matter.MTRGeneralDiagnosticsClusterPayloadTestRequestParams
  ( MTRGeneralDiagnosticsClusterPayloadTestRequestParams
  , IsMTRGeneralDiagnosticsClusterPayloadTestRequestParams(..)
  , enableKey
  , setEnableKey
  , value
  , setValue
  , count
  , setCount
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , countSelector
  , enableKeySelector
  , serverSideProcessingTimeoutSelector
  , setCountSelector
  , setEnableKeySelector
  , setServerSideProcessingTimeoutSelector
  , setTimedInvokeTimeoutMsSelector
  , setValueSelector
  , timedInvokeTimeoutMsSelector
  , valueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- enableKey@
enableKey :: IsMTRGeneralDiagnosticsClusterPayloadTestRequestParams mtrGeneralDiagnosticsClusterPayloadTestRequestParams => mtrGeneralDiagnosticsClusterPayloadTestRequestParams -> IO (Id NSData)
enableKey mtrGeneralDiagnosticsClusterPayloadTestRequestParams =
  sendMessage mtrGeneralDiagnosticsClusterPayloadTestRequestParams enableKeySelector

-- | @- setEnableKey:@
setEnableKey :: (IsMTRGeneralDiagnosticsClusterPayloadTestRequestParams mtrGeneralDiagnosticsClusterPayloadTestRequestParams, IsNSData value) => mtrGeneralDiagnosticsClusterPayloadTestRequestParams -> value -> IO ()
setEnableKey mtrGeneralDiagnosticsClusterPayloadTestRequestParams value =
  sendMessage mtrGeneralDiagnosticsClusterPayloadTestRequestParams setEnableKeySelector (toNSData value)

-- | @- value@
value :: IsMTRGeneralDiagnosticsClusterPayloadTestRequestParams mtrGeneralDiagnosticsClusterPayloadTestRequestParams => mtrGeneralDiagnosticsClusterPayloadTestRequestParams -> IO (Id NSNumber)
value mtrGeneralDiagnosticsClusterPayloadTestRequestParams =
  sendMessage mtrGeneralDiagnosticsClusterPayloadTestRequestParams valueSelector

-- | @- setValue:@
setValue :: (IsMTRGeneralDiagnosticsClusterPayloadTestRequestParams mtrGeneralDiagnosticsClusterPayloadTestRequestParams, IsNSNumber value) => mtrGeneralDiagnosticsClusterPayloadTestRequestParams -> value -> IO ()
setValue mtrGeneralDiagnosticsClusterPayloadTestRequestParams value =
  sendMessage mtrGeneralDiagnosticsClusterPayloadTestRequestParams setValueSelector (toNSNumber value)

-- | @- count@
count :: IsMTRGeneralDiagnosticsClusterPayloadTestRequestParams mtrGeneralDiagnosticsClusterPayloadTestRequestParams => mtrGeneralDiagnosticsClusterPayloadTestRequestParams -> IO (Id NSNumber)
count mtrGeneralDiagnosticsClusterPayloadTestRequestParams =
  sendMessage mtrGeneralDiagnosticsClusterPayloadTestRequestParams countSelector

-- | @- setCount:@
setCount :: (IsMTRGeneralDiagnosticsClusterPayloadTestRequestParams mtrGeneralDiagnosticsClusterPayloadTestRequestParams, IsNSNumber value) => mtrGeneralDiagnosticsClusterPayloadTestRequestParams -> value -> IO ()
setCount mtrGeneralDiagnosticsClusterPayloadTestRequestParams value =
  sendMessage mtrGeneralDiagnosticsClusterPayloadTestRequestParams setCountSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRGeneralDiagnosticsClusterPayloadTestRequestParams mtrGeneralDiagnosticsClusterPayloadTestRequestParams => mtrGeneralDiagnosticsClusterPayloadTestRequestParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrGeneralDiagnosticsClusterPayloadTestRequestParams =
  sendMessage mtrGeneralDiagnosticsClusterPayloadTestRequestParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRGeneralDiagnosticsClusterPayloadTestRequestParams mtrGeneralDiagnosticsClusterPayloadTestRequestParams, IsNSNumber value) => mtrGeneralDiagnosticsClusterPayloadTestRequestParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrGeneralDiagnosticsClusterPayloadTestRequestParams value =
  sendMessage mtrGeneralDiagnosticsClusterPayloadTestRequestParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRGeneralDiagnosticsClusterPayloadTestRequestParams mtrGeneralDiagnosticsClusterPayloadTestRequestParams => mtrGeneralDiagnosticsClusterPayloadTestRequestParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrGeneralDiagnosticsClusterPayloadTestRequestParams =
  sendMessage mtrGeneralDiagnosticsClusterPayloadTestRequestParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRGeneralDiagnosticsClusterPayloadTestRequestParams mtrGeneralDiagnosticsClusterPayloadTestRequestParams, IsNSNumber value) => mtrGeneralDiagnosticsClusterPayloadTestRequestParams -> value -> IO ()
setServerSideProcessingTimeout mtrGeneralDiagnosticsClusterPayloadTestRequestParams value =
  sendMessage mtrGeneralDiagnosticsClusterPayloadTestRequestParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @enableKey@
enableKeySelector :: Selector '[] (Id NSData)
enableKeySelector = mkSelector "enableKey"

-- | @Selector@ for @setEnableKey:@
setEnableKeySelector :: Selector '[Id NSData] ()
setEnableKeySelector = mkSelector "setEnableKey:"

-- | @Selector@ for @value@
valueSelector :: Selector '[] (Id NSNumber)
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector '[Id NSNumber] ()
setValueSelector = mkSelector "setValue:"

-- | @Selector@ for @count@
countSelector :: Selector '[] (Id NSNumber)
countSelector = mkSelector "count"

-- | @Selector@ for @setCount:@
setCountSelector :: Selector '[Id NSNumber] ()
setCountSelector = mkSelector "setCount:"

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

