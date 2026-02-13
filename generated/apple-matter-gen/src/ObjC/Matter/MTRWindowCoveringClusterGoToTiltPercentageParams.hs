{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRWindowCoveringClusterGoToTiltPercentageParams@.
module ObjC.Matter.MTRWindowCoveringClusterGoToTiltPercentageParams
  ( MTRWindowCoveringClusterGoToTiltPercentageParams
  , IsMTRWindowCoveringClusterGoToTiltPercentageParams(..)
  , tiltPercent100thsValue
  , setTiltPercent100thsValue
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , serverSideProcessingTimeoutSelector
  , setServerSideProcessingTimeoutSelector
  , setTiltPercent100thsValueSelector
  , setTimedInvokeTimeoutMsSelector
  , tiltPercent100thsValueSelector
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

-- | @- tiltPercent100thsValue@
tiltPercent100thsValue :: IsMTRWindowCoveringClusterGoToTiltPercentageParams mtrWindowCoveringClusterGoToTiltPercentageParams => mtrWindowCoveringClusterGoToTiltPercentageParams -> IO (Id NSNumber)
tiltPercent100thsValue mtrWindowCoveringClusterGoToTiltPercentageParams =
  sendMessage mtrWindowCoveringClusterGoToTiltPercentageParams tiltPercent100thsValueSelector

-- | @- setTiltPercent100thsValue:@
setTiltPercent100thsValue :: (IsMTRWindowCoveringClusterGoToTiltPercentageParams mtrWindowCoveringClusterGoToTiltPercentageParams, IsNSNumber value) => mtrWindowCoveringClusterGoToTiltPercentageParams -> value -> IO ()
setTiltPercent100thsValue mtrWindowCoveringClusterGoToTiltPercentageParams value =
  sendMessage mtrWindowCoveringClusterGoToTiltPercentageParams setTiltPercent100thsValueSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRWindowCoveringClusterGoToTiltPercentageParams mtrWindowCoveringClusterGoToTiltPercentageParams => mtrWindowCoveringClusterGoToTiltPercentageParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrWindowCoveringClusterGoToTiltPercentageParams =
  sendMessage mtrWindowCoveringClusterGoToTiltPercentageParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRWindowCoveringClusterGoToTiltPercentageParams mtrWindowCoveringClusterGoToTiltPercentageParams, IsNSNumber value) => mtrWindowCoveringClusterGoToTiltPercentageParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrWindowCoveringClusterGoToTiltPercentageParams value =
  sendMessage mtrWindowCoveringClusterGoToTiltPercentageParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRWindowCoveringClusterGoToTiltPercentageParams mtrWindowCoveringClusterGoToTiltPercentageParams => mtrWindowCoveringClusterGoToTiltPercentageParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrWindowCoveringClusterGoToTiltPercentageParams =
  sendMessage mtrWindowCoveringClusterGoToTiltPercentageParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRWindowCoveringClusterGoToTiltPercentageParams mtrWindowCoveringClusterGoToTiltPercentageParams, IsNSNumber value) => mtrWindowCoveringClusterGoToTiltPercentageParams -> value -> IO ()
setServerSideProcessingTimeout mtrWindowCoveringClusterGoToTiltPercentageParams value =
  sendMessage mtrWindowCoveringClusterGoToTiltPercentageParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @tiltPercent100thsValue@
tiltPercent100thsValueSelector :: Selector '[] (Id NSNumber)
tiltPercent100thsValueSelector = mkSelector "tiltPercent100thsValue"

-- | @Selector@ for @setTiltPercent100thsValue:@
setTiltPercent100thsValueSelector :: Selector '[Id NSNumber] ()
setTiltPercent100thsValueSelector = mkSelector "setTiltPercent100thsValue:"

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

