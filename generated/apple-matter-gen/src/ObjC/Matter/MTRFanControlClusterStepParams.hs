{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRFanControlClusterStepParams@.
module ObjC.Matter.MTRFanControlClusterStepParams
  ( MTRFanControlClusterStepParams
  , IsMTRFanControlClusterStepParams(..)
  , direction
  , setDirection
  , wrap
  , setWrap
  , lowestOff
  , setLowestOff
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , directionSelector
  , lowestOffSelector
  , serverSideProcessingTimeoutSelector
  , setDirectionSelector
  , setLowestOffSelector
  , setServerSideProcessingTimeoutSelector
  , setTimedInvokeTimeoutMsSelector
  , setWrapSelector
  , timedInvokeTimeoutMsSelector
  , wrapSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- direction@
direction :: IsMTRFanControlClusterStepParams mtrFanControlClusterStepParams => mtrFanControlClusterStepParams -> IO (Id NSNumber)
direction mtrFanControlClusterStepParams =
  sendMessage mtrFanControlClusterStepParams directionSelector

-- | @- setDirection:@
setDirection :: (IsMTRFanControlClusterStepParams mtrFanControlClusterStepParams, IsNSNumber value) => mtrFanControlClusterStepParams -> value -> IO ()
setDirection mtrFanControlClusterStepParams value =
  sendMessage mtrFanControlClusterStepParams setDirectionSelector (toNSNumber value)

-- | @- wrap@
wrap :: IsMTRFanControlClusterStepParams mtrFanControlClusterStepParams => mtrFanControlClusterStepParams -> IO (Id NSNumber)
wrap mtrFanControlClusterStepParams =
  sendMessage mtrFanControlClusterStepParams wrapSelector

-- | @- setWrap:@
setWrap :: (IsMTRFanControlClusterStepParams mtrFanControlClusterStepParams, IsNSNumber value) => mtrFanControlClusterStepParams -> value -> IO ()
setWrap mtrFanControlClusterStepParams value =
  sendMessage mtrFanControlClusterStepParams setWrapSelector (toNSNumber value)

-- | @- lowestOff@
lowestOff :: IsMTRFanControlClusterStepParams mtrFanControlClusterStepParams => mtrFanControlClusterStepParams -> IO (Id NSNumber)
lowestOff mtrFanControlClusterStepParams =
  sendMessage mtrFanControlClusterStepParams lowestOffSelector

-- | @- setLowestOff:@
setLowestOff :: (IsMTRFanControlClusterStepParams mtrFanControlClusterStepParams, IsNSNumber value) => mtrFanControlClusterStepParams -> value -> IO ()
setLowestOff mtrFanControlClusterStepParams value =
  sendMessage mtrFanControlClusterStepParams setLowestOffSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRFanControlClusterStepParams mtrFanControlClusterStepParams => mtrFanControlClusterStepParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrFanControlClusterStepParams =
  sendMessage mtrFanControlClusterStepParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRFanControlClusterStepParams mtrFanControlClusterStepParams, IsNSNumber value) => mtrFanControlClusterStepParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrFanControlClusterStepParams value =
  sendMessage mtrFanControlClusterStepParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRFanControlClusterStepParams mtrFanControlClusterStepParams => mtrFanControlClusterStepParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrFanControlClusterStepParams =
  sendMessage mtrFanControlClusterStepParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRFanControlClusterStepParams mtrFanControlClusterStepParams, IsNSNumber value) => mtrFanControlClusterStepParams -> value -> IO ()
setServerSideProcessingTimeout mtrFanControlClusterStepParams value =
  sendMessage mtrFanControlClusterStepParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @direction@
directionSelector :: Selector '[] (Id NSNumber)
directionSelector = mkSelector "direction"

-- | @Selector@ for @setDirection:@
setDirectionSelector :: Selector '[Id NSNumber] ()
setDirectionSelector = mkSelector "setDirection:"

-- | @Selector@ for @wrap@
wrapSelector :: Selector '[] (Id NSNumber)
wrapSelector = mkSelector "wrap"

-- | @Selector@ for @setWrap:@
setWrapSelector :: Selector '[Id NSNumber] ()
setWrapSelector = mkSelector "setWrap:"

-- | @Selector@ for @lowestOff@
lowestOffSelector :: Selector '[] (Id NSNumber)
lowestOffSelector = mkSelector "lowestOff"

-- | @Selector@ for @setLowestOff:@
setLowestOffSelector :: Selector '[Id NSNumber] ()
setLowestOffSelector = mkSelector "setLowestOff:"

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

