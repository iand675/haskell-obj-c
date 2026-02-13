{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRColorControlClusterColorLoopSetParams@.
module ObjC.Matter.MTRColorControlClusterColorLoopSetParams
  ( MTRColorControlClusterColorLoopSetParams
  , IsMTRColorControlClusterColorLoopSetParams(..)
  , updateFlags
  , setUpdateFlags
  , action
  , setAction
  , direction
  , setDirection
  , time
  , setTime
  , startHue
  , setStartHue
  , optionsMask
  , setOptionsMask
  , optionsOverride
  , setOptionsOverride
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , actionSelector
  , directionSelector
  , optionsMaskSelector
  , optionsOverrideSelector
  , serverSideProcessingTimeoutSelector
  , setActionSelector
  , setDirectionSelector
  , setOptionsMaskSelector
  , setOptionsOverrideSelector
  , setServerSideProcessingTimeoutSelector
  , setStartHueSelector
  , setTimeSelector
  , setTimedInvokeTimeoutMsSelector
  , setUpdateFlagsSelector
  , startHueSelector
  , timeSelector
  , timedInvokeTimeoutMsSelector
  , updateFlagsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- updateFlags@
updateFlags :: IsMTRColorControlClusterColorLoopSetParams mtrColorControlClusterColorLoopSetParams => mtrColorControlClusterColorLoopSetParams -> IO (Id NSNumber)
updateFlags mtrColorControlClusterColorLoopSetParams =
  sendMessage mtrColorControlClusterColorLoopSetParams updateFlagsSelector

-- | @- setUpdateFlags:@
setUpdateFlags :: (IsMTRColorControlClusterColorLoopSetParams mtrColorControlClusterColorLoopSetParams, IsNSNumber value) => mtrColorControlClusterColorLoopSetParams -> value -> IO ()
setUpdateFlags mtrColorControlClusterColorLoopSetParams value =
  sendMessage mtrColorControlClusterColorLoopSetParams setUpdateFlagsSelector (toNSNumber value)

-- | @- action@
action :: IsMTRColorControlClusterColorLoopSetParams mtrColorControlClusterColorLoopSetParams => mtrColorControlClusterColorLoopSetParams -> IO (Id NSNumber)
action mtrColorControlClusterColorLoopSetParams =
  sendMessage mtrColorControlClusterColorLoopSetParams actionSelector

-- | @- setAction:@
setAction :: (IsMTRColorControlClusterColorLoopSetParams mtrColorControlClusterColorLoopSetParams, IsNSNumber value) => mtrColorControlClusterColorLoopSetParams -> value -> IO ()
setAction mtrColorControlClusterColorLoopSetParams value =
  sendMessage mtrColorControlClusterColorLoopSetParams setActionSelector (toNSNumber value)

-- | @- direction@
direction :: IsMTRColorControlClusterColorLoopSetParams mtrColorControlClusterColorLoopSetParams => mtrColorControlClusterColorLoopSetParams -> IO (Id NSNumber)
direction mtrColorControlClusterColorLoopSetParams =
  sendMessage mtrColorControlClusterColorLoopSetParams directionSelector

-- | @- setDirection:@
setDirection :: (IsMTRColorControlClusterColorLoopSetParams mtrColorControlClusterColorLoopSetParams, IsNSNumber value) => mtrColorControlClusterColorLoopSetParams -> value -> IO ()
setDirection mtrColorControlClusterColorLoopSetParams value =
  sendMessage mtrColorControlClusterColorLoopSetParams setDirectionSelector (toNSNumber value)

-- | @- time@
time :: IsMTRColorControlClusterColorLoopSetParams mtrColorControlClusterColorLoopSetParams => mtrColorControlClusterColorLoopSetParams -> IO (Id NSNumber)
time mtrColorControlClusterColorLoopSetParams =
  sendMessage mtrColorControlClusterColorLoopSetParams timeSelector

-- | @- setTime:@
setTime :: (IsMTRColorControlClusterColorLoopSetParams mtrColorControlClusterColorLoopSetParams, IsNSNumber value) => mtrColorControlClusterColorLoopSetParams -> value -> IO ()
setTime mtrColorControlClusterColorLoopSetParams value =
  sendMessage mtrColorControlClusterColorLoopSetParams setTimeSelector (toNSNumber value)

-- | @- startHue@
startHue :: IsMTRColorControlClusterColorLoopSetParams mtrColorControlClusterColorLoopSetParams => mtrColorControlClusterColorLoopSetParams -> IO (Id NSNumber)
startHue mtrColorControlClusterColorLoopSetParams =
  sendMessage mtrColorControlClusterColorLoopSetParams startHueSelector

-- | @- setStartHue:@
setStartHue :: (IsMTRColorControlClusterColorLoopSetParams mtrColorControlClusterColorLoopSetParams, IsNSNumber value) => mtrColorControlClusterColorLoopSetParams -> value -> IO ()
setStartHue mtrColorControlClusterColorLoopSetParams value =
  sendMessage mtrColorControlClusterColorLoopSetParams setStartHueSelector (toNSNumber value)

-- | @- optionsMask@
optionsMask :: IsMTRColorControlClusterColorLoopSetParams mtrColorControlClusterColorLoopSetParams => mtrColorControlClusterColorLoopSetParams -> IO (Id NSNumber)
optionsMask mtrColorControlClusterColorLoopSetParams =
  sendMessage mtrColorControlClusterColorLoopSetParams optionsMaskSelector

-- | @- setOptionsMask:@
setOptionsMask :: (IsMTRColorControlClusterColorLoopSetParams mtrColorControlClusterColorLoopSetParams, IsNSNumber value) => mtrColorControlClusterColorLoopSetParams -> value -> IO ()
setOptionsMask mtrColorControlClusterColorLoopSetParams value =
  sendMessage mtrColorControlClusterColorLoopSetParams setOptionsMaskSelector (toNSNumber value)

-- | @- optionsOverride@
optionsOverride :: IsMTRColorControlClusterColorLoopSetParams mtrColorControlClusterColorLoopSetParams => mtrColorControlClusterColorLoopSetParams -> IO (Id NSNumber)
optionsOverride mtrColorControlClusterColorLoopSetParams =
  sendMessage mtrColorControlClusterColorLoopSetParams optionsOverrideSelector

-- | @- setOptionsOverride:@
setOptionsOverride :: (IsMTRColorControlClusterColorLoopSetParams mtrColorControlClusterColorLoopSetParams, IsNSNumber value) => mtrColorControlClusterColorLoopSetParams -> value -> IO ()
setOptionsOverride mtrColorControlClusterColorLoopSetParams value =
  sendMessage mtrColorControlClusterColorLoopSetParams setOptionsOverrideSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRColorControlClusterColorLoopSetParams mtrColorControlClusterColorLoopSetParams => mtrColorControlClusterColorLoopSetParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrColorControlClusterColorLoopSetParams =
  sendMessage mtrColorControlClusterColorLoopSetParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRColorControlClusterColorLoopSetParams mtrColorControlClusterColorLoopSetParams, IsNSNumber value) => mtrColorControlClusterColorLoopSetParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrColorControlClusterColorLoopSetParams value =
  sendMessage mtrColorControlClusterColorLoopSetParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRColorControlClusterColorLoopSetParams mtrColorControlClusterColorLoopSetParams => mtrColorControlClusterColorLoopSetParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrColorControlClusterColorLoopSetParams =
  sendMessage mtrColorControlClusterColorLoopSetParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRColorControlClusterColorLoopSetParams mtrColorControlClusterColorLoopSetParams, IsNSNumber value) => mtrColorControlClusterColorLoopSetParams -> value -> IO ()
setServerSideProcessingTimeout mtrColorControlClusterColorLoopSetParams value =
  sendMessage mtrColorControlClusterColorLoopSetParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @updateFlags@
updateFlagsSelector :: Selector '[] (Id NSNumber)
updateFlagsSelector = mkSelector "updateFlags"

-- | @Selector@ for @setUpdateFlags:@
setUpdateFlagsSelector :: Selector '[Id NSNumber] ()
setUpdateFlagsSelector = mkSelector "setUpdateFlags:"

-- | @Selector@ for @action@
actionSelector :: Selector '[] (Id NSNumber)
actionSelector = mkSelector "action"

-- | @Selector@ for @setAction:@
setActionSelector :: Selector '[Id NSNumber] ()
setActionSelector = mkSelector "setAction:"

-- | @Selector@ for @direction@
directionSelector :: Selector '[] (Id NSNumber)
directionSelector = mkSelector "direction"

-- | @Selector@ for @setDirection:@
setDirectionSelector :: Selector '[Id NSNumber] ()
setDirectionSelector = mkSelector "setDirection:"

-- | @Selector@ for @time@
timeSelector :: Selector '[] (Id NSNumber)
timeSelector = mkSelector "time"

-- | @Selector@ for @setTime:@
setTimeSelector :: Selector '[Id NSNumber] ()
setTimeSelector = mkSelector "setTime:"

-- | @Selector@ for @startHue@
startHueSelector :: Selector '[] (Id NSNumber)
startHueSelector = mkSelector "startHue"

-- | @Selector@ for @setStartHue:@
setStartHueSelector :: Selector '[Id NSNumber] ()
setStartHueSelector = mkSelector "setStartHue:"

-- | @Selector@ for @optionsMask@
optionsMaskSelector :: Selector '[] (Id NSNumber)
optionsMaskSelector = mkSelector "optionsMask"

-- | @Selector@ for @setOptionsMask:@
setOptionsMaskSelector :: Selector '[Id NSNumber] ()
setOptionsMaskSelector = mkSelector "setOptionsMask:"

-- | @Selector@ for @optionsOverride@
optionsOverrideSelector :: Selector '[] (Id NSNumber)
optionsOverrideSelector = mkSelector "optionsOverride"

-- | @Selector@ for @setOptionsOverride:@
setOptionsOverrideSelector :: Selector '[Id NSNumber] ()
setOptionsOverrideSelector = mkSelector "setOptionsOverride:"

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

