{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRColorControlClusterStepColorTemperatureParams@.
module ObjC.Matter.MTRColorControlClusterStepColorTemperatureParams
  ( MTRColorControlClusterStepColorTemperatureParams
  , IsMTRColorControlClusterStepColorTemperatureParams(..)
  , stepMode
  , setStepMode
  , stepSize
  , setStepSize
  , transitionTime
  , setTransitionTime
  , colorTemperatureMinimumMireds
  , setColorTemperatureMinimumMireds
  , colorTemperatureMaximumMireds
  , setColorTemperatureMaximumMireds
  , optionsMask
  , setOptionsMask
  , optionsOverride
  , setOptionsOverride
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , colorTemperatureMaximumMiredsSelector
  , colorTemperatureMinimumMiredsSelector
  , optionsMaskSelector
  , optionsOverrideSelector
  , serverSideProcessingTimeoutSelector
  , setColorTemperatureMaximumMiredsSelector
  , setColorTemperatureMinimumMiredsSelector
  , setOptionsMaskSelector
  , setOptionsOverrideSelector
  , setServerSideProcessingTimeoutSelector
  , setStepModeSelector
  , setStepSizeSelector
  , setTimedInvokeTimeoutMsSelector
  , setTransitionTimeSelector
  , stepModeSelector
  , stepSizeSelector
  , timedInvokeTimeoutMsSelector
  , transitionTimeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- stepMode@
stepMode :: IsMTRColorControlClusterStepColorTemperatureParams mtrColorControlClusterStepColorTemperatureParams => mtrColorControlClusterStepColorTemperatureParams -> IO (Id NSNumber)
stepMode mtrColorControlClusterStepColorTemperatureParams =
  sendMessage mtrColorControlClusterStepColorTemperatureParams stepModeSelector

-- | @- setStepMode:@
setStepMode :: (IsMTRColorControlClusterStepColorTemperatureParams mtrColorControlClusterStepColorTemperatureParams, IsNSNumber value) => mtrColorControlClusterStepColorTemperatureParams -> value -> IO ()
setStepMode mtrColorControlClusterStepColorTemperatureParams value =
  sendMessage mtrColorControlClusterStepColorTemperatureParams setStepModeSelector (toNSNumber value)

-- | @- stepSize@
stepSize :: IsMTRColorControlClusterStepColorTemperatureParams mtrColorControlClusterStepColorTemperatureParams => mtrColorControlClusterStepColorTemperatureParams -> IO (Id NSNumber)
stepSize mtrColorControlClusterStepColorTemperatureParams =
  sendMessage mtrColorControlClusterStepColorTemperatureParams stepSizeSelector

-- | @- setStepSize:@
setStepSize :: (IsMTRColorControlClusterStepColorTemperatureParams mtrColorControlClusterStepColorTemperatureParams, IsNSNumber value) => mtrColorControlClusterStepColorTemperatureParams -> value -> IO ()
setStepSize mtrColorControlClusterStepColorTemperatureParams value =
  sendMessage mtrColorControlClusterStepColorTemperatureParams setStepSizeSelector (toNSNumber value)

-- | @- transitionTime@
transitionTime :: IsMTRColorControlClusterStepColorTemperatureParams mtrColorControlClusterStepColorTemperatureParams => mtrColorControlClusterStepColorTemperatureParams -> IO (Id NSNumber)
transitionTime mtrColorControlClusterStepColorTemperatureParams =
  sendMessage mtrColorControlClusterStepColorTemperatureParams transitionTimeSelector

-- | @- setTransitionTime:@
setTransitionTime :: (IsMTRColorControlClusterStepColorTemperatureParams mtrColorControlClusterStepColorTemperatureParams, IsNSNumber value) => mtrColorControlClusterStepColorTemperatureParams -> value -> IO ()
setTransitionTime mtrColorControlClusterStepColorTemperatureParams value =
  sendMessage mtrColorControlClusterStepColorTemperatureParams setTransitionTimeSelector (toNSNumber value)

-- | @- colorTemperatureMinimumMireds@
colorTemperatureMinimumMireds :: IsMTRColorControlClusterStepColorTemperatureParams mtrColorControlClusterStepColorTemperatureParams => mtrColorControlClusterStepColorTemperatureParams -> IO (Id NSNumber)
colorTemperatureMinimumMireds mtrColorControlClusterStepColorTemperatureParams =
  sendMessage mtrColorControlClusterStepColorTemperatureParams colorTemperatureMinimumMiredsSelector

-- | @- setColorTemperatureMinimumMireds:@
setColorTemperatureMinimumMireds :: (IsMTRColorControlClusterStepColorTemperatureParams mtrColorControlClusterStepColorTemperatureParams, IsNSNumber value) => mtrColorControlClusterStepColorTemperatureParams -> value -> IO ()
setColorTemperatureMinimumMireds mtrColorControlClusterStepColorTemperatureParams value =
  sendMessage mtrColorControlClusterStepColorTemperatureParams setColorTemperatureMinimumMiredsSelector (toNSNumber value)

-- | @- colorTemperatureMaximumMireds@
colorTemperatureMaximumMireds :: IsMTRColorControlClusterStepColorTemperatureParams mtrColorControlClusterStepColorTemperatureParams => mtrColorControlClusterStepColorTemperatureParams -> IO (Id NSNumber)
colorTemperatureMaximumMireds mtrColorControlClusterStepColorTemperatureParams =
  sendMessage mtrColorControlClusterStepColorTemperatureParams colorTemperatureMaximumMiredsSelector

-- | @- setColorTemperatureMaximumMireds:@
setColorTemperatureMaximumMireds :: (IsMTRColorControlClusterStepColorTemperatureParams mtrColorControlClusterStepColorTemperatureParams, IsNSNumber value) => mtrColorControlClusterStepColorTemperatureParams -> value -> IO ()
setColorTemperatureMaximumMireds mtrColorControlClusterStepColorTemperatureParams value =
  sendMessage mtrColorControlClusterStepColorTemperatureParams setColorTemperatureMaximumMiredsSelector (toNSNumber value)

-- | @- optionsMask@
optionsMask :: IsMTRColorControlClusterStepColorTemperatureParams mtrColorControlClusterStepColorTemperatureParams => mtrColorControlClusterStepColorTemperatureParams -> IO (Id NSNumber)
optionsMask mtrColorControlClusterStepColorTemperatureParams =
  sendMessage mtrColorControlClusterStepColorTemperatureParams optionsMaskSelector

-- | @- setOptionsMask:@
setOptionsMask :: (IsMTRColorControlClusterStepColorTemperatureParams mtrColorControlClusterStepColorTemperatureParams, IsNSNumber value) => mtrColorControlClusterStepColorTemperatureParams -> value -> IO ()
setOptionsMask mtrColorControlClusterStepColorTemperatureParams value =
  sendMessage mtrColorControlClusterStepColorTemperatureParams setOptionsMaskSelector (toNSNumber value)

-- | @- optionsOverride@
optionsOverride :: IsMTRColorControlClusterStepColorTemperatureParams mtrColorControlClusterStepColorTemperatureParams => mtrColorControlClusterStepColorTemperatureParams -> IO (Id NSNumber)
optionsOverride mtrColorControlClusterStepColorTemperatureParams =
  sendMessage mtrColorControlClusterStepColorTemperatureParams optionsOverrideSelector

-- | @- setOptionsOverride:@
setOptionsOverride :: (IsMTRColorControlClusterStepColorTemperatureParams mtrColorControlClusterStepColorTemperatureParams, IsNSNumber value) => mtrColorControlClusterStepColorTemperatureParams -> value -> IO ()
setOptionsOverride mtrColorControlClusterStepColorTemperatureParams value =
  sendMessage mtrColorControlClusterStepColorTemperatureParams setOptionsOverrideSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRColorControlClusterStepColorTemperatureParams mtrColorControlClusterStepColorTemperatureParams => mtrColorControlClusterStepColorTemperatureParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrColorControlClusterStepColorTemperatureParams =
  sendMessage mtrColorControlClusterStepColorTemperatureParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRColorControlClusterStepColorTemperatureParams mtrColorControlClusterStepColorTemperatureParams, IsNSNumber value) => mtrColorControlClusterStepColorTemperatureParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrColorControlClusterStepColorTemperatureParams value =
  sendMessage mtrColorControlClusterStepColorTemperatureParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRColorControlClusterStepColorTemperatureParams mtrColorControlClusterStepColorTemperatureParams => mtrColorControlClusterStepColorTemperatureParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrColorControlClusterStepColorTemperatureParams =
  sendMessage mtrColorControlClusterStepColorTemperatureParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRColorControlClusterStepColorTemperatureParams mtrColorControlClusterStepColorTemperatureParams, IsNSNumber value) => mtrColorControlClusterStepColorTemperatureParams -> value -> IO ()
setServerSideProcessingTimeout mtrColorControlClusterStepColorTemperatureParams value =
  sendMessage mtrColorControlClusterStepColorTemperatureParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stepMode@
stepModeSelector :: Selector '[] (Id NSNumber)
stepModeSelector = mkSelector "stepMode"

-- | @Selector@ for @setStepMode:@
setStepModeSelector :: Selector '[Id NSNumber] ()
setStepModeSelector = mkSelector "setStepMode:"

-- | @Selector@ for @stepSize@
stepSizeSelector :: Selector '[] (Id NSNumber)
stepSizeSelector = mkSelector "stepSize"

-- | @Selector@ for @setStepSize:@
setStepSizeSelector :: Selector '[Id NSNumber] ()
setStepSizeSelector = mkSelector "setStepSize:"

-- | @Selector@ for @transitionTime@
transitionTimeSelector :: Selector '[] (Id NSNumber)
transitionTimeSelector = mkSelector "transitionTime"

-- | @Selector@ for @setTransitionTime:@
setTransitionTimeSelector :: Selector '[Id NSNumber] ()
setTransitionTimeSelector = mkSelector "setTransitionTime:"

-- | @Selector@ for @colorTemperatureMinimumMireds@
colorTemperatureMinimumMiredsSelector :: Selector '[] (Id NSNumber)
colorTemperatureMinimumMiredsSelector = mkSelector "colorTemperatureMinimumMireds"

-- | @Selector@ for @setColorTemperatureMinimumMireds:@
setColorTemperatureMinimumMiredsSelector :: Selector '[Id NSNumber] ()
setColorTemperatureMinimumMiredsSelector = mkSelector "setColorTemperatureMinimumMireds:"

-- | @Selector@ for @colorTemperatureMaximumMireds@
colorTemperatureMaximumMiredsSelector :: Selector '[] (Id NSNumber)
colorTemperatureMaximumMiredsSelector = mkSelector "colorTemperatureMaximumMireds"

-- | @Selector@ for @setColorTemperatureMaximumMireds:@
setColorTemperatureMaximumMiredsSelector :: Selector '[Id NSNumber] ()
setColorTemperatureMaximumMiredsSelector = mkSelector "setColorTemperatureMaximumMireds:"

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

