{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRColorControlClusterStepHueParams@.
module ObjC.Matter.MTRColorControlClusterStepHueParams
  ( MTRColorControlClusterStepHueParams
  , IsMTRColorControlClusterStepHueParams(..)
  , stepMode
  , setStepMode
  , stepSize
  , setStepSize
  , transitionTime
  , setTransitionTime
  , optionsMask
  , setOptionsMask
  , optionsOverride
  , setOptionsOverride
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , optionsMaskSelector
  , optionsOverrideSelector
  , serverSideProcessingTimeoutSelector
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
stepMode :: IsMTRColorControlClusterStepHueParams mtrColorControlClusterStepHueParams => mtrColorControlClusterStepHueParams -> IO (Id NSNumber)
stepMode mtrColorControlClusterStepHueParams =
  sendMessage mtrColorControlClusterStepHueParams stepModeSelector

-- | @- setStepMode:@
setStepMode :: (IsMTRColorControlClusterStepHueParams mtrColorControlClusterStepHueParams, IsNSNumber value) => mtrColorControlClusterStepHueParams -> value -> IO ()
setStepMode mtrColorControlClusterStepHueParams value =
  sendMessage mtrColorControlClusterStepHueParams setStepModeSelector (toNSNumber value)

-- | @- stepSize@
stepSize :: IsMTRColorControlClusterStepHueParams mtrColorControlClusterStepHueParams => mtrColorControlClusterStepHueParams -> IO (Id NSNumber)
stepSize mtrColorControlClusterStepHueParams =
  sendMessage mtrColorControlClusterStepHueParams stepSizeSelector

-- | @- setStepSize:@
setStepSize :: (IsMTRColorControlClusterStepHueParams mtrColorControlClusterStepHueParams, IsNSNumber value) => mtrColorControlClusterStepHueParams -> value -> IO ()
setStepSize mtrColorControlClusterStepHueParams value =
  sendMessage mtrColorControlClusterStepHueParams setStepSizeSelector (toNSNumber value)

-- | @- transitionTime@
transitionTime :: IsMTRColorControlClusterStepHueParams mtrColorControlClusterStepHueParams => mtrColorControlClusterStepHueParams -> IO (Id NSNumber)
transitionTime mtrColorControlClusterStepHueParams =
  sendMessage mtrColorControlClusterStepHueParams transitionTimeSelector

-- | @- setTransitionTime:@
setTransitionTime :: (IsMTRColorControlClusterStepHueParams mtrColorControlClusterStepHueParams, IsNSNumber value) => mtrColorControlClusterStepHueParams -> value -> IO ()
setTransitionTime mtrColorControlClusterStepHueParams value =
  sendMessage mtrColorControlClusterStepHueParams setTransitionTimeSelector (toNSNumber value)

-- | @- optionsMask@
optionsMask :: IsMTRColorControlClusterStepHueParams mtrColorControlClusterStepHueParams => mtrColorControlClusterStepHueParams -> IO (Id NSNumber)
optionsMask mtrColorControlClusterStepHueParams =
  sendMessage mtrColorControlClusterStepHueParams optionsMaskSelector

-- | @- setOptionsMask:@
setOptionsMask :: (IsMTRColorControlClusterStepHueParams mtrColorControlClusterStepHueParams, IsNSNumber value) => mtrColorControlClusterStepHueParams -> value -> IO ()
setOptionsMask mtrColorControlClusterStepHueParams value =
  sendMessage mtrColorControlClusterStepHueParams setOptionsMaskSelector (toNSNumber value)

-- | @- optionsOverride@
optionsOverride :: IsMTRColorControlClusterStepHueParams mtrColorControlClusterStepHueParams => mtrColorControlClusterStepHueParams -> IO (Id NSNumber)
optionsOverride mtrColorControlClusterStepHueParams =
  sendMessage mtrColorControlClusterStepHueParams optionsOverrideSelector

-- | @- setOptionsOverride:@
setOptionsOverride :: (IsMTRColorControlClusterStepHueParams mtrColorControlClusterStepHueParams, IsNSNumber value) => mtrColorControlClusterStepHueParams -> value -> IO ()
setOptionsOverride mtrColorControlClusterStepHueParams value =
  sendMessage mtrColorControlClusterStepHueParams setOptionsOverrideSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRColorControlClusterStepHueParams mtrColorControlClusterStepHueParams => mtrColorControlClusterStepHueParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrColorControlClusterStepHueParams =
  sendMessage mtrColorControlClusterStepHueParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRColorControlClusterStepHueParams mtrColorControlClusterStepHueParams, IsNSNumber value) => mtrColorControlClusterStepHueParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrColorControlClusterStepHueParams value =
  sendMessage mtrColorControlClusterStepHueParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRColorControlClusterStepHueParams mtrColorControlClusterStepHueParams => mtrColorControlClusterStepHueParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrColorControlClusterStepHueParams =
  sendMessage mtrColorControlClusterStepHueParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRColorControlClusterStepHueParams mtrColorControlClusterStepHueParams, IsNSNumber value) => mtrColorControlClusterStepHueParams -> value -> IO ()
setServerSideProcessingTimeout mtrColorControlClusterStepHueParams value =
  sendMessage mtrColorControlClusterStepHueParams setServerSideProcessingTimeoutSelector (toNSNumber value)

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

