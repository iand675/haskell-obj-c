{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRColorControlClusterStepSaturationParams@.
module ObjC.Matter.MTRColorControlClusterStepSaturationParams
  ( MTRColorControlClusterStepSaturationParams
  , IsMTRColorControlClusterStepSaturationParams(..)
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
stepMode :: IsMTRColorControlClusterStepSaturationParams mtrColorControlClusterStepSaturationParams => mtrColorControlClusterStepSaturationParams -> IO (Id NSNumber)
stepMode mtrColorControlClusterStepSaturationParams =
  sendMessage mtrColorControlClusterStepSaturationParams stepModeSelector

-- | @- setStepMode:@
setStepMode :: (IsMTRColorControlClusterStepSaturationParams mtrColorControlClusterStepSaturationParams, IsNSNumber value) => mtrColorControlClusterStepSaturationParams -> value -> IO ()
setStepMode mtrColorControlClusterStepSaturationParams value =
  sendMessage mtrColorControlClusterStepSaturationParams setStepModeSelector (toNSNumber value)

-- | @- stepSize@
stepSize :: IsMTRColorControlClusterStepSaturationParams mtrColorControlClusterStepSaturationParams => mtrColorControlClusterStepSaturationParams -> IO (Id NSNumber)
stepSize mtrColorControlClusterStepSaturationParams =
  sendMessage mtrColorControlClusterStepSaturationParams stepSizeSelector

-- | @- setStepSize:@
setStepSize :: (IsMTRColorControlClusterStepSaturationParams mtrColorControlClusterStepSaturationParams, IsNSNumber value) => mtrColorControlClusterStepSaturationParams -> value -> IO ()
setStepSize mtrColorControlClusterStepSaturationParams value =
  sendMessage mtrColorControlClusterStepSaturationParams setStepSizeSelector (toNSNumber value)

-- | @- transitionTime@
transitionTime :: IsMTRColorControlClusterStepSaturationParams mtrColorControlClusterStepSaturationParams => mtrColorControlClusterStepSaturationParams -> IO (Id NSNumber)
transitionTime mtrColorControlClusterStepSaturationParams =
  sendMessage mtrColorControlClusterStepSaturationParams transitionTimeSelector

-- | @- setTransitionTime:@
setTransitionTime :: (IsMTRColorControlClusterStepSaturationParams mtrColorControlClusterStepSaturationParams, IsNSNumber value) => mtrColorControlClusterStepSaturationParams -> value -> IO ()
setTransitionTime mtrColorControlClusterStepSaturationParams value =
  sendMessage mtrColorControlClusterStepSaturationParams setTransitionTimeSelector (toNSNumber value)

-- | @- optionsMask@
optionsMask :: IsMTRColorControlClusterStepSaturationParams mtrColorControlClusterStepSaturationParams => mtrColorControlClusterStepSaturationParams -> IO (Id NSNumber)
optionsMask mtrColorControlClusterStepSaturationParams =
  sendMessage mtrColorControlClusterStepSaturationParams optionsMaskSelector

-- | @- setOptionsMask:@
setOptionsMask :: (IsMTRColorControlClusterStepSaturationParams mtrColorControlClusterStepSaturationParams, IsNSNumber value) => mtrColorControlClusterStepSaturationParams -> value -> IO ()
setOptionsMask mtrColorControlClusterStepSaturationParams value =
  sendMessage mtrColorControlClusterStepSaturationParams setOptionsMaskSelector (toNSNumber value)

-- | @- optionsOverride@
optionsOverride :: IsMTRColorControlClusterStepSaturationParams mtrColorControlClusterStepSaturationParams => mtrColorControlClusterStepSaturationParams -> IO (Id NSNumber)
optionsOverride mtrColorControlClusterStepSaturationParams =
  sendMessage mtrColorControlClusterStepSaturationParams optionsOverrideSelector

-- | @- setOptionsOverride:@
setOptionsOverride :: (IsMTRColorControlClusterStepSaturationParams mtrColorControlClusterStepSaturationParams, IsNSNumber value) => mtrColorControlClusterStepSaturationParams -> value -> IO ()
setOptionsOverride mtrColorControlClusterStepSaturationParams value =
  sendMessage mtrColorControlClusterStepSaturationParams setOptionsOverrideSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRColorControlClusterStepSaturationParams mtrColorControlClusterStepSaturationParams => mtrColorControlClusterStepSaturationParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrColorControlClusterStepSaturationParams =
  sendMessage mtrColorControlClusterStepSaturationParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRColorControlClusterStepSaturationParams mtrColorControlClusterStepSaturationParams, IsNSNumber value) => mtrColorControlClusterStepSaturationParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrColorControlClusterStepSaturationParams value =
  sendMessage mtrColorControlClusterStepSaturationParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRColorControlClusterStepSaturationParams mtrColorControlClusterStepSaturationParams => mtrColorControlClusterStepSaturationParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrColorControlClusterStepSaturationParams =
  sendMessage mtrColorControlClusterStepSaturationParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRColorControlClusterStepSaturationParams mtrColorControlClusterStepSaturationParams, IsNSNumber value) => mtrColorControlClusterStepSaturationParams -> value -> IO ()
setServerSideProcessingTimeout mtrColorControlClusterStepSaturationParams value =
  sendMessage mtrColorControlClusterStepSaturationParams setServerSideProcessingTimeoutSelector (toNSNumber value)

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

