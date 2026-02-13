{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRLevelControlClusterStepParams@.
module ObjC.Matter.MTRLevelControlClusterStepParams
  ( MTRLevelControlClusterStepParams
  , IsMTRLevelControlClusterStepParams(..)
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
stepMode :: IsMTRLevelControlClusterStepParams mtrLevelControlClusterStepParams => mtrLevelControlClusterStepParams -> IO (Id NSNumber)
stepMode mtrLevelControlClusterStepParams =
  sendMessage mtrLevelControlClusterStepParams stepModeSelector

-- | @- setStepMode:@
setStepMode :: (IsMTRLevelControlClusterStepParams mtrLevelControlClusterStepParams, IsNSNumber value) => mtrLevelControlClusterStepParams -> value -> IO ()
setStepMode mtrLevelControlClusterStepParams value =
  sendMessage mtrLevelControlClusterStepParams setStepModeSelector (toNSNumber value)

-- | @- stepSize@
stepSize :: IsMTRLevelControlClusterStepParams mtrLevelControlClusterStepParams => mtrLevelControlClusterStepParams -> IO (Id NSNumber)
stepSize mtrLevelControlClusterStepParams =
  sendMessage mtrLevelControlClusterStepParams stepSizeSelector

-- | @- setStepSize:@
setStepSize :: (IsMTRLevelControlClusterStepParams mtrLevelControlClusterStepParams, IsNSNumber value) => mtrLevelControlClusterStepParams -> value -> IO ()
setStepSize mtrLevelControlClusterStepParams value =
  sendMessage mtrLevelControlClusterStepParams setStepSizeSelector (toNSNumber value)

-- | @- transitionTime@
transitionTime :: IsMTRLevelControlClusterStepParams mtrLevelControlClusterStepParams => mtrLevelControlClusterStepParams -> IO (Id NSNumber)
transitionTime mtrLevelControlClusterStepParams =
  sendMessage mtrLevelControlClusterStepParams transitionTimeSelector

-- | @- setTransitionTime:@
setTransitionTime :: (IsMTRLevelControlClusterStepParams mtrLevelControlClusterStepParams, IsNSNumber value) => mtrLevelControlClusterStepParams -> value -> IO ()
setTransitionTime mtrLevelControlClusterStepParams value =
  sendMessage mtrLevelControlClusterStepParams setTransitionTimeSelector (toNSNumber value)

-- | @- optionsMask@
optionsMask :: IsMTRLevelControlClusterStepParams mtrLevelControlClusterStepParams => mtrLevelControlClusterStepParams -> IO (Id NSNumber)
optionsMask mtrLevelControlClusterStepParams =
  sendMessage mtrLevelControlClusterStepParams optionsMaskSelector

-- | @- setOptionsMask:@
setOptionsMask :: (IsMTRLevelControlClusterStepParams mtrLevelControlClusterStepParams, IsNSNumber value) => mtrLevelControlClusterStepParams -> value -> IO ()
setOptionsMask mtrLevelControlClusterStepParams value =
  sendMessage mtrLevelControlClusterStepParams setOptionsMaskSelector (toNSNumber value)

-- | @- optionsOverride@
optionsOverride :: IsMTRLevelControlClusterStepParams mtrLevelControlClusterStepParams => mtrLevelControlClusterStepParams -> IO (Id NSNumber)
optionsOverride mtrLevelControlClusterStepParams =
  sendMessage mtrLevelControlClusterStepParams optionsOverrideSelector

-- | @- setOptionsOverride:@
setOptionsOverride :: (IsMTRLevelControlClusterStepParams mtrLevelControlClusterStepParams, IsNSNumber value) => mtrLevelControlClusterStepParams -> value -> IO ()
setOptionsOverride mtrLevelControlClusterStepParams value =
  sendMessage mtrLevelControlClusterStepParams setOptionsOverrideSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRLevelControlClusterStepParams mtrLevelControlClusterStepParams => mtrLevelControlClusterStepParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrLevelControlClusterStepParams =
  sendMessage mtrLevelControlClusterStepParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRLevelControlClusterStepParams mtrLevelControlClusterStepParams, IsNSNumber value) => mtrLevelControlClusterStepParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrLevelControlClusterStepParams value =
  sendMessage mtrLevelControlClusterStepParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRLevelControlClusterStepParams mtrLevelControlClusterStepParams => mtrLevelControlClusterStepParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrLevelControlClusterStepParams =
  sendMessage mtrLevelControlClusterStepParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRLevelControlClusterStepParams mtrLevelControlClusterStepParams, IsNSNumber value) => mtrLevelControlClusterStepParams -> value -> IO ()
setServerSideProcessingTimeout mtrLevelControlClusterStepParams value =
  sendMessage mtrLevelControlClusterStepParams setServerSideProcessingTimeoutSelector (toNSNumber value)

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

