{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRColorControlClusterEnhancedMoveToHueAndSaturationParams@.
module ObjC.Matter.MTRColorControlClusterEnhancedMoveToHueAndSaturationParams
  ( MTRColorControlClusterEnhancedMoveToHueAndSaturationParams
  , IsMTRColorControlClusterEnhancedMoveToHueAndSaturationParams(..)
  , enhancedHue
  , setEnhancedHue
  , saturation
  , setSaturation
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
  , enhancedHueSelector
  , optionsMaskSelector
  , optionsOverrideSelector
  , saturationSelector
  , serverSideProcessingTimeoutSelector
  , setEnhancedHueSelector
  , setOptionsMaskSelector
  , setOptionsOverrideSelector
  , setSaturationSelector
  , setServerSideProcessingTimeoutSelector
  , setTimedInvokeTimeoutMsSelector
  , setTransitionTimeSelector
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

-- | @- enhancedHue@
enhancedHue :: IsMTRColorControlClusterEnhancedMoveToHueAndSaturationParams mtrColorControlClusterEnhancedMoveToHueAndSaturationParams => mtrColorControlClusterEnhancedMoveToHueAndSaturationParams -> IO (Id NSNumber)
enhancedHue mtrColorControlClusterEnhancedMoveToHueAndSaturationParams =
  sendMessage mtrColorControlClusterEnhancedMoveToHueAndSaturationParams enhancedHueSelector

-- | @- setEnhancedHue:@
setEnhancedHue :: (IsMTRColorControlClusterEnhancedMoveToHueAndSaturationParams mtrColorControlClusterEnhancedMoveToHueAndSaturationParams, IsNSNumber value) => mtrColorControlClusterEnhancedMoveToHueAndSaturationParams -> value -> IO ()
setEnhancedHue mtrColorControlClusterEnhancedMoveToHueAndSaturationParams value =
  sendMessage mtrColorControlClusterEnhancedMoveToHueAndSaturationParams setEnhancedHueSelector (toNSNumber value)

-- | @- saturation@
saturation :: IsMTRColorControlClusterEnhancedMoveToHueAndSaturationParams mtrColorControlClusterEnhancedMoveToHueAndSaturationParams => mtrColorControlClusterEnhancedMoveToHueAndSaturationParams -> IO (Id NSNumber)
saturation mtrColorControlClusterEnhancedMoveToHueAndSaturationParams =
  sendMessage mtrColorControlClusterEnhancedMoveToHueAndSaturationParams saturationSelector

-- | @- setSaturation:@
setSaturation :: (IsMTRColorControlClusterEnhancedMoveToHueAndSaturationParams mtrColorControlClusterEnhancedMoveToHueAndSaturationParams, IsNSNumber value) => mtrColorControlClusterEnhancedMoveToHueAndSaturationParams -> value -> IO ()
setSaturation mtrColorControlClusterEnhancedMoveToHueAndSaturationParams value =
  sendMessage mtrColorControlClusterEnhancedMoveToHueAndSaturationParams setSaturationSelector (toNSNumber value)

-- | @- transitionTime@
transitionTime :: IsMTRColorControlClusterEnhancedMoveToHueAndSaturationParams mtrColorControlClusterEnhancedMoveToHueAndSaturationParams => mtrColorControlClusterEnhancedMoveToHueAndSaturationParams -> IO (Id NSNumber)
transitionTime mtrColorControlClusterEnhancedMoveToHueAndSaturationParams =
  sendMessage mtrColorControlClusterEnhancedMoveToHueAndSaturationParams transitionTimeSelector

-- | @- setTransitionTime:@
setTransitionTime :: (IsMTRColorControlClusterEnhancedMoveToHueAndSaturationParams mtrColorControlClusterEnhancedMoveToHueAndSaturationParams, IsNSNumber value) => mtrColorControlClusterEnhancedMoveToHueAndSaturationParams -> value -> IO ()
setTransitionTime mtrColorControlClusterEnhancedMoveToHueAndSaturationParams value =
  sendMessage mtrColorControlClusterEnhancedMoveToHueAndSaturationParams setTransitionTimeSelector (toNSNumber value)

-- | @- optionsMask@
optionsMask :: IsMTRColorControlClusterEnhancedMoveToHueAndSaturationParams mtrColorControlClusterEnhancedMoveToHueAndSaturationParams => mtrColorControlClusterEnhancedMoveToHueAndSaturationParams -> IO (Id NSNumber)
optionsMask mtrColorControlClusterEnhancedMoveToHueAndSaturationParams =
  sendMessage mtrColorControlClusterEnhancedMoveToHueAndSaturationParams optionsMaskSelector

-- | @- setOptionsMask:@
setOptionsMask :: (IsMTRColorControlClusterEnhancedMoveToHueAndSaturationParams mtrColorControlClusterEnhancedMoveToHueAndSaturationParams, IsNSNumber value) => mtrColorControlClusterEnhancedMoveToHueAndSaturationParams -> value -> IO ()
setOptionsMask mtrColorControlClusterEnhancedMoveToHueAndSaturationParams value =
  sendMessage mtrColorControlClusterEnhancedMoveToHueAndSaturationParams setOptionsMaskSelector (toNSNumber value)

-- | @- optionsOverride@
optionsOverride :: IsMTRColorControlClusterEnhancedMoveToHueAndSaturationParams mtrColorControlClusterEnhancedMoveToHueAndSaturationParams => mtrColorControlClusterEnhancedMoveToHueAndSaturationParams -> IO (Id NSNumber)
optionsOverride mtrColorControlClusterEnhancedMoveToHueAndSaturationParams =
  sendMessage mtrColorControlClusterEnhancedMoveToHueAndSaturationParams optionsOverrideSelector

-- | @- setOptionsOverride:@
setOptionsOverride :: (IsMTRColorControlClusterEnhancedMoveToHueAndSaturationParams mtrColorControlClusterEnhancedMoveToHueAndSaturationParams, IsNSNumber value) => mtrColorControlClusterEnhancedMoveToHueAndSaturationParams -> value -> IO ()
setOptionsOverride mtrColorControlClusterEnhancedMoveToHueAndSaturationParams value =
  sendMessage mtrColorControlClusterEnhancedMoveToHueAndSaturationParams setOptionsOverrideSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRColorControlClusterEnhancedMoveToHueAndSaturationParams mtrColorControlClusterEnhancedMoveToHueAndSaturationParams => mtrColorControlClusterEnhancedMoveToHueAndSaturationParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrColorControlClusterEnhancedMoveToHueAndSaturationParams =
  sendMessage mtrColorControlClusterEnhancedMoveToHueAndSaturationParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRColorControlClusterEnhancedMoveToHueAndSaturationParams mtrColorControlClusterEnhancedMoveToHueAndSaturationParams, IsNSNumber value) => mtrColorControlClusterEnhancedMoveToHueAndSaturationParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrColorControlClusterEnhancedMoveToHueAndSaturationParams value =
  sendMessage mtrColorControlClusterEnhancedMoveToHueAndSaturationParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRColorControlClusterEnhancedMoveToHueAndSaturationParams mtrColorControlClusterEnhancedMoveToHueAndSaturationParams => mtrColorControlClusterEnhancedMoveToHueAndSaturationParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrColorControlClusterEnhancedMoveToHueAndSaturationParams =
  sendMessage mtrColorControlClusterEnhancedMoveToHueAndSaturationParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRColorControlClusterEnhancedMoveToHueAndSaturationParams mtrColorControlClusterEnhancedMoveToHueAndSaturationParams, IsNSNumber value) => mtrColorControlClusterEnhancedMoveToHueAndSaturationParams -> value -> IO ()
setServerSideProcessingTimeout mtrColorControlClusterEnhancedMoveToHueAndSaturationParams value =
  sendMessage mtrColorControlClusterEnhancedMoveToHueAndSaturationParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @enhancedHue@
enhancedHueSelector :: Selector '[] (Id NSNumber)
enhancedHueSelector = mkSelector "enhancedHue"

-- | @Selector@ for @setEnhancedHue:@
setEnhancedHueSelector :: Selector '[Id NSNumber] ()
setEnhancedHueSelector = mkSelector "setEnhancedHue:"

-- | @Selector@ for @saturation@
saturationSelector :: Selector '[] (Id NSNumber)
saturationSelector = mkSelector "saturation"

-- | @Selector@ for @setSaturation:@
setSaturationSelector :: Selector '[Id NSNumber] ()
setSaturationSelector = mkSelector "setSaturation:"

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

