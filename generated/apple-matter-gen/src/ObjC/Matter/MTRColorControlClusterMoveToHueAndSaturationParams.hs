{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRColorControlClusterMoveToHueAndSaturationParams@.
module ObjC.Matter.MTRColorControlClusterMoveToHueAndSaturationParams
  ( MTRColorControlClusterMoveToHueAndSaturationParams
  , IsMTRColorControlClusterMoveToHueAndSaturationParams(..)
  , hue
  , setHue
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
  , hueSelector
  , optionsMaskSelector
  , optionsOverrideSelector
  , saturationSelector
  , serverSideProcessingTimeoutSelector
  , setHueSelector
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

-- | @- hue@
hue :: IsMTRColorControlClusterMoveToHueAndSaturationParams mtrColorControlClusterMoveToHueAndSaturationParams => mtrColorControlClusterMoveToHueAndSaturationParams -> IO (Id NSNumber)
hue mtrColorControlClusterMoveToHueAndSaturationParams =
  sendMessage mtrColorControlClusterMoveToHueAndSaturationParams hueSelector

-- | @- setHue:@
setHue :: (IsMTRColorControlClusterMoveToHueAndSaturationParams mtrColorControlClusterMoveToHueAndSaturationParams, IsNSNumber value) => mtrColorControlClusterMoveToHueAndSaturationParams -> value -> IO ()
setHue mtrColorControlClusterMoveToHueAndSaturationParams value =
  sendMessage mtrColorControlClusterMoveToHueAndSaturationParams setHueSelector (toNSNumber value)

-- | @- saturation@
saturation :: IsMTRColorControlClusterMoveToHueAndSaturationParams mtrColorControlClusterMoveToHueAndSaturationParams => mtrColorControlClusterMoveToHueAndSaturationParams -> IO (Id NSNumber)
saturation mtrColorControlClusterMoveToHueAndSaturationParams =
  sendMessage mtrColorControlClusterMoveToHueAndSaturationParams saturationSelector

-- | @- setSaturation:@
setSaturation :: (IsMTRColorControlClusterMoveToHueAndSaturationParams mtrColorControlClusterMoveToHueAndSaturationParams, IsNSNumber value) => mtrColorControlClusterMoveToHueAndSaturationParams -> value -> IO ()
setSaturation mtrColorControlClusterMoveToHueAndSaturationParams value =
  sendMessage mtrColorControlClusterMoveToHueAndSaturationParams setSaturationSelector (toNSNumber value)

-- | @- transitionTime@
transitionTime :: IsMTRColorControlClusterMoveToHueAndSaturationParams mtrColorControlClusterMoveToHueAndSaturationParams => mtrColorControlClusterMoveToHueAndSaturationParams -> IO (Id NSNumber)
transitionTime mtrColorControlClusterMoveToHueAndSaturationParams =
  sendMessage mtrColorControlClusterMoveToHueAndSaturationParams transitionTimeSelector

-- | @- setTransitionTime:@
setTransitionTime :: (IsMTRColorControlClusterMoveToHueAndSaturationParams mtrColorControlClusterMoveToHueAndSaturationParams, IsNSNumber value) => mtrColorControlClusterMoveToHueAndSaturationParams -> value -> IO ()
setTransitionTime mtrColorControlClusterMoveToHueAndSaturationParams value =
  sendMessage mtrColorControlClusterMoveToHueAndSaturationParams setTransitionTimeSelector (toNSNumber value)

-- | @- optionsMask@
optionsMask :: IsMTRColorControlClusterMoveToHueAndSaturationParams mtrColorControlClusterMoveToHueAndSaturationParams => mtrColorControlClusterMoveToHueAndSaturationParams -> IO (Id NSNumber)
optionsMask mtrColorControlClusterMoveToHueAndSaturationParams =
  sendMessage mtrColorControlClusterMoveToHueAndSaturationParams optionsMaskSelector

-- | @- setOptionsMask:@
setOptionsMask :: (IsMTRColorControlClusterMoveToHueAndSaturationParams mtrColorControlClusterMoveToHueAndSaturationParams, IsNSNumber value) => mtrColorControlClusterMoveToHueAndSaturationParams -> value -> IO ()
setOptionsMask mtrColorControlClusterMoveToHueAndSaturationParams value =
  sendMessage mtrColorControlClusterMoveToHueAndSaturationParams setOptionsMaskSelector (toNSNumber value)

-- | @- optionsOverride@
optionsOverride :: IsMTRColorControlClusterMoveToHueAndSaturationParams mtrColorControlClusterMoveToHueAndSaturationParams => mtrColorControlClusterMoveToHueAndSaturationParams -> IO (Id NSNumber)
optionsOverride mtrColorControlClusterMoveToHueAndSaturationParams =
  sendMessage mtrColorControlClusterMoveToHueAndSaturationParams optionsOverrideSelector

-- | @- setOptionsOverride:@
setOptionsOverride :: (IsMTRColorControlClusterMoveToHueAndSaturationParams mtrColorControlClusterMoveToHueAndSaturationParams, IsNSNumber value) => mtrColorControlClusterMoveToHueAndSaturationParams -> value -> IO ()
setOptionsOverride mtrColorControlClusterMoveToHueAndSaturationParams value =
  sendMessage mtrColorControlClusterMoveToHueAndSaturationParams setOptionsOverrideSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRColorControlClusterMoveToHueAndSaturationParams mtrColorControlClusterMoveToHueAndSaturationParams => mtrColorControlClusterMoveToHueAndSaturationParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrColorControlClusterMoveToHueAndSaturationParams =
  sendMessage mtrColorControlClusterMoveToHueAndSaturationParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRColorControlClusterMoveToHueAndSaturationParams mtrColorControlClusterMoveToHueAndSaturationParams, IsNSNumber value) => mtrColorControlClusterMoveToHueAndSaturationParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrColorControlClusterMoveToHueAndSaturationParams value =
  sendMessage mtrColorControlClusterMoveToHueAndSaturationParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRColorControlClusterMoveToHueAndSaturationParams mtrColorControlClusterMoveToHueAndSaturationParams => mtrColorControlClusterMoveToHueAndSaturationParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrColorControlClusterMoveToHueAndSaturationParams =
  sendMessage mtrColorControlClusterMoveToHueAndSaturationParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRColorControlClusterMoveToHueAndSaturationParams mtrColorControlClusterMoveToHueAndSaturationParams, IsNSNumber value) => mtrColorControlClusterMoveToHueAndSaturationParams -> value -> IO ()
setServerSideProcessingTimeout mtrColorControlClusterMoveToHueAndSaturationParams value =
  sendMessage mtrColorControlClusterMoveToHueAndSaturationParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @hue@
hueSelector :: Selector '[] (Id NSNumber)
hueSelector = mkSelector "hue"

-- | @Selector@ for @setHue:@
setHueSelector :: Selector '[Id NSNumber] ()
setHueSelector = mkSelector "setHue:"

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

