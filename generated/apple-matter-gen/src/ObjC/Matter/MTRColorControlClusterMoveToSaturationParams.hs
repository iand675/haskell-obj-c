{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRColorControlClusterMoveToSaturationParams@.
module ObjC.Matter.MTRColorControlClusterMoveToSaturationParams
  ( MTRColorControlClusterMoveToSaturationParams
  , IsMTRColorControlClusterMoveToSaturationParams(..)
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
  , optionsMaskSelector
  , optionsOverrideSelector
  , saturationSelector
  , serverSideProcessingTimeoutSelector
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

-- | @- saturation@
saturation :: IsMTRColorControlClusterMoveToSaturationParams mtrColorControlClusterMoveToSaturationParams => mtrColorControlClusterMoveToSaturationParams -> IO (Id NSNumber)
saturation mtrColorControlClusterMoveToSaturationParams =
  sendMessage mtrColorControlClusterMoveToSaturationParams saturationSelector

-- | @- setSaturation:@
setSaturation :: (IsMTRColorControlClusterMoveToSaturationParams mtrColorControlClusterMoveToSaturationParams, IsNSNumber value) => mtrColorControlClusterMoveToSaturationParams -> value -> IO ()
setSaturation mtrColorControlClusterMoveToSaturationParams value =
  sendMessage mtrColorControlClusterMoveToSaturationParams setSaturationSelector (toNSNumber value)

-- | @- transitionTime@
transitionTime :: IsMTRColorControlClusterMoveToSaturationParams mtrColorControlClusterMoveToSaturationParams => mtrColorControlClusterMoveToSaturationParams -> IO (Id NSNumber)
transitionTime mtrColorControlClusterMoveToSaturationParams =
  sendMessage mtrColorControlClusterMoveToSaturationParams transitionTimeSelector

-- | @- setTransitionTime:@
setTransitionTime :: (IsMTRColorControlClusterMoveToSaturationParams mtrColorControlClusterMoveToSaturationParams, IsNSNumber value) => mtrColorControlClusterMoveToSaturationParams -> value -> IO ()
setTransitionTime mtrColorControlClusterMoveToSaturationParams value =
  sendMessage mtrColorControlClusterMoveToSaturationParams setTransitionTimeSelector (toNSNumber value)

-- | @- optionsMask@
optionsMask :: IsMTRColorControlClusterMoveToSaturationParams mtrColorControlClusterMoveToSaturationParams => mtrColorControlClusterMoveToSaturationParams -> IO (Id NSNumber)
optionsMask mtrColorControlClusterMoveToSaturationParams =
  sendMessage mtrColorControlClusterMoveToSaturationParams optionsMaskSelector

-- | @- setOptionsMask:@
setOptionsMask :: (IsMTRColorControlClusterMoveToSaturationParams mtrColorControlClusterMoveToSaturationParams, IsNSNumber value) => mtrColorControlClusterMoveToSaturationParams -> value -> IO ()
setOptionsMask mtrColorControlClusterMoveToSaturationParams value =
  sendMessage mtrColorControlClusterMoveToSaturationParams setOptionsMaskSelector (toNSNumber value)

-- | @- optionsOverride@
optionsOverride :: IsMTRColorControlClusterMoveToSaturationParams mtrColorControlClusterMoveToSaturationParams => mtrColorControlClusterMoveToSaturationParams -> IO (Id NSNumber)
optionsOverride mtrColorControlClusterMoveToSaturationParams =
  sendMessage mtrColorControlClusterMoveToSaturationParams optionsOverrideSelector

-- | @- setOptionsOverride:@
setOptionsOverride :: (IsMTRColorControlClusterMoveToSaturationParams mtrColorControlClusterMoveToSaturationParams, IsNSNumber value) => mtrColorControlClusterMoveToSaturationParams -> value -> IO ()
setOptionsOverride mtrColorControlClusterMoveToSaturationParams value =
  sendMessage mtrColorControlClusterMoveToSaturationParams setOptionsOverrideSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRColorControlClusterMoveToSaturationParams mtrColorControlClusterMoveToSaturationParams => mtrColorControlClusterMoveToSaturationParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrColorControlClusterMoveToSaturationParams =
  sendMessage mtrColorControlClusterMoveToSaturationParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRColorControlClusterMoveToSaturationParams mtrColorControlClusterMoveToSaturationParams, IsNSNumber value) => mtrColorControlClusterMoveToSaturationParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrColorControlClusterMoveToSaturationParams value =
  sendMessage mtrColorControlClusterMoveToSaturationParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRColorControlClusterMoveToSaturationParams mtrColorControlClusterMoveToSaturationParams => mtrColorControlClusterMoveToSaturationParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrColorControlClusterMoveToSaturationParams =
  sendMessage mtrColorControlClusterMoveToSaturationParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRColorControlClusterMoveToSaturationParams mtrColorControlClusterMoveToSaturationParams, IsNSNumber value) => mtrColorControlClusterMoveToSaturationParams -> value -> IO ()
setServerSideProcessingTimeout mtrColorControlClusterMoveToSaturationParams value =
  sendMessage mtrColorControlClusterMoveToSaturationParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

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

