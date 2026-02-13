{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRColorControlClusterMoveToColorTemperatureParams@.
module ObjC.Matter.MTRColorControlClusterMoveToColorTemperatureParams
  ( MTRColorControlClusterMoveToColorTemperatureParams
  , IsMTRColorControlClusterMoveToColorTemperatureParams(..)
  , colorTemperatureMireds
  , setColorTemperatureMireds
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
  , colorTemperature
  , setColorTemperature
  , colorTemperatureMiredsSelector
  , colorTemperatureSelector
  , optionsMaskSelector
  , optionsOverrideSelector
  , serverSideProcessingTimeoutSelector
  , setColorTemperatureMiredsSelector
  , setColorTemperatureSelector
  , setOptionsMaskSelector
  , setOptionsOverrideSelector
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

-- | @- colorTemperatureMireds@
colorTemperatureMireds :: IsMTRColorControlClusterMoveToColorTemperatureParams mtrColorControlClusterMoveToColorTemperatureParams => mtrColorControlClusterMoveToColorTemperatureParams -> IO (Id NSNumber)
colorTemperatureMireds mtrColorControlClusterMoveToColorTemperatureParams =
  sendMessage mtrColorControlClusterMoveToColorTemperatureParams colorTemperatureMiredsSelector

-- | @- setColorTemperatureMireds:@
setColorTemperatureMireds :: (IsMTRColorControlClusterMoveToColorTemperatureParams mtrColorControlClusterMoveToColorTemperatureParams, IsNSNumber value) => mtrColorControlClusterMoveToColorTemperatureParams -> value -> IO ()
setColorTemperatureMireds mtrColorControlClusterMoveToColorTemperatureParams value =
  sendMessage mtrColorControlClusterMoveToColorTemperatureParams setColorTemperatureMiredsSelector (toNSNumber value)

-- | @- transitionTime@
transitionTime :: IsMTRColorControlClusterMoveToColorTemperatureParams mtrColorControlClusterMoveToColorTemperatureParams => mtrColorControlClusterMoveToColorTemperatureParams -> IO (Id NSNumber)
transitionTime mtrColorControlClusterMoveToColorTemperatureParams =
  sendMessage mtrColorControlClusterMoveToColorTemperatureParams transitionTimeSelector

-- | @- setTransitionTime:@
setTransitionTime :: (IsMTRColorControlClusterMoveToColorTemperatureParams mtrColorControlClusterMoveToColorTemperatureParams, IsNSNumber value) => mtrColorControlClusterMoveToColorTemperatureParams -> value -> IO ()
setTransitionTime mtrColorControlClusterMoveToColorTemperatureParams value =
  sendMessage mtrColorControlClusterMoveToColorTemperatureParams setTransitionTimeSelector (toNSNumber value)

-- | @- optionsMask@
optionsMask :: IsMTRColorControlClusterMoveToColorTemperatureParams mtrColorControlClusterMoveToColorTemperatureParams => mtrColorControlClusterMoveToColorTemperatureParams -> IO (Id NSNumber)
optionsMask mtrColorControlClusterMoveToColorTemperatureParams =
  sendMessage mtrColorControlClusterMoveToColorTemperatureParams optionsMaskSelector

-- | @- setOptionsMask:@
setOptionsMask :: (IsMTRColorControlClusterMoveToColorTemperatureParams mtrColorControlClusterMoveToColorTemperatureParams, IsNSNumber value) => mtrColorControlClusterMoveToColorTemperatureParams -> value -> IO ()
setOptionsMask mtrColorControlClusterMoveToColorTemperatureParams value =
  sendMessage mtrColorControlClusterMoveToColorTemperatureParams setOptionsMaskSelector (toNSNumber value)

-- | @- optionsOverride@
optionsOverride :: IsMTRColorControlClusterMoveToColorTemperatureParams mtrColorControlClusterMoveToColorTemperatureParams => mtrColorControlClusterMoveToColorTemperatureParams -> IO (Id NSNumber)
optionsOverride mtrColorControlClusterMoveToColorTemperatureParams =
  sendMessage mtrColorControlClusterMoveToColorTemperatureParams optionsOverrideSelector

-- | @- setOptionsOverride:@
setOptionsOverride :: (IsMTRColorControlClusterMoveToColorTemperatureParams mtrColorControlClusterMoveToColorTemperatureParams, IsNSNumber value) => mtrColorControlClusterMoveToColorTemperatureParams -> value -> IO ()
setOptionsOverride mtrColorControlClusterMoveToColorTemperatureParams value =
  sendMessage mtrColorControlClusterMoveToColorTemperatureParams setOptionsOverrideSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRColorControlClusterMoveToColorTemperatureParams mtrColorControlClusterMoveToColorTemperatureParams => mtrColorControlClusterMoveToColorTemperatureParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrColorControlClusterMoveToColorTemperatureParams =
  sendMessage mtrColorControlClusterMoveToColorTemperatureParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRColorControlClusterMoveToColorTemperatureParams mtrColorControlClusterMoveToColorTemperatureParams, IsNSNumber value) => mtrColorControlClusterMoveToColorTemperatureParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrColorControlClusterMoveToColorTemperatureParams value =
  sendMessage mtrColorControlClusterMoveToColorTemperatureParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRColorControlClusterMoveToColorTemperatureParams mtrColorControlClusterMoveToColorTemperatureParams => mtrColorControlClusterMoveToColorTemperatureParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrColorControlClusterMoveToColorTemperatureParams =
  sendMessage mtrColorControlClusterMoveToColorTemperatureParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRColorControlClusterMoveToColorTemperatureParams mtrColorControlClusterMoveToColorTemperatureParams, IsNSNumber value) => mtrColorControlClusterMoveToColorTemperatureParams -> value -> IO ()
setServerSideProcessingTimeout mtrColorControlClusterMoveToColorTemperatureParams value =
  sendMessage mtrColorControlClusterMoveToColorTemperatureParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- | @- colorTemperature@
colorTemperature :: IsMTRColorControlClusterMoveToColorTemperatureParams mtrColorControlClusterMoveToColorTemperatureParams => mtrColorControlClusterMoveToColorTemperatureParams -> IO (Id NSNumber)
colorTemperature mtrColorControlClusterMoveToColorTemperatureParams =
  sendMessage mtrColorControlClusterMoveToColorTemperatureParams colorTemperatureSelector

-- | @- setColorTemperature:@
setColorTemperature :: (IsMTRColorControlClusterMoveToColorTemperatureParams mtrColorControlClusterMoveToColorTemperatureParams, IsNSNumber value) => mtrColorControlClusterMoveToColorTemperatureParams -> value -> IO ()
setColorTemperature mtrColorControlClusterMoveToColorTemperatureParams value =
  sendMessage mtrColorControlClusterMoveToColorTemperatureParams setColorTemperatureSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @colorTemperatureMireds@
colorTemperatureMiredsSelector :: Selector '[] (Id NSNumber)
colorTemperatureMiredsSelector = mkSelector "colorTemperatureMireds"

-- | @Selector@ for @setColorTemperatureMireds:@
setColorTemperatureMiredsSelector :: Selector '[Id NSNumber] ()
setColorTemperatureMiredsSelector = mkSelector "setColorTemperatureMireds:"

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

-- | @Selector@ for @colorTemperature@
colorTemperatureSelector :: Selector '[] (Id NSNumber)
colorTemperatureSelector = mkSelector "colorTemperature"

-- | @Selector@ for @setColorTemperature:@
setColorTemperatureSelector :: Selector '[Id NSNumber] ()
setColorTemperatureSelector = mkSelector "setColorTemperature:"

