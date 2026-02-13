{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRColorControlClusterMoveColorTemperatureParams@.
module ObjC.Matter.MTRColorControlClusterMoveColorTemperatureParams
  ( MTRColorControlClusterMoveColorTemperatureParams
  , IsMTRColorControlClusterMoveColorTemperatureParams(..)
  , moveMode
  , setMoveMode
  , rate
  , setRate
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
  , moveModeSelector
  , optionsMaskSelector
  , optionsOverrideSelector
  , rateSelector
  , serverSideProcessingTimeoutSelector
  , setColorTemperatureMaximumMiredsSelector
  , setColorTemperatureMinimumMiredsSelector
  , setMoveModeSelector
  , setOptionsMaskSelector
  , setOptionsOverrideSelector
  , setRateSelector
  , setServerSideProcessingTimeoutSelector
  , setTimedInvokeTimeoutMsSelector
  , timedInvokeTimeoutMsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- moveMode@
moveMode :: IsMTRColorControlClusterMoveColorTemperatureParams mtrColorControlClusterMoveColorTemperatureParams => mtrColorControlClusterMoveColorTemperatureParams -> IO (Id NSNumber)
moveMode mtrColorControlClusterMoveColorTemperatureParams =
  sendMessage mtrColorControlClusterMoveColorTemperatureParams moveModeSelector

-- | @- setMoveMode:@
setMoveMode :: (IsMTRColorControlClusterMoveColorTemperatureParams mtrColorControlClusterMoveColorTemperatureParams, IsNSNumber value) => mtrColorControlClusterMoveColorTemperatureParams -> value -> IO ()
setMoveMode mtrColorControlClusterMoveColorTemperatureParams value =
  sendMessage mtrColorControlClusterMoveColorTemperatureParams setMoveModeSelector (toNSNumber value)

-- | @- rate@
rate :: IsMTRColorControlClusterMoveColorTemperatureParams mtrColorControlClusterMoveColorTemperatureParams => mtrColorControlClusterMoveColorTemperatureParams -> IO (Id NSNumber)
rate mtrColorControlClusterMoveColorTemperatureParams =
  sendMessage mtrColorControlClusterMoveColorTemperatureParams rateSelector

-- | @- setRate:@
setRate :: (IsMTRColorControlClusterMoveColorTemperatureParams mtrColorControlClusterMoveColorTemperatureParams, IsNSNumber value) => mtrColorControlClusterMoveColorTemperatureParams -> value -> IO ()
setRate mtrColorControlClusterMoveColorTemperatureParams value =
  sendMessage mtrColorControlClusterMoveColorTemperatureParams setRateSelector (toNSNumber value)

-- | @- colorTemperatureMinimumMireds@
colorTemperatureMinimumMireds :: IsMTRColorControlClusterMoveColorTemperatureParams mtrColorControlClusterMoveColorTemperatureParams => mtrColorControlClusterMoveColorTemperatureParams -> IO (Id NSNumber)
colorTemperatureMinimumMireds mtrColorControlClusterMoveColorTemperatureParams =
  sendMessage mtrColorControlClusterMoveColorTemperatureParams colorTemperatureMinimumMiredsSelector

-- | @- setColorTemperatureMinimumMireds:@
setColorTemperatureMinimumMireds :: (IsMTRColorControlClusterMoveColorTemperatureParams mtrColorControlClusterMoveColorTemperatureParams, IsNSNumber value) => mtrColorControlClusterMoveColorTemperatureParams -> value -> IO ()
setColorTemperatureMinimumMireds mtrColorControlClusterMoveColorTemperatureParams value =
  sendMessage mtrColorControlClusterMoveColorTemperatureParams setColorTemperatureMinimumMiredsSelector (toNSNumber value)

-- | @- colorTemperatureMaximumMireds@
colorTemperatureMaximumMireds :: IsMTRColorControlClusterMoveColorTemperatureParams mtrColorControlClusterMoveColorTemperatureParams => mtrColorControlClusterMoveColorTemperatureParams -> IO (Id NSNumber)
colorTemperatureMaximumMireds mtrColorControlClusterMoveColorTemperatureParams =
  sendMessage mtrColorControlClusterMoveColorTemperatureParams colorTemperatureMaximumMiredsSelector

-- | @- setColorTemperatureMaximumMireds:@
setColorTemperatureMaximumMireds :: (IsMTRColorControlClusterMoveColorTemperatureParams mtrColorControlClusterMoveColorTemperatureParams, IsNSNumber value) => mtrColorControlClusterMoveColorTemperatureParams -> value -> IO ()
setColorTemperatureMaximumMireds mtrColorControlClusterMoveColorTemperatureParams value =
  sendMessage mtrColorControlClusterMoveColorTemperatureParams setColorTemperatureMaximumMiredsSelector (toNSNumber value)

-- | @- optionsMask@
optionsMask :: IsMTRColorControlClusterMoveColorTemperatureParams mtrColorControlClusterMoveColorTemperatureParams => mtrColorControlClusterMoveColorTemperatureParams -> IO (Id NSNumber)
optionsMask mtrColorControlClusterMoveColorTemperatureParams =
  sendMessage mtrColorControlClusterMoveColorTemperatureParams optionsMaskSelector

-- | @- setOptionsMask:@
setOptionsMask :: (IsMTRColorControlClusterMoveColorTemperatureParams mtrColorControlClusterMoveColorTemperatureParams, IsNSNumber value) => mtrColorControlClusterMoveColorTemperatureParams -> value -> IO ()
setOptionsMask mtrColorControlClusterMoveColorTemperatureParams value =
  sendMessage mtrColorControlClusterMoveColorTemperatureParams setOptionsMaskSelector (toNSNumber value)

-- | @- optionsOverride@
optionsOverride :: IsMTRColorControlClusterMoveColorTemperatureParams mtrColorControlClusterMoveColorTemperatureParams => mtrColorControlClusterMoveColorTemperatureParams -> IO (Id NSNumber)
optionsOverride mtrColorControlClusterMoveColorTemperatureParams =
  sendMessage mtrColorControlClusterMoveColorTemperatureParams optionsOverrideSelector

-- | @- setOptionsOverride:@
setOptionsOverride :: (IsMTRColorControlClusterMoveColorTemperatureParams mtrColorControlClusterMoveColorTemperatureParams, IsNSNumber value) => mtrColorControlClusterMoveColorTemperatureParams -> value -> IO ()
setOptionsOverride mtrColorControlClusterMoveColorTemperatureParams value =
  sendMessage mtrColorControlClusterMoveColorTemperatureParams setOptionsOverrideSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRColorControlClusterMoveColorTemperatureParams mtrColorControlClusterMoveColorTemperatureParams => mtrColorControlClusterMoveColorTemperatureParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrColorControlClusterMoveColorTemperatureParams =
  sendMessage mtrColorControlClusterMoveColorTemperatureParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRColorControlClusterMoveColorTemperatureParams mtrColorControlClusterMoveColorTemperatureParams, IsNSNumber value) => mtrColorControlClusterMoveColorTemperatureParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrColorControlClusterMoveColorTemperatureParams value =
  sendMessage mtrColorControlClusterMoveColorTemperatureParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRColorControlClusterMoveColorTemperatureParams mtrColorControlClusterMoveColorTemperatureParams => mtrColorControlClusterMoveColorTemperatureParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrColorControlClusterMoveColorTemperatureParams =
  sendMessage mtrColorControlClusterMoveColorTemperatureParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRColorControlClusterMoveColorTemperatureParams mtrColorControlClusterMoveColorTemperatureParams, IsNSNumber value) => mtrColorControlClusterMoveColorTemperatureParams -> value -> IO ()
setServerSideProcessingTimeout mtrColorControlClusterMoveColorTemperatureParams value =
  sendMessage mtrColorControlClusterMoveColorTemperatureParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @moveMode@
moveModeSelector :: Selector '[] (Id NSNumber)
moveModeSelector = mkSelector "moveMode"

-- | @Selector@ for @setMoveMode:@
setMoveModeSelector :: Selector '[Id NSNumber] ()
setMoveModeSelector = mkSelector "setMoveMode:"

-- | @Selector@ for @rate@
rateSelector :: Selector '[] (Id NSNumber)
rateSelector = mkSelector "rate"

-- | @Selector@ for @setRate:@
setRateSelector :: Selector '[Id NSNumber] ()
setRateSelector = mkSelector "setRate:"

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

