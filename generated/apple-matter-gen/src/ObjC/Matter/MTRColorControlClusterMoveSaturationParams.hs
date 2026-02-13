{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRColorControlClusterMoveSaturationParams@.
module ObjC.Matter.MTRColorControlClusterMoveSaturationParams
  ( MTRColorControlClusterMoveSaturationParams
  , IsMTRColorControlClusterMoveSaturationParams(..)
  , moveMode
  , setMoveMode
  , rate
  , setRate
  , optionsMask
  , setOptionsMask
  , optionsOverride
  , setOptionsOverride
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , moveModeSelector
  , optionsMaskSelector
  , optionsOverrideSelector
  , rateSelector
  , serverSideProcessingTimeoutSelector
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
moveMode :: IsMTRColorControlClusterMoveSaturationParams mtrColorControlClusterMoveSaturationParams => mtrColorControlClusterMoveSaturationParams -> IO (Id NSNumber)
moveMode mtrColorControlClusterMoveSaturationParams =
  sendMessage mtrColorControlClusterMoveSaturationParams moveModeSelector

-- | @- setMoveMode:@
setMoveMode :: (IsMTRColorControlClusterMoveSaturationParams mtrColorControlClusterMoveSaturationParams, IsNSNumber value) => mtrColorControlClusterMoveSaturationParams -> value -> IO ()
setMoveMode mtrColorControlClusterMoveSaturationParams value =
  sendMessage mtrColorControlClusterMoveSaturationParams setMoveModeSelector (toNSNumber value)

-- | @- rate@
rate :: IsMTRColorControlClusterMoveSaturationParams mtrColorControlClusterMoveSaturationParams => mtrColorControlClusterMoveSaturationParams -> IO (Id NSNumber)
rate mtrColorControlClusterMoveSaturationParams =
  sendMessage mtrColorControlClusterMoveSaturationParams rateSelector

-- | @- setRate:@
setRate :: (IsMTRColorControlClusterMoveSaturationParams mtrColorControlClusterMoveSaturationParams, IsNSNumber value) => mtrColorControlClusterMoveSaturationParams -> value -> IO ()
setRate mtrColorControlClusterMoveSaturationParams value =
  sendMessage mtrColorControlClusterMoveSaturationParams setRateSelector (toNSNumber value)

-- | @- optionsMask@
optionsMask :: IsMTRColorControlClusterMoveSaturationParams mtrColorControlClusterMoveSaturationParams => mtrColorControlClusterMoveSaturationParams -> IO (Id NSNumber)
optionsMask mtrColorControlClusterMoveSaturationParams =
  sendMessage mtrColorControlClusterMoveSaturationParams optionsMaskSelector

-- | @- setOptionsMask:@
setOptionsMask :: (IsMTRColorControlClusterMoveSaturationParams mtrColorControlClusterMoveSaturationParams, IsNSNumber value) => mtrColorControlClusterMoveSaturationParams -> value -> IO ()
setOptionsMask mtrColorControlClusterMoveSaturationParams value =
  sendMessage mtrColorControlClusterMoveSaturationParams setOptionsMaskSelector (toNSNumber value)

-- | @- optionsOverride@
optionsOverride :: IsMTRColorControlClusterMoveSaturationParams mtrColorControlClusterMoveSaturationParams => mtrColorControlClusterMoveSaturationParams -> IO (Id NSNumber)
optionsOverride mtrColorControlClusterMoveSaturationParams =
  sendMessage mtrColorControlClusterMoveSaturationParams optionsOverrideSelector

-- | @- setOptionsOverride:@
setOptionsOverride :: (IsMTRColorControlClusterMoveSaturationParams mtrColorControlClusterMoveSaturationParams, IsNSNumber value) => mtrColorControlClusterMoveSaturationParams -> value -> IO ()
setOptionsOverride mtrColorControlClusterMoveSaturationParams value =
  sendMessage mtrColorControlClusterMoveSaturationParams setOptionsOverrideSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRColorControlClusterMoveSaturationParams mtrColorControlClusterMoveSaturationParams => mtrColorControlClusterMoveSaturationParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrColorControlClusterMoveSaturationParams =
  sendMessage mtrColorControlClusterMoveSaturationParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRColorControlClusterMoveSaturationParams mtrColorControlClusterMoveSaturationParams, IsNSNumber value) => mtrColorControlClusterMoveSaturationParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrColorControlClusterMoveSaturationParams value =
  sendMessage mtrColorControlClusterMoveSaturationParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRColorControlClusterMoveSaturationParams mtrColorControlClusterMoveSaturationParams => mtrColorControlClusterMoveSaturationParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrColorControlClusterMoveSaturationParams =
  sendMessage mtrColorControlClusterMoveSaturationParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRColorControlClusterMoveSaturationParams mtrColorControlClusterMoveSaturationParams, IsNSNumber value) => mtrColorControlClusterMoveSaturationParams -> value -> IO ()
setServerSideProcessingTimeout mtrColorControlClusterMoveSaturationParams value =
  sendMessage mtrColorControlClusterMoveSaturationParams setServerSideProcessingTimeoutSelector (toNSNumber value)

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

