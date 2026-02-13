{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTemperatureControlClusterSetTemperatureParams@.
module ObjC.Matter.MTRTemperatureControlClusterSetTemperatureParams
  ( MTRTemperatureControlClusterSetTemperatureParams
  , IsMTRTemperatureControlClusterSetTemperatureParams(..)
  , targetTemperature
  , setTargetTemperature
  , targetTemperatureLevel
  , setTargetTemperatureLevel
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , serverSideProcessingTimeoutSelector
  , setServerSideProcessingTimeoutSelector
  , setTargetTemperatureLevelSelector
  , setTargetTemperatureSelector
  , setTimedInvokeTimeoutMsSelector
  , targetTemperatureLevelSelector
  , targetTemperatureSelector
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

-- | @- targetTemperature@
targetTemperature :: IsMTRTemperatureControlClusterSetTemperatureParams mtrTemperatureControlClusterSetTemperatureParams => mtrTemperatureControlClusterSetTemperatureParams -> IO (Id NSNumber)
targetTemperature mtrTemperatureControlClusterSetTemperatureParams =
  sendMessage mtrTemperatureControlClusterSetTemperatureParams targetTemperatureSelector

-- | @- setTargetTemperature:@
setTargetTemperature :: (IsMTRTemperatureControlClusterSetTemperatureParams mtrTemperatureControlClusterSetTemperatureParams, IsNSNumber value) => mtrTemperatureControlClusterSetTemperatureParams -> value -> IO ()
setTargetTemperature mtrTemperatureControlClusterSetTemperatureParams value =
  sendMessage mtrTemperatureControlClusterSetTemperatureParams setTargetTemperatureSelector (toNSNumber value)

-- | @- targetTemperatureLevel@
targetTemperatureLevel :: IsMTRTemperatureControlClusterSetTemperatureParams mtrTemperatureControlClusterSetTemperatureParams => mtrTemperatureControlClusterSetTemperatureParams -> IO (Id NSNumber)
targetTemperatureLevel mtrTemperatureControlClusterSetTemperatureParams =
  sendMessage mtrTemperatureControlClusterSetTemperatureParams targetTemperatureLevelSelector

-- | @- setTargetTemperatureLevel:@
setTargetTemperatureLevel :: (IsMTRTemperatureControlClusterSetTemperatureParams mtrTemperatureControlClusterSetTemperatureParams, IsNSNumber value) => mtrTemperatureControlClusterSetTemperatureParams -> value -> IO ()
setTargetTemperatureLevel mtrTemperatureControlClusterSetTemperatureParams value =
  sendMessage mtrTemperatureControlClusterSetTemperatureParams setTargetTemperatureLevelSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRTemperatureControlClusterSetTemperatureParams mtrTemperatureControlClusterSetTemperatureParams => mtrTemperatureControlClusterSetTemperatureParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrTemperatureControlClusterSetTemperatureParams =
  sendMessage mtrTemperatureControlClusterSetTemperatureParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRTemperatureControlClusterSetTemperatureParams mtrTemperatureControlClusterSetTemperatureParams, IsNSNumber value) => mtrTemperatureControlClusterSetTemperatureParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrTemperatureControlClusterSetTemperatureParams value =
  sendMessage mtrTemperatureControlClusterSetTemperatureParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRTemperatureControlClusterSetTemperatureParams mtrTemperatureControlClusterSetTemperatureParams => mtrTemperatureControlClusterSetTemperatureParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrTemperatureControlClusterSetTemperatureParams =
  sendMessage mtrTemperatureControlClusterSetTemperatureParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRTemperatureControlClusterSetTemperatureParams mtrTemperatureControlClusterSetTemperatureParams, IsNSNumber value) => mtrTemperatureControlClusterSetTemperatureParams -> value -> IO ()
setServerSideProcessingTimeout mtrTemperatureControlClusterSetTemperatureParams value =
  sendMessage mtrTemperatureControlClusterSetTemperatureParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @targetTemperature@
targetTemperatureSelector :: Selector '[] (Id NSNumber)
targetTemperatureSelector = mkSelector "targetTemperature"

-- | @Selector@ for @setTargetTemperature:@
setTargetTemperatureSelector :: Selector '[Id NSNumber] ()
setTargetTemperatureSelector = mkSelector "setTargetTemperature:"

-- | @Selector@ for @targetTemperatureLevel@
targetTemperatureLevelSelector :: Selector '[] (Id NSNumber)
targetTemperatureLevelSelector = mkSelector "targetTemperatureLevel"

-- | @Selector@ for @setTargetTemperatureLevel:@
setTargetTemperatureLevelSelector :: Selector '[Id NSNumber] ()
setTargetTemperatureLevelSelector = mkSelector "setTargetTemperatureLevel:"

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

