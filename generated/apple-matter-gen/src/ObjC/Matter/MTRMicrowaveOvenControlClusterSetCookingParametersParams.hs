{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRMicrowaveOvenControlClusterSetCookingParametersParams@.
module ObjC.Matter.MTRMicrowaveOvenControlClusterSetCookingParametersParams
  ( MTRMicrowaveOvenControlClusterSetCookingParametersParams
  , IsMTRMicrowaveOvenControlClusterSetCookingParametersParams(..)
  , cookMode
  , setCookMode
  , cookTime
  , setCookTime
  , powerSetting
  , setPowerSetting
  , wattSettingIndex
  , setWattSettingIndex
  , startAfterSetting
  , setStartAfterSetting
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , cookModeSelector
  , cookTimeSelector
  , powerSettingSelector
  , serverSideProcessingTimeoutSelector
  , setCookModeSelector
  , setCookTimeSelector
  , setPowerSettingSelector
  , setServerSideProcessingTimeoutSelector
  , setStartAfterSettingSelector
  , setTimedInvokeTimeoutMsSelector
  , setWattSettingIndexSelector
  , startAfterSettingSelector
  , timedInvokeTimeoutMsSelector
  , wattSettingIndexSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- cookMode@
cookMode :: IsMTRMicrowaveOvenControlClusterSetCookingParametersParams mtrMicrowaveOvenControlClusterSetCookingParametersParams => mtrMicrowaveOvenControlClusterSetCookingParametersParams -> IO (Id NSNumber)
cookMode mtrMicrowaveOvenControlClusterSetCookingParametersParams =
  sendMessage mtrMicrowaveOvenControlClusterSetCookingParametersParams cookModeSelector

-- | @- setCookMode:@
setCookMode :: (IsMTRMicrowaveOvenControlClusterSetCookingParametersParams mtrMicrowaveOvenControlClusterSetCookingParametersParams, IsNSNumber value) => mtrMicrowaveOvenControlClusterSetCookingParametersParams -> value -> IO ()
setCookMode mtrMicrowaveOvenControlClusterSetCookingParametersParams value =
  sendMessage mtrMicrowaveOvenControlClusterSetCookingParametersParams setCookModeSelector (toNSNumber value)

-- | @- cookTime@
cookTime :: IsMTRMicrowaveOvenControlClusterSetCookingParametersParams mtrMicrowaveOvenControlClusterSetCookingParametersParams => mtrMicrowaveOvenControlClusterSetCookingParametersParams -> IO (Id NSNumber)
cookTime mtrMicrowaveOvenControlClusterSetCookingParametersParams =
  sendMessage mtrMicrowaveOvenControlClusterSetCookingParametersParams cookTimeSelector

-- | @- setCookTime:@
setCookTime :: (IsMTRMicrowaveOvenControlClusterSetCookingParametersParams mtrMicrowaveOvenControlClusterSetCookingParametersParams, IsNSNumber value) => mtrMicrowaveOvenControlClusterSetCookingParametersParams -> value -> IO ()
setCookTime mtrMicrowaveOvenControlClusterSetCookingParametersParams value =
  sendMessage mtrMicrowaveOvenControlClusterSetCookingParametersParams setCookTimeSelector (toNSNumber value)

-- | @- powerSetting@
powerSetting :: IsMTRMicrowaveOvenControlClusterSetCookingParametersParams mtrMicrowaveOvenControlClusterSetCookingParametersParams => mtrMicrowaveOvenControlClusterSetCookingParametersParams -> IO (Id NSNumber)
powerSetting mtrMicrowaveOvenControlClusterSetCookingParametersParams =
  sendMessage mtrMicrowaveOvenControlClusterSetCookingParametersParams powerSettingSelector

-- | @- setPowerSetting:@
setPowerSetting :: (IsMTRMicrowaveOvenControlClusterSetCookingParametersParams mtrMicrowaveOvenControlClusterSetCookingParametersParams, IsNSNumber value) => mtrMicrowaveOvenControlClusterSetCookingParametersParams -> value -> IO ()
setPowerSetting mtrMicrowaveOvenControlClusterSetCookingParametersParams value =
  sendMessage mtrMicrowaveOvenControlClusterSetCookingParametersParams setPowerSettingSelector (toNSNumber value)

-- | @- wattSettingIndex@
wattSettingIndex :: IsMTRMicrowaveOvenControlClusterSetCookingParametersParams mtrMicrowaveOvenControlClusterSetCookingParametersParams => mtrMicrowaveOvenControlClusterSetCookingParametersParams -> IO (Id NSNumber)
wattSettingIndex mtrMicrowaveOvenControlClusterSetCookingParametersParams =
  sendMessage mtrMicrowaveOvenControlClusterSetCookingParametersParams wattSettingIndexSelector

-- | @- setWattSettingIndex:@
setWattSettingIndex :: (IsMTRMicrowaveOvenControlClusterSetCookingParametersParams mtrMicrowaveOvenControlClusterSetCookingParametersParams, IsNSNumber value) => mtrMicrowaveOvenControlClusterSetCookingParametersParams -> value -> IO ()
setWattSettingIndex mtrMicrowaveOvenControlClusterSetCookingParametersParams value =
  sendMessage mtrMicrowaveOvenControlClusterSetCookingParametersParams setWattSettingIndexSelector (toNSNumber value)

-- | @- startAfterSetting@
startAfterSetting :: IsMTRMicrowaveOvenControlClusterSetCookingParametersParams mtrMicrowaveOvenControlClusterSetCookingParametersParams => mtrMicrowaveOvenControlClusterSetCookingParametersParams -> IO (Id NSNumber)
startAfterSetting mtrMicrowaveOvenControlClusterSetCookingParametersParams =
  sendMessage mtrMicrowaveOvenControlClusterSetCookingParametersParams startAfterSettingSelector

-- | @- setStartAfterSetting:@
setStartAfterSetting :: (IsMTRMicrowaveOvenControlClusterSetCookingParametersParams mtrMicrowaveOvenControlClusterSetCookingParametersParams, IsNSNumber value) => mtrMicrowaveOvenControlClusterSetCookingParametersParams -> value -> IO ()
setStartAfterSetting mtrMicrowaveOvenControlClusterSetCookingParametersParams value =
  sendMessage mtrMicrowaveOvenControlClusterSetCookingParametersParams setStartAfterSettingSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRMicrowaveOvenControlClusterSetCookingParametersParams mtrMicrowaveOvenControlClusterSetCookingParametersParams => mtrMicrowaveOvenControlClusterSetCookingParametersParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrMicrowaveOvenControlClusterSetCookingParametersParams =
  sendMessage mtrMicrowaveOvenControlClusterSetCookingParametersParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRMicrowaveOvenControlClusterSetCookingParametersParams mtrMicrowaveOvenControlClusterSetCookingParametersParams, IsNSNumber value) => mtrMicrowaveOvenControlClusterSetCookingParametersParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrMicrowaveOvenControlClusterSetCookingParametersParams value =
  sendMessage mtrMicrowaveOvenControlClusterSetCookingParametersParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRMicrowaveOvenControlClusterSetCookingParametersParams mtrMicrowaveOvenControlClusterSetCookingParametersParams => mtrMicrowaveOvenControlClusterSetCookingParametersParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrMicrowaveOvenControlClusterSetCookingParametersParams =
  sendMessage mtrMicrowaveOvenControlClusterSetCookingParametersParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRMicrowaveOvenControlClusterSetCookingParametersParams mtrMicrowaveOvenControlClusterSetCookingParametersParams, IsNSNumber value) => mtrMicrowaveOvenControlClusterSetCookingParametersParams -> value -> IO ()
setServerSideProcessingTimeout mtrMicrowaveOvenControlClusterSetCookingParametersParams value =
  sendMessage mtrMicrowaveOvenControlClusterSetCookingParametersParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cookMode@
cookModeSelector :: Selector '[] (Id NSNumber)
cookModeSelector = mkSelector "cookMode"

-- | @Selector@ for @setCookMode:@
setCookModeSelector :: Selector '[Id NSNumber] ()
setCookModeSelector = mkSelector "setCookMode:"

-- | @Selector@ for @cookTime@
cookTimeSelector :: Selector '[] (Id NSNumber)
cookTimeSelector = mkSelector "cookTime"

-- | @Selector@ for @setCookTime:@
setCookTimeSelector :: Selector '[Id NSNumber] ()
setCookTimeSelector = mkSelector "setCookTime:"

-- | @Selector@ for @powerSetting@
powerSettingSelector :: Selector '[] (Id NSNumber)
powerSettingSelector = mkSelector "powerSetting"

-- | @Selector@ for @setPowerSetting:@
setPowerSettingSelector :: Selector '[Id NSNumber] ()
setPowerSettingSelector = mkSelector "setPowerSetting:"

-- | @Selector@ for @wattSettingIndex@
wattSettingIndexSelector :: Selector '[] (Id NSNumber)
wattSettingIndexSelector = mkSelector "wattSettingIndex"

-- | @Selector@ for @setWattSettingIndex:@
setWattSettingIndexSelector :: Selector '[Id NSNumber] ()
setWattSettingIndexSelector = mkSelector "setWattSettingIndex:"

-- | @Selector@ for @startAfterSetting@
startAfterSettingSelector :: Selector '[] (Id NSNumber)
startAfterSettingSelector = mkSelector "startAfterSetting"

-- | @Selector@ for @setStartAfterSetting:@
setStartAfterSettingSelector :: Selector '[Id NSNumber] ()
setStartAfterSettingSelector = mkSelector "setStartAfterSetting:"

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

