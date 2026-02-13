{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRColorControlClusterMoveToColorParams@.
module ObjC.Matter.MTRColorControlClusterMoveToColorParams
  ( MTRColorControlClusterMoveToColorParams
  , IsMTRColorControlClusterMoveToColorParams(..)
  , colorX
  , setColorX
  , colorY
  , setColorY
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
  , colorXSelector
  , colorYSelector
  , optionsMaskSelector
  , optionsOverrideSelector
  , serverSideProcessingTimeoutSelector
  , setColorXSelector
  , setColorYSelector
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

-- | @- colorX@
colorX :: IsMTRColorControlClusterMoveToColorParams mtrColorControlClusterMoveToColorParams => mtrColorControlClusterMoveToColorParams -> IO (Id NSNumber)
colorX mtrColorControlClusterMoveToColorParams =
  sendMessage mtrColorControlClusterMoveToColorParams colorXSelector

-- | @- setColorX:@
setColorX :: (IsMTRColorControlClusterMoveToColorParams mtrColorControlClusterMoveToColorParams, IsNSNumber value) => mtrColorControlClusterMoveToColorParams -> value -> IO ()
setColorX mtrColorControlClusterMoveToColorParams value =
  sendMessage mtrColorControlClusterMoveToColorParams setColorXSelector (toNSNumber value)

-- | @- colorY@
colorY :: IsMTRColorControlClusterMoveToColorParams mtrColorControlClusterMoveToColorParams => mtrColorControlClusterMoveToColorParams -> IO (Id NSNumber)
colorY mtrColorControlClusterMoveToColorParams =
  sendMessage mtrColorControlClusterMoveToColorParams colorYSelector

-- | @- setColorY:@
setColorY :: (IsMTRColorControlClusterMoveToColorParams mtrColorControlClusterMoveToColorParams, IsNSNumber value) => mtrColorControlClusterMoveToColorParams -> value -> IO ()
setColorY mtrColorControlClusterMoveToColorParams value =
  sendMessage mtrColorControlClusterMoveToColorParams setColorYSelector (toNSNumber value)

-- | @- transitionTime@
transitionTime :: IsMTRColorControlClusterMoveToColorParams mtrColorControlClusterMoveToColorParams => mtrColorControlClusterMoveToColorParams -> IO (Id NSNumber)
transitionTime mtrColorControlClusterMoveToColorParams =
  sendMessage mtrColorControlClusterMoveToColorParams transitionTimeSelector

-- | @- setTransitionTime:@
setTransitionTime :: (IsMTRColorControlClusterMoveToColorParams mtrColorControlClusterMoveToColorParams, IsNSNumber value) => mtrColorControlClusterMoveToColorParams -> value -> IO ()
setTransitionTime mtrColorControlClusterMoveToColorParams value =
  sendMessage mtrColorControlClusterMoveToColorParams setTransitionTimeSelector (toNSNumber value)

-- | @- optionsMask@
optionsMask :: IsMTRColorControlClusterMoveToColorParams mtrColorControlClusterMoveToColorParams => mtrColorControlClusterMoveToColorParams -> IO (Id NSNumber)
optionsMask mtrColorControlClusterMoveToColorParams =
  sendMessage mtrColorControlClusterMoveToColorParams optionsMaskSelector

-- | @- setOptionsMask:@
setOptionsMask :: (IsMTRColorControlClusterMoveToColorParams mtrColorControlClusterMoveToColorParams, IsNSNumber value) => mtrColorControlClusterMoveToColorParams -> value -> IO ()
setOptionsMask mtrColorControlClusterMoveToColorParams value =
  sendMessage mtrColorControlClusterMoveToColorParams setOptionsMaskSelector (toNSNumber value)

-- | @- optionsOverride@
optionsOverride :: IsMTRColorControlClusterMoveToColorParams mtrColorControlClusterMoveToColorParams => mtrColorControlClusterMoveToColorParams -> IO (Id NSNumber)
optionsOverride mtrColorControlClusterMoveToColorParams =
  sendMessage mtrColorControlClusterMoveToColorParams optionsOverrideSelector

-- | @- setOptionsOverride:@
setOptionsOverride :: (IsMTRColorControlClusterMoveToColorParams mtrColorControlClusterMoveToColorParams, IsNSNumber value) => mtrColorControlClusterMoveToColorParams -> value -> IO ()
setOptionsOverride mtrColorControlClusterMoveToColorParams value =
  sendMessage mtrColorControlClusterMoveToColorParams setOptionsOverrideSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRColorControlClusterMoveToColorParams mtrColorControlClusterMoveToColorParams => mtrColorControlClusterMoveToColorParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrColorControlClusterMoveToColorParams =
  sendMessage mtrColorControlClusterMoveToColorParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRColorControlClusterMoveToColorParams mtrColorControlClusterMoveToColorParams, IsNSNumber value) => mtrColorControlClusterMoveToColorParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrColorControlClusterMoveToColorParams value =
  sendMessage mtrColorControlClusterMoveToColorParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRColorControlClusterMoveToColorParams mtrColorControlClusterMoveToColorParams => mtrColorControlClusterMoveToColorParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrColorControlClusterMoveToColorParams =
  sendMessage mtrColorControlClusterMoveToColorParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRColorControlClusterMoveToColorParams mtrColorControlClusterMoveToColorParams, IsNSNumber value) => mtrColorControlClusterMoveToColorParams -> value -> IO ()
setServerSideProcessingTimeout mtrColorControlClusterMoveToColorParams value =
  sendMessage mtrColorControlClusterMoveToColorParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @colorX@
colorXSelector :: Selector '[] (Id NSNumber)
colorXSelector = mkSelector "colorX"

-- | @Selector@ for @setColorX:@
setColorXSelector :: Selector '[Id NSNumber] ()
setColorXSelector = mkSelector "setColorX:"

-- | @Selector@ for @colorY@
colorYSelector :: Selector '[] (Id NSNumber)
colorYSelector = mkSelector "colorY"

-- | @Selector@ for @setColorY:@
setColorYSelector :: Selector '[Id NSNumber] ()
setColorYSelector = mkSelector "setColorY:"

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

