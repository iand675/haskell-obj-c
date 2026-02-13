{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRColorControlClusterStepColorParams@.
module ObjC.Matter.MTRColorControlClusterStepColorParams
  ( MTRColorControlClusterStepColorParams
  , IsMTRColorControlClusterStepColorParams(..)
  , stepX
  , setStepX
  , stepY
  , setStepY
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
  , setStepXSelector
  , setStepYSelector
  , setTimedInvokeTimeoutMsSelector
  , setTransitionTimeSelector
  , stepXSelector
  , stepYSelector
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

-- | @- stepX@
stepX :: IsMTRColorControlClusterStepColorParams mtrColorControlClusterStepColorParams => mtrColorControlClusterStepColorParams -> IO (Id NSNumber)
stepX mtrColorControlClusterStepColorParams =
  sendMessage mtrColorControlClusterStepColorParams stepXSelector

-- | @- setStepX:@
setStepX :: (IsMTRColorControlClusterStepColorParams mtrColorControlClusterStepColorParams, IsNSNumber value) => mtrColorControlClusterStepColorParams -> value -> IO ()
setStepX mtrColorControlClusterStepColorParams value =
  sendMessage mtrColorControlClusterStepColorParams setStepXSelector (toNSNumber value)

-- | @- stepY@
stepY :: IsMTRColorControlClusterStepColorParams mtrColorControlClusterStepColorParams => mtrColorControlClusterStepColorParams -> IO (Id NSNumber)
stepY mtrColorControlClusterStepColorParams =
  sendMessage mtrColorControlClusterStepColorParams stepYSelector

-- | @- setStepY:@
setStepY :: (IsMTRColorControlClusterStepColorParams mtrColorControlClusterStepColorParams, IsNSNumber value) => mtrColorControlClusterStepColorParams -> value -> IO ()
setStepY mtrColorControlClusterStepColorParams value =
  sendMessage mtrColorControlClusterStepColorParams setStepYSelector (toNSNumber value)

-- | @- transitionTime@
transitionTime :: IsMTRColorControlClusterStepColorParams mtrColorControlClusterStepColorParams => mtrColorControlClusterStepColorParams -> IO (Id NSNumber)
transitionTime mtrColorControlClusterStepColorParams =
  sendMessage mtrColorControlClusterStepColorParams transitionTimeSelector

-- | @- setTransitionTime:@
setTransitionTime :: (IsMTRColorControlClusterStepColorParams mtrColorControlClusterStepColorParams, IsNSNumber value) => mtrColorControlClusterStepColorParams -> value -> IO ()
setTransitionTime mtrColorControlClusterStepColorParams value =
  sendMessage mtrColorControlClusterStepColorParams setTransitionTimeSelector (toNSNumber value)

-- | @- optionsMask@
optionsMask :: IsMTRColorControlClusterStepColorParams mtrColorControlClusterStepColorParams => mtrColorControlClusterStepColorParams -> IO (Id NSNumber)
optionsMask mtrColorControlClusterStepColorParams =
  sendMessage mtrColorControlClusterStepColorParams optionsMaskSelector

-- | @- setOptionsMask:@
setOptionsMask :: (IsMTRColorControlClusterStepColorParams mtrColorControlClusterStepColorParams, IsNSNumber value) => mtrColorControlClusterStepColorParams -> value -> IO ()
setOptionsMask mtrColorControlClusterStepColorParams value =
  sendMessage mtrColorControlClusterStepColorParams setOptionsMaskSelector (toNSNumber value)

-- | @- optionsOverride@
optionsOverride :: IsMTRColorControlClusterStepColorParams mtrColorControlClusterStepColorParams => mtrColorControlClusterStepColorParams -> IO (Id NSNumber)
optionsOverride mtrColorControlClusterStepColorParams =
  sendMessage mtrColorControlClusterStepColorParams optionsOverrideSelector

-- | @- setOptionsOverride:@
setOptionsOverride :: (IsMTRColorControlClusterStepColorParams mtrColorControlClusterStepColorParams, IsNSNumber value) => mtrColorControlClusterStepColorParams -> value -> IO ()
setOptionsOverride mtrColorControlClusterStepColorParams value =
  sendMessage mtrColorControlClusterStepColorParams setOptionsOverrideSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRColorControlClusterStepColorParams mtrColorControlClusterStepColorParams => mtrColorControlClusterStepColorParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrColorControlClusterStepColorParams =
  sendMessage mtrColorControlClusterStepColorParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRColorControlClusterStepColorParams mtrColorControlClusterStepColorParams, IsNSNumber value) => mtrColorControlClusterStepColorParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrColorControlClusterStepColorParams value =
  sendMessage mtrColorControlClusterStepColorParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRColorControlClusterStepColorParams mtrColorControlClusterStepColorParams => mtrColorControlClusterStepColorParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrColorControlClusterStepColorParams =
  sendMessage mtrColorControlClusterStepColorParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRColorControlClusterStepColorParams mtrColorControlClusterStepColorParams, IsNSNumber value) => mtrColorControlClusterStepColorParams -> value -> IO ()
setServerSideProcessingTimeout mtrColorControlClusterStepColorParams value =
  sendMessage mtrColorControlClusterStepColorParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stepX@
stepXSelector :: Selector '[] (Id NSNumber)
stepXSelector = mkSelector "stepX"

-- | @Selector@ for @setStepX:@
setStepXSelector :: Selector '[Id NSNumber] ()
setStepXSelector = mkSelector "setStepX:"

-- | @Selector@ for @stepY@
stepYSelector :: Selector '[] (Id NSNumber)
stepYSelector = mkSelector "stepY"

-- | @Selector@ for @setStepY:@
setStepYSelector :: Selector '[Id NSNumber] ()
setStepYSelector = mkSelector "setStepY:"

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

