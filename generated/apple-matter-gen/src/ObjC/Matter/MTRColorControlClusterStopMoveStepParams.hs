{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRColorControlClusterStopMoveStepParams@.
module ObjC.Matter.MTRColorControlClusterStopMoveStepParams
  ( MTRColorControlClusterStopMoveStepParams
  , IsMTRColorControlClusterStopMoveStepParams(..)
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

-- | @- optionsMask@
optionsMask :: IsMTRColorControlClusterStopMoveStepParams mtrColorControlClusterStopMoveStepParams => mtrColorControlClusterStopMoveStepParams -> IO (Id NSNumber)
optionsMask mtrColorControlClusterStopMoveStepParams =
  sendMessage mtrColorControlClusterStopMoveStepParams optionsMaskSelector

-- | @- setOptionsMask:@
setOptionsMask :: (IsMTRColorControlClusterStopMoveStepParams mtrColorControlClusterStopMoveStepParams, IsNSNumber value) => mtrColorControlClusterStopMoveStepParams -> value -> IO ()
setOptionsMask mtrColorControlClusterStopMoveStepParams value =
  sendMessage mtrColorControlClusterStopMoveStepParams setOptionsMaskSelector (toNSNumber value)

-- | @- optionsOverride@
optionsOverride :: IsMTRColorControlClusterStopMoveStepParams mtrColorControlClusterStopMoveStepParams => mtrColorControlClusterStopMoveStepParams -> IO (Id NSNumber)
optionsOverride mtrColorControlClusterStopMoveStepParams =
  sendMessage mtrColorControlClusterStopMoveStepParams optionsOverrideSelector

-- | @- setOptionsOverride:@
setOptionsOverride :: (IsMTRColorControlClusterStopMoveStepParams mtrColorControlClusterStopMoveStepParams, IsNSNumber value) => mtrColorControlClusterStopMoveStepParams -> value -> IO ()
setOptionsOverride mtrColorControlClusterStopMoveStepParams value =
  sendMessage mtrColorControlClusterStopMoveStepParams setOptionsOverrideSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRColorControlClusterStopMoveStepParams mtrColorControlClusterStopMoveStepParams => mtrColorControlClusterStopMoveStepParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrColorControlClusterStopMoveStepParams =
  sendMessage mtrColorControlClusterStopMoveStepParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRColorControlClusterStopMoveStepParams mtrColorControlClusterStopMoveStepParams, IsNSNumber value) => mtrColorControlClusterStopMoveStepParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrColorControlClusterStopMoveStepParams value =
  sendMessage mtrColorControlClusterStopMoveStepParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRColorControlClusterStopMoveStepParams mtrColorControlClusterStopMoveStepParams => mtrColorControlClusterStopMoveStepParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrColorControlClusterStopMoveStepParams =
  sendMessage mtrColorControlClusterStopMoveStepParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRColorControlClusterStopMoveStepParams mtrColorControlClusterStopMoveStepParams, IsNSNumber value) => mtrColorControlClusterStopMoveStepParams -> value -> IO ()
setServerSideProcessingTimeout mtrColorControlClusterStopMoveStepParams value =
  sendMessage mtrColorControlClusterStopMoveStepParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

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

