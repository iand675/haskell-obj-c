{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRLevelControlClusterMoveToLevelWithOnOffParams@.
module ObjC.Matter.MTRLevelControlClusterMoveToLevelWithOnOffParams
  ( MTRLevelControlClusterMoveToLevelWithOnOffParams
  , IsMTRLevelControlClusterMoveToLevelWithOnOffParams(..)
  , level
  , setLevel
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
  , levelSelector
  , optionsMaskSelector
  , optionsOverrideSelector
  , serverSideProcessingTimeoutSelector
  , setLevelSelector
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

-- | @- level@
level :: IsMTRLevelControlClusterMoveToLevelWithOnOffParams mtrLevelControlClusterMoveToLevelWithOnOffParams => mtrLevelControlClusterMoveToLevelWithOnOffParams -> IO (Id NSNumber)
level mtrLevelControlClusterMoveToLevelWithOnOffParams =
  sendMessage mtrLevelControlClusterMoveToLevelWithOnOffParams levelSelector

-- | @- setLevel:@
setLevel :: (IsMTRLevelControlClusterMoveToLevelWithOnOffParams mtrLevelControlClusterMoveToLevelWithOnOffParams, IsNSNumber value) => mtrLevelControlClusterMoveToLevelWithOnOffParams -> value -> IO ()
setLevel mtrLevelControlClusterMoveToLevelWithOnOffParams value =
  sendMessage mtrLevelControlClusterMoveToLevelWithOnOffParams setLevelSelector (toNSNumber value)

-- | @- transitionTime@
transitionTime :: IsMTRLevelControlClusterMoveToLevelWithOnOffParams mtrLevelControlClusterMoveToLevelWithOnOffParams => mtrLevelControlClusterMoveToLevelWithOnOffParams -> IO (Id NSNumber)
transitionTime mtrLevelControlClusterMoveToLevelWithOnOffParams =
  sendMessage mtrLevelControlClusterMoveToLevelWithOnOffParams transitionTimeSelector

-- | @- setTransitionTime:@
setTransitionTime :: (IsMTRLevelControlClusterMoveToLevelWithOnOffParams mtrLevelControlClusterMoveToLevelWithOnOffParams, IsNSNumber value) => mtrLevelControlClusterMoveToLevelWithOnOffParams -> value -> IO ()
setTransitionTime mtrLevelControlClusterMoveToLevelWithOnOffParams value =
  sendMessage mtrLevelControlClusterMoveToLevelWithOnOffParams setTransitionTimeSelector (toNSNumber value)

-- | @- optionsMask@
optionsMask :: IsMTRLevelControlClusterMoveToLevelWithOnOffParams mtrLevelControlClusterMoveToLevelWithOnOffParams => mtrLevelControlClusterMoveToLevelWithOnOffParams -> IO (Id NSNumber)
optionsMask mtrLevelControlClusterMoveToLevelWithOnOffParams =
  sendMessage mtrLevelControlClusterMoveToLevelWithOnOffParams optionsMaskSelector

-- | @- setOptionsMask:@
setOptionsMask :: (IsMTRLevelControlClusterMoveToLevelWithOnOffParams mtrLevelControlClusterMoveToLevelWithOnOffParams, IsNSNumber value) => mtrLevelControlClusterMoveToLevelWithOnOffParams -> value -> IO ()
setOptionsMask mtrLevelControlClusterMoveToLevelWithOnOffParams value =
  sendMessage mtrLevelControlClusterMoveToLevelWithOnOffParams setOptionsMaskSelector (toNSNumber value)

-- | @- optionsOverride@
optionsOverride :: IsMTRLevelControlClusterMoveToLevelWithOnOffParams mtrLevelControlClusterMoveToLevelWithOnOffParams => mtrLevelControlClusterMoveToLevelWithOnOffParams -> IO (Id NSNumber)
optionsOverride mtrLevelControlClusterMoveToLevelWithOnOffParams =
  sendMessage mtrLevelControlClusterMoveToLevelWithOnOffParams optionsOverrideSelector

-- | @- setOptionsOverride:@
setOptionsOverride :: (IsMTRLevelControlClusterMoveToLevelWithOnOffParams mtrLevelControlClusterMoveToLevelWithOnOffParams, IsNSNumber value) => mtrLevelControlClusterMoveToLevelWithOnOffParams -> value -> IO ()
setOptionsOverride mtrLevelControlClusterMoveToLevelWithOnOffParams value =
  sendMessage mtrLevelControlClusterMoveToLevelWithOnOffParams setOptionsOverrideSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRLevelControlClusterMoveToLevelWithOnOffParams mtrLevelControlClusterMoveToLevelWithOnOffParams => mtrLevelControlClusterMoveToLevelWithOnOffParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrLevelControlClusterMoveToLevelWithOnOffParams =
  sendMessage mtrLevelControlClusterMoveToLevelWithOnOffParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRLevelControlClusterMoveToLevelWithOnOffParams mtrLevelControlClusterMoveToLevelWithOnOffParams, IsNSNumber value) => mtrLevelControlClusterMoveToLevelWithOnOffParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrLevelControlClusterMoveToLevelWithOnOffParams value =
  sendMessage mtrLevelControlClusterMoveToLevelWithOnOffParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRLevelControlClusterMoveToLevelWithOnOffParams mtrLevelControlClusterMoveToLevelWithOnOffParams => mtrLevelControlClusterMoveToLevelWithOnOffParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrLevelControlClusterMoveToLevelWithOnOffParams =
  sendMessage mtrLevelControlClusterMoveToLevelWithOnOffParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRLevelControlClusterMoveToLevelWithOnOffParams mtrLevelControlClusterMoveToLevelWithOnOffParams, IsNSNumber value) => mtrLevelControlClusterMoveToLevelWithOnOffParams -> value -> IO ()
setServerSideProcessingTimeout mtrLevelControlClusterMoveToLevelWithOnOffParams value =
  sendMessage mtrLevelControlClusterMoveToLevelWithOnOffParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @level@
levelSelector :: Selector '[] (Id NSNumber)
levelSelector = mkSelector "level"

-- | @Selector@ for @setLevel:@
setLevelSelector :: Selector '[Id NSNumber] ()
setLevelSelector = mkSelector "setLevel:"

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

