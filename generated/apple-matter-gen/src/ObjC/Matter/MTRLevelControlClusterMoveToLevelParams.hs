{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRLevelControlClusterMoveToLevelParams@.
module ObjC.Matter.MTRLevelControlClusterMoveToLevelParams
  ( MTRLevelControlClusterMoveToLevelParams
  , IsMTRLevelControlClusterMoveToLevelParams(..)
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
level :: IsMTRLevelControlClusterMoveToLevelParams mtrLevelControlClusterMoveToLevelParams => mtrLevelControlClusterMoveToLevelParams -> IO (Id NSNumber)
level mtrLevelControlClusterMoveToLevelParams =
  sendMessage mtrLevelControlClusterMoveToLevelParams levelSelector

-- | @- setLevel:@
setLevel :: (IsMTRLevelControlClusterMoveToLevelParams mtrLevelControlClusterMoveToLevelParams, IsNSNumber value) => mtrLevelControlClusterMoveToLevelParams -> value -> IO ()
setLevel mtrLevelControlClusterMoveToLevelParams value =
  sendMessage mtrLevelControlClusterMoveToLevelParams setLevelSelector (toNSNumber value)

-- | @- transitionTime@
transitionTime :: IsMTRLevelControlClusterMoveToLevelParams mtrLevelControlClusterMoveToLevelParams => mtrLevelControlClusterMoveToLevelParams -> IO (Id NSNumber)
transitionTime mtrLevelControlClusterMoveToLevelParams =
  sendMessage mtrLevelControlClusterMoveToLevelParams transitionTimeSelector

-- | @- setTransitionTime:@
setTransitionTime :: (IsMTRLevelControlClusterMoveToLevelParams mtrLevelControlClusterMoveToLevelParams, IsNSNumber value) => mtrLevelControlClusterMoveToLevelParams -> value -> IO ()
setTransitionTime mtrLevelControlClusterMoveToLevelParams value =
  sendMessage mtrLevelControlClusterMoveToLevelParams setTransitionTimeSelector (toNSNumber value)

-- | @- optionsMask@
optionsMask :: IsMTRLevelControlClusterMoveToLevelParams mtrLevelControlClusterMoveToLevelParams => mtrLevelControlClusterMoveToLevelParams -> IO (Id NSNumber)
optionsMask mtrLevelControlClusterMoveToLevelParams =
  sendMessage mtrLevelControlClusterMoveToLevelParams optionsMaskSelector

-- | @- setOptionsMask:@
setOptionsMask :: (IsMTRLevelControlClusterMoveToLevelParams mtrLevelControlClusterMoveToLevelParams, IsNSNumber value) => mtrLevelControlClusterMoveToLevelParams -> value -> IO ()
setOptionsMask mtrLevelControlClusterMoveToLevelParams value =
  sendMessage mtrLevelControlClusterMoveToLevelParams setOptionsMaskSelector (toNSNumber value)

-- | @- optionsOverride@
optionsOverride :: IsMTRLevelControlClusterMoveToLevelParams mtrLevelControlClusterMoveToLevelParams => mtrLevelControlClusterMoveToLevelParams -> IO (Id NSNumber)
optionsOverride mtrLevelControlClusterMoveToLevelParams =
  sendMessage mtrLevelControlClusterMoveToLevelParams optionsOverrideSelector

-- | @- setOptionsOverride:@
setOptionsOverride :: (IsMTRLevelControlClusterMoveToLevelParams mtrLevelControlClusterMoveToLevelParams, IsNSNumber value) => mtrLevelControlClusterMoveToLevelParams -> value -> IO ()
setOptionsOverride mtrLevelControlClusterMoveToLevelParams value =
  sendMessage mtrLevelControlClusterMoveToLevelParams setOptionsOverrideSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRLevelControlClusterMoveToLevelParams mtrLevelControlClusterMoveToLevelParams => mtrLevelControlClusterMoveToLevelParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrLevelControlClusterMoveToLevelParams =
  sendMessage mtrLevelControlClusterMoveToLevelParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRLevelControlClusterMoveToLevelParams mtrLevelControlClusterMoveToLevelParams, IsNSNumber value) => mtrLevelControlClusterMoveToLevelParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrLevelControlClusterMoveToLevelParams value =
  sendMessage mtrLevelControlClusterMoveToLevelParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRLevelControlClusterMoveToLevelParams mtrLevelControlClusterMoveToLevelParams => mtrLevelControlClusterMoveToLevelParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrLevelControlClusterMoveToLevelParams =
  sendMessage mtrLevelControlClusterMoveToLevelParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRLevelControlClusterMoveToLevelParams mtrLevelControlClusterMoveToLevelParams, IsNSNumber value) => mtrLevelControlClusterMoveToLevelParams -> value -> IO ()
setServerSideProcessingTimeout mtrLevelControlClusterMoveToLevelParams value =
  sendMessage mtrLevelControlClusterMoveToLevelParams setServerSideProcessingTimeoutSelector (toNSNumber value)

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

