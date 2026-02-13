{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRColorControlClusterEnhancedMoveToHueParams@.
module ObjC.Matter.MTRColorControlClusterEnhancedMoveToHueParams
  ( MTRColorControlClusterEnhancedMoveToHueParams
  , IsMTRColorControlClusterEnhancedMoveToHueParams(..)
  , enhancedHue
  , setEnhancedHue
  , direction
  , setDirection
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
  , directionSelector
  , enhancedHueSelector
  , optionsMaskSelector
  , optionsOverrideSelector
  , serverSideProcessingTimeoutSelector
  , setDirectionSelector
  , setEnhancedHueSelector
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

-- | @- enhancedHue@
enhancedHue :: IsMTRColorControlClusterEnhancedMoveToHueParams mtrColorControlClusterEnhancedMoveToHueParams => mtrColorControlClusterEnhancedMoveToHueParams -> IO (Id NSNumber)
enhancedHue mtrColorControlClusterEnhancedMoveToHueParams =
  sendMessage mtrColorControlClusterEnhancedMoveToHueParams enhancedHueSelector

-- | @- setEnhancedHue:@
setEnhancedHue :: (IsMTRColorControlClusterEnhancedMoveToHueParams mtrColorControlClusterEnhancedMoveToHueParams, IsNSNumber value) => mtrColorControlClusterEnhancedMoveToHueParams -> value -> IO ()
setEnhancedHue mtrColorControlClusterEnhancedMoveToHueParams value =
  sendMessage mtrColorControlClusterEnhancedMoveToHueParams setEnhancedHueSelector (toNSNumber value)

-- | @- direction@
direction :: IsMTRColorControlClusterEnhancedMoveToHueParams mtrColorControlClusterEnhancedMoveToHueParams => mtrColorControlClusterEnhancedMoveToHueParams -> IO (Id NSNumber)
direction mtrColorControlClusterEnhancedMoveToHueParams =
  sendMessage mtrColorControlClusterEnhancedMoveToHueParams directionSelector

-- | @- setDirection:@
setDirection :: (IsMTRColorControlClusterEnhancedMoveToHueParams mtrColorControlClusterEnhancedMoveToHueParams, IsNSNumber value) => mtrColorControlClusterEnhancedMoveToHueParams -> value -> IO ()
setDirection mtrColorControlClusterEnhancedMoveToHueParams value =
  sendMessage mtrColorControlClusterEnhancedMoveToHueParams setDirectionSelector (toNSNumber value)

-- | @- transitionTime@
transitionTime :: IsMTRColorControlClusterEnhancedMoveToHueParams mtrColorControlClusterEnhancedMoveToHueParams => mtrColorControlClusterEnhancedMoveToHueParams -> IO (Id NSNumber)
transitionTime mtrColorControlClusterEnhancedMoveToHueParams =
  sendMessage mtrColorControlClusterEnhancedMoveToHueParams transitionTimeSelector

-- | @- setTransitionTime:@
setTransitionTime :: (IsMTRColorControlClusterEnhancedMoveToHueParams mtrColorControlClusterEnhancedMoveToHueParams, IsNSNumber value) => mtrColorControlClusterEnhancedMoveToHueParams -> value -> IO ()
setTransitionTime mtrColorControlClusterEnhancedMoveToHueParams value =
  sendMessage mtrColorControlClusterEnhancedMoveToHueParams setTransitionTimeSelector (toNSNumber value)

-- | @- optionsMask@
optionsMask :: IsMTRColorControlClusterEnhancedMoveToHueParams mtrColorControlClusterEnhancedMoveToHueParams => mtrColorControlClusterEnhancedMoveToHueParams -> IO (Id NSNumber)
optionsMask mtrColorControlClusterEnhancedMoveToHueParams =
  sendMessage mtrColorControlClusterEnhancedMoveToHueParams optionsMaskSelector

-- | @- setOptionsMask:@
setOptionsMask :: (IsMTRColorControlClusterEnhancedMoveToHueParams mtrColorControlClusterEnhancedMoveToHueParams, IsNSNumber value) => mtrColorControlClusterEnhancedMoveToHueParams -> value -> IO ()
setOptionsMask mtrColorControlClusterEnhancedMoveToHueParams value =
  sendMessage mtrColorControlClusterEnhancedMoveToHueParams setOptionsMaskSelector (toNSNumber value)

-- | @- optionsOverride@
optionsOverride :: IsMTRColorControlClusterEnhancedMoveToHueParams mtrColorControlClusterEnhancedMoveToHueParams => mtrColorControlClusterEnhancedMoveToHueParams -> IO (Id NSNumber)
optionsOverride mtrColorControlClusterEnhancedMoveToHueParams =
  sendMessage mtrColorControlClusterEnhancedMoveToHueParams optionsOverrideSelector

-- | @- setOptionsOverride:@
setOptionsOverride :: (IsMTRColorControlClusterEnhancedMoveToHueParams mtrColorControlClusterEnhancedMoveToHueParams, IsNSNumber value) => mtrColorControlClusterEnhancedMoveToHueParams -> value -> IO ()
setOptionsOverride mtrColorControlClusterEnhancedMoveToHueParams value =
  sendMessage mtrColorControlClusterEnhancedMoveToHueParams setOptionsOverrideSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRColorControlClusterEnhancedMoveToHueParams mtrColorControlClusterEnhancedMoveToHueParams => mtrColorControlClusterEnhancedMoveToHueParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrColorControlClusterEnhancedMoveToHueParams =
  sendMessage mtrColorControlClusterEnhancedMoveToHueParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRColorControlClusterEnhancedMoveToHueParams mtrColorControlClusterEnhancedMoveToHueParams, IsNSNumber value) => mtrColorControlClusterEnhancedMoveToHueParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrColorControlClusterEnhancedMoveToHueParams value =
  sendMessage mtrColorControlClusterEnhancedMoveToHueParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRColorControlClusterEnhancedMoveToHueParams mtrColorControlClusterEnhancedMoveToHueParams => mtrColorControlClusterEnhancedMoveToHueParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrColorControlClusterEnhancedMoveToHueParams =
  sendMessage mtrColorControlClusterEnhancedMoveToHueParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRColorControlClusterEnhancedMoveToHueParams mtrColorControlClusterEnhancedMoveToHueParams, IsNSNumber value) => mtrColorControlClusterEnhancedMoveToHueParams -> value -> IO ()
setServerSideProcessingTimeout mtrColorControlClusterEnhancedMoveToHueParams value =
  sendMessage mtrColorControlClusterEnhancedMoveToHueParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @enhancedHue@
enhancedHueSelector :: Selector '[] (Id NSNumber)
enhancedHueSelector = mkSelector "enhancedHue"

-- | @Selector@ for @setEnhancedHue:@
setEnhancedHueSelector :: Selector '[Id NSNumber] ()
setEnhancedHueSelector = mkSelector "setEnhancedHue:"

-- | @Selector@ for @direction@
directionSelector :: Selector '[] (Id NSNumber)
directionSelector = mkSelector "direction"

-- | @Selector@ for @setDirection:@
setDirectionSelector :: Selector '[Id NSNumber] ()
setDirectionSelector = mkSelector "setDirection:"

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

