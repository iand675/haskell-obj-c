{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRColorControlClusterMoveToHueParams@.
module ObjC.Matter.MTRColorControlClusterMoveToHueParams
  ( MTRColorControlClusterMoveToHueParams
  , IsMTRColorControlClusterMoveToHueParams(..)
  , hue
  , setHue
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
  , hueSelector
  , optionsMaskSelector
  , optionsOverrideSelector
  , serverSideProcessingTimeoutSelector
  , setDirectionSelector
  , setHueSelector
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

-- | @- hue@
hue :: IsMTRColorControlClusterMoveToHueParams mtrColorControlClusterMoveToHueParams => mtrColorControlClusterMoveToHueParams -> IO (Id NSNumber)
hue mtrColorControlClusterMoveToHueParams =
  sendMessage mtrColorControlClusterMoveToHueParams hueSelector

-- | @- setHue:@
setHue :: (IsMTRColorControlClusterMoveToHueParams mtrColorControlClusterMoveToHueParams, IsNSNumber value) => mtrColorControlClusterMoveToHueParams -> value -> IO ()
setHue mtrColorControlClusterMoveToHueParams value =
  sendMessage mtrColorControlClusterMoveToHueParams setHueSelector (toNSNumber value)

-- | @- direction@
direction :: IsMTRColorControlClusterMoveToHueParams mtrColorControlClusterMoveToHueParams => mtrColorControlClusterMoveToHueParams -> IO (Id NSNumber)
direction mtrColorControlClusterMoveToHueParams =
  sendMessage mtrColorControlClusterMoveToHueParams directionSelector

-- | @- setDirection:@
setDirection :: (IsMTRColorControlClusterMoveToHueParams mtrColorControlClusterMoveToHueParams, IsNSNumber value) => mtrColorControlClusterMoveToHueParams -> value -> IO ()
setDirection mtrColorControlClusterMoveToHueParams value =
  sendMessage mtrColorControlClusterMoveToHueParams setDirectionSelector (toNSNumber value)

-- | @- transitionTime@
transitionTime :: IsMTRColorControlClusterMoveToHueParams mtrColorControlClusterMoveToHueParams => mtrColorControlClusterMoveToHueParams -> IO (Id NSNumber)
transitionTime mtrColorControlClusterMoveToHueParams =
  sendMessage mtrColorControlClusterMoveToHueParams transitionTimeSelector

-- | @- setTransitionTime:@
setTransitionTime :: (IsMTRColorControlClusterMoveToHueParams mtrColorControlClusterMoveToHueParams, IsNSNumber value) => mtrColorControlClusterMoveToHueParams -> value -> IO ()
setTransitionTime mtrColorControlClusterMoveToHueParams value =
  sendMessage mtrColorControlClusterMoveToHueParams setTransitionTimeSelector (toNSNumber value)

-- | @- optionsMask@
optionsMask :: IsMTRColorControlClusterMoveToHueParams mtrColorControlClusterMoveToHueParams => mtrColorControlClusterMoveToHueParams -> IO (Id NSNumber)
optionsMask mtrColorControlClusterMoveToHueParams =
  sendMessage mtrColorControlClusterMoveToHueParams optionsMaskSelector

-- | @- setOptionsMask:@
setOptionsMask :: (IsMTRColorControlClusterMoveToHueParams mtrColorControlClusterMoveToHueParams, IsNSNumber value) => mtrColorControlClusterMoveToHueParams -> value -> IO ()
setOptionsMask mtrColorControlClusterMoveToHueParams value =
  sendMessage mtrColorControlClusterMoveToHueParams setOptionsMaskSelector (toNSNumber value)

-- | @- optionsOverride@
optionsOverride :: IsMTRColorControlClusterMoveToHueParams mtrColorControlClusterMoveToHueParams => mtrColorControlClusterMoveToHueParams -> IO (Id NSNumber)
optionsOverride mtrColorControlClusterMoveToHueParams =
  sendMessage mtrColorControlClusterMoveToHueParams optionsOverrideSelector

-- | @- setOptionsOverride:@
setOptionsOverride :: (IsMTRColorControlClusterMoveToHueParams mtrColorControlClusterMoveToHueParams, IsNSNumber value) => mtrColorControlClusterMoveToHueParams -> value -> IO ()
setOptionsOverride mtrColorControlClusterMoveToHueParams value =
  sendMessage mtrColorControlClusterMoveToHueParams setOptionsOverrideSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRColorControlClusterMoveToHueParams mtrColorControlClusterMoveToHueParams => mtrColorControlClusterMoveToHueParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrColorControlClusterMoveToHueParams =
  sendMessage mtrColorControlClusterMoveToHueParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRColorControlClusterMoveToHueParams mtrColorControlClusterMoveToHueParams, IsNSNumber value) => mtrColorControlClusterMoveToHueParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrColorControlClusterMoveToHueParams value =
  sendMessage mtrColorControlClusterMoveToHueParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRColorControlClusterMoveToHueParams mtrColorControlClusterMoveToHueParams => mtrColorControlClusterMoveToHueParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrColorControlClusterMoveToHueParams =
  sendMessage mtrColorControlClusterMoveToHueParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRColorControlClusterMoveToHueParams mtrColorControlClusterMoveToHueParams, IsNSNumber value) => mtrColorControlClusterMoveToHueParams -> value -> IO ()
setServerSideProcessingTimeout mtrColorControlClusterMoveToHueParams value =
  sendMessage mtrColorControlClusterMoveToHueParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @hue@
hueSelector :: Selector '[] (Id NSNumber)
hueSelector = mkSelector "hue"

-- | @Selector@ for @setHue:@
setHueSelector :: Selector '[Id NSNumber] ()
setHueSelector = mkSelector "setHue:"

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

