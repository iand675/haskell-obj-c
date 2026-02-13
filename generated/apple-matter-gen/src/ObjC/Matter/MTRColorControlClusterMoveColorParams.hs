{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRColorControlClusterMoveColorParams@.
module ObjC.Matter.MTRColorControlClusterMoveColorParams
  ( MTRColorControlClusterMoveColorParams
  , IsMTRColorControlClusterMoveColorParams(..)
  , rateX
  , setRateX
  , rateY
  , setRateY
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
  , rateXSelector
  , rateYSelector
  , serverSideProcessingTimeoutSelector
  , setOptionsMaskSelector
  , setOptionsOverrideSelector
  , setRateXSelector
  , setRateYSelector
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

-- | @- rateX@
rateX :: IsMTRColorControlClusterMoveColorParams mtrColorControlClusterMoveColorParams => mtrColorControlClusterMoveColorParams -> IO (Id NSNumber)
rateX mtrColorControlClusterMoveColorParams =
  sendMessage mtrColorControlClusterMoveColorParams rateXSelector

-- | @- setRateX:@
setRateX :: (IsMTRColorControlClusterMoveColorParams mtrColorControlClusterMoveColorParams, IsNSNumber value) => mtrColorControlClusterMoveColorParams -> value -> IO ()
setRateX mtrColorControlClusterMoveColorParams value =
  sendMessage mtrColorControlClusterMoveColorParams setRateXSelector (toNSNumber value)

-- | @- rateY@
rateY :: IsMTRColorControlClusterMoveColorParams mtrColorControlClusterMoveColorParams => mtrColorControlClusterMoveColorParams -> IO (Id NSNumber)
rateY mtrColorControlClusterMoveColorParams =
  sendMessage mtrColorControlClusterMoveColorParams rateYSelector

-- | @- setRateY:@
setRateY :: (IsMTRColorControlClusterMoveColorParams mtrColorControlClusterMoveColorParams, IsNSNumber value) => mtrColorControlClusterMoveColorParams -> value -> IO ()
setRateY mtrColorControlClusterMoveColorParams value =
  sendMessage mtrColorControlClusterMoveColorParams setRateYSelector (toNSNumber value)

-- | @- optionsMask@
optionsMask :: IsMTRColorControlClusterMoveColorParams mtrColorControlClusterMoveColorParams => mtrColorControlClusterMoveColorParams -> IO (Id NSNumber)
optionsMask mtrColorControlClusterMoveColorParams =
  sendMessage mtrColorControlClusterMoveColorParams optionsMaskSelector

-- | @- setOptionsMask:@
setOptionsMask :: (IsMTRColorControlClusterMoveColorParams mtrColorControlClusterMoveColorParams, IsNSNumber value) => mtrColorControlClusterMoveColorParams -> value -> IO ()
setOptionsMask mtrColorControlClusterMoveColorParams value =
  sendMessage mtrColorControlClusterMoveColorParams setOptionsMaskSelector (toNSNumber value)

-- | @- optionsOverride@
optionsOverride :: IsMTRColorControlClusterMoveColorParams mtrColorControlClusterMoveColorParams => mtrColorControlClusterMoveColorParams -> IO (Id NSNumber)
optionsOverride mtrColorControlClusterMoveColorParams =
  sendMessage mtrColorControlClusterMoveColorParams optionsOverrideSelector

-- | @- setOptionsOverride:@
setOptionsOverride :: (IsMTRColorControlClusterMoveColorParams mtrColorControlClusterMoveColorParams, IsNSNumber value) => mtrColorControlClusterMoveColorParams -> value -> IO ()
setOptionsOverride mtrColorControlClusterMoveColorParams value =
  sendMessage mtrColorControlClusterMoveColorParams setOptionsOverrideSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRColorControlClusterMoveColorParams mtrColorControlClusterMoveColorParams => mtrColorControlClusterMoveColorParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrColorControlClusterMoveColorParams =
  sendMessage mtrColorControlClusterMoveColorParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRColorControlClusterMoveColorParams mtrColorControlClusterMoveColorParams, IsNSNumber value) => mtrColorControlClusterMoveColorParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrColorControlClusterMoveColorParams value =
  sendMessage mtrColorControlClusterMoveColorParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRColorControlClusterMoveColorParams mtrColorControlClusterMoveColorParams => mtrColorControlClusterMoveColorParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrColorControlClusterMoveColorParams =
  sendMessage mtrColorControlClusterMoveColorParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRColorControlClusterMoveColorParams mtrColorControlClusterMoveColorParams, IsNSNumber value) => mtrColorControlClusterMoveColorParams -> value -> IO ()
setServerSideProcessingTimeout mtrColorControlClusterMoveColorParams value =
  sendMessage mtrColorControlClusterMoveColorParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @rateX@
rateXSelector :: Selector '[] (Id NSNumber)
rateXSelector = mkSelector "rateX"

-- | @Selector@ for @setRateX:@
setRateXSelector :: Selector '[Id NSNumber] ()
setRateXSelector = mkSelector "setRateX:"

-- | @Selector@ for @rateY@
rateYSelector :: Selector '[] (Id NSNumber)
rateYSelector = mkSelector "rateY"

-- | @Selector@ for @setRateY:@
setRateYSelector :: Selector '[Id NSNumber] ()
setRateYSelector = mkSelector "setRateY:"

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

