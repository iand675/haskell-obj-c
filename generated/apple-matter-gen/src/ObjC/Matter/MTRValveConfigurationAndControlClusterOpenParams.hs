{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRValveConfigurationAndControlClusterOpenParams@.
module ObjC.Matter.MTRValveConfigurationAndControlClusterOpenParams
  ( MTRValveConfigurationAndControlClusterOpenParams
  , IsMTRValveConfigurationAndControlClusterOpenParams(..)
  , openDuration
  , setOpenDuration
  , targetLevel
  , setTargetLevel
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , openDurationSelector
  , serverSideProcessingTimeoutSelector
  , setOpenDurationSelector
  , setServerSideProcessingTimeoutSelector
  , setTargetLevelSelector
  , setTimedInvokeTimeoutMsSelector
  , targetLevelSelector
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

-- | @- openDuration@
openDuration :: IsMTRValveConfigurationAndControlClusterOpenParams mtrValveConfigurationAndControlClusterOpenParams => mtrValveConfigurationAndControlClusterOpenParams -> IO (Id NSNumber)
openDuration mtrValveConfigurationAndControlClusterOpenParams =
  sendMessage mtrValveConfigurationAndControlClusterOpenParams openDurationSelector

-- | @- setOpenDuration:@
setOpenDuration :: (IsMTRValveConfigurationAndControlClusterOpenParams mtrValveConfigurationAndControlClusterOpenParams, IsNSNumber value) => mtrValveConfigurationAndControlClusterOpenParams -> value -> IO ()
setOpenDuration mtrValveConfigurationAndControlClusterOpenParams value =
  sendMessage mtrValveConfigurationAndControlClusterOpenParams setOpenDurationSelector (toNSNumber value)

-- | @- targetLevel@
targetLevel :: IsMTRValveConfigurationAndControlClusterOpenParams mtrValveConfigurationAndControlClusterOpenParams => mtrValveConfigurationAndControlClusterOpenParams -> IO (Id NSNumber)
targetLevel mtrValveConfigurationAndControlClusterOpenParams =
  sendMessage mtrValveConfigurationAndControlClusterOpenParams targetLevelSelector

-- | @- setTargetLevel:@
setTargetLevel :: (IsMTRValveConfigurationAndControlClusterOpenParams mtrValveConfigurationAndControlClusterOpenParams, IsNSNumber value) => mtrValveConfigurationAndControlClusterOpenParams -> value -> IO ()
setTargetLevel mtrValveConfigurationAndControlClusterOpenParams value =
  sendMessage mtrValveConfigurationAndControlClusterOpenParams setTargetLevelSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRValveConfigurationAndControlClusterOpenParams mtrValveConfigurationAndControlClusterOpenParams => mtrValveConfigurationAndControlClusterOpenParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrValveConfigurationAndControlClusterOpenParams =
  sendMessage mtrValveConfigurationAndControlClusterOpenParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRValveConfigurationAndControlClusterOpenParams mtrValveConfigurationAndControlClusterOpenParams, IsNSNumber value) => mtrValveConfigurationAndControlClusterOpenParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrValveConfigurationAndControlClusterOpenParams value =
  sendMessage mtrValveConfigurationAndControlClusterOpenParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRValveConfigurationAndControlClusterOpenParams mtrValveConfigurationAndControlClusterOpenParams => mtrValveConfigurationAndControlClusterOpenParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrValveConfigurationAndControlClusterOpenParams =
  sendMessage mtrValveConfigurationAndControlClusterOpenParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRValveConfigurationAndControlClusterOpenParams mtrValveConfigurationAndControlClusterOpenParams, IsNSNumber value) => mtrValveConfigurationAndControlClusterOpenParams -> value -> IO ()
setServerSideProcessingTimeout mtrValveConfigurationAndControlClusterOpenParams value =
  sendMessage mtrValveConfigurationAndControlClusterOpenParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @openDuration@
openDurationSelector :: Selector '[] (Id NSNumber)
openDurationSelector = mkSelector "openDuration"

-- | @Selector@ for @setOpenDuration:@
setOpenDurationSelector :: Selector '[Id NSNumber] ()
setOpenDurationSelector = mkSelector "setOpenDuration:"

-- | @Selector@ for @targetLevel@
targetLevelSelector :: Selector '[] (Id NSNumber)
targetLevelSelector = mkSelector "targetLevel"

-- | @Selector@ for @setTargetLevel:@
setTargetLevelSelector :: Selector '[Id NSNumber] ()
setTargetLevelSelector = mkSelector "setTargetLevel:"

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

