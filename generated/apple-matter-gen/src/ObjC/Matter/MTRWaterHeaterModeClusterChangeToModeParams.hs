{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRWaterHeaterModeClusterChangeToModeParams@.
module ObjC.Matter.MTRWaterHeaterModeClusterChangeToModeParams
  ( MTRWaterHeaterModeClusterChangeToModeParams
  , IsMTRWaterHeaterModeClusterChangeToModeParams(..)
  , newMode
  , setNewMode
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , newModeSelector
  , serverSideProcessingTimeoutSelector
  , setNewModeSelector
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

-- | @- newMode@
newMode :: IsMTRWaterHeaterModeClusterChangeToModeParams mtrWaterHeaterModeClusterChangeToModeParams => mtrWaterHeaterModeClusterChangeToModeParams -> IO (Id NSNumber)
newMode mtrWaterHeaterModeClusterChangeToModeParams =
  sendOwnedMessage mtrWaterHeaterModeClusterChangeToModeParams newModeSelector

-- | @- setNewMode:@
setNewMode :: (IsMTRWaterHeaterModeClusterChangeToModeParams mtrWaterHeaterModeClusterChangeToModeParams, IsNSNumber value) => mtrWaterHeaterModeClusterChangeToModeParams -> value -> IO ()
setNewMode mtrWaterHeaterModeClusterChangeToModeParams value =
  sendMessage mtrWaterHeaterModeClusterChangeToModeParams setNewModeSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRWaterHeaterModeClusterChangeToModeParams mtrWaterHeaterModeClusterChangeToModeParams => mtrWaterHeaterModeClusterChangeToModeParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrWaterHeaterModeClusterChangeToModeParams =
  sendMessage mtrWaterHeaterModeClusterChangeToModeParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRWaterHeaterModeClusterChangeToModeParams mtrWaterHeaterModeClusterChangeToModeParams, IsNSNumber value) => mtrWaterHeaterModeClusterChangeToModeParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrWaterHeaterModeClusterChangeToModeParams value =
  sendMessage mtrWaterHeaterModeClusterChangeToModeParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRWaterHeaterModeClusterChangeToModeParams mtrWaterHeaterModeClusterChangeToModeParams => mtrWaterHeaterModeClusterChangeToModeParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrWaterHeaterModeClusterChangeToModeParams =
  sendMessage mtrWaterHeaterModeClusterChangeToModeParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRWaterHeaterModeClusterChangeToModeParams mtrWaterHeaterModeClusterChangeToModeParams, IsNSNumber value) => mtrWaterHeaterModeClusterChangeToModeParams -> value -> IO ()
setServerSideProcessingTimeout mtrWaterHeaterModeClusterChangeToModeParams value =
  sendMessage mtrWaterHeaterModeClusterChangeToModeParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @newMode@
newModeSelector :: Selector '[] (Id NSNumber)
newModeSelector = mkSelector "newMode"

-- | @Selector@ for @setNewMode:@
setNewModeSelector :: Selector '[Id NSNumber] ()
setNewModeSelector = mkSelector "setNewMode:"

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

