{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRModeSelectClusterChangeToModeParams@.
module ObjC.Matter.MTRModeSelectClusterChangeToModeParams
  ( MTRModeSelectClusterChangeToModeParams
  , IsMTRModeSelectClusterChangeToModeParams(..)
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
newMode :: IsMTRModeSelectClusterChangeToModeParams mtrModeSelectClusterChangeToModeParams => mtrModeSelectClusterChangeToModeParams -> IO (Id NSNumber)
newMode mtrModeSelectClusterChangeToModeParams =
  sendOwnedMessage mtrModeSelectClusterChangeToModeParams newModeSelector

-- | @- setNewMode:@
setNewMode :: (IsMTRModeSelectClusterChangeToModeParams mtrModeSelectClusterChangeToModeParams, IsNSNumber value) => mtrModeSelectClusterChangeToModeParams -> value -> IO ()
setNewMode mtrModeSelectClusterChangeToModeParams value =
  sendMessage mtrModeSelectClusterChangeToModeParams setNewModeSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRModeSelectClusterChangeToModeParams mtrModeSelectClusterChangeToModeParams => mtrModeSelectClusterChangeToModeParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrModeSelectClusterChangeToModeParams =
  sendMessage mtrModeSelectClusterChangeToModeParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRModeSelectClusterChangeToModeParams mtrModeSelectClusterChangeToModeParams, IsNSNumber value) => mtrModeSelectClusterChangeToModeParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrModeSelectClusterChangeToModeParams value =
  sendMessage mtrModeSelectClusterChangeToModeParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRModeSelectClusterChangeToModeParams mtrModeSelectClusterChangeToModeParams => mtrModeSelectClusterChangeToModeParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrModeSelectClusterChangeToModeParams =
  sendMessage mtrModeSelectClusterChangeToModeParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRModeSelectClusterChangeToModeParams mtrModeSelectClusterChangeToModeParams, IsNSNumber value) => mtrModeSelectClusterChangeToModeParams -> value -> IO ()
setServerSideProcessingTimeout mtrModeSelectClusterChangeToModeParams value =
  sendMessage mtrModeSelectClusterChangeToModeParams setServerSideProcessingTimeoutSelector (toNSNumber value)

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

