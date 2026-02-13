{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRActionsClusterEnableActionWithDurationParams@.
module ObjC.Matter.MTRActionsClusterEnableActionWithDurationParams
  ( MTRActionsClusterEnableActionWithDurationParams
  , IsMTRActionsClusterEnableActionWithDurationParams(..)
  , actionID
  , setActionID
  , invokeID
  , setInvokeID
  , duration
  , setDuration
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , actionIDSelector
  , durationSelector
  , invokeIDSelector
  , serverSideProcessingTimeoutSelector
  , setActionIDSelector
  , setDurationSelector
  , setInvokeIDSelector
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

-- | @- actionID@
actionID :: IsMTRActionsClusterEnableActionWithDurationParams mtrActionsClusterEnableActionWithDurationParams => mtrActionsClusterEnableActionWithDurationParams -> IO (Id NSNumber)
actionID mtrActionsClusterEnableActionWithDurationParams =
  sendMessage mtrActionsClusterEnableActionWithDurationParams actionIDSelector

-- | @- setActionID:@
setActionID :: (IsMTRActionsClusterEnableActionWithDurationParams mtrActionsClusterEnableActionWithDurationParams, IsNSNumber value) => mtrActionsClusterEnableActionWithDurationParams -> value -> IO ()
setActionID mtrActionsClusterEnableActionWithDurationParams value =
  sendMessage mtrActionsClusterEnableActionWithDurationParams setActionIDSelector (toNSNumber value)

-- | @- invokeID@
invokeID :: IsMTRActionsClusterEnableActionWithDurationParams mtrActionsClusterEnableActionWithDurationParams => mtrActionsClusterEnableActionWithDurationParams -> IO (Id NSNumber)
invokeID mtrActionsClusterEnableActionWithDurationParams =
  sendMessage mtrActionsClusterEnableActionWithDurationParams invokeIDSelector

-- | @- setInvokeID:@
setInvokeID :: (IsMTRActionsClusterEnableActionWithDurationParams mtrActionsClusterEnableActionWithDurationParams, IsNSNumber value) => mtrActionsClusterEnableActionWithDurationParams -> value -> IO ()
setInvokeID mtrActionsClusterEnableActionWithDurationParams value =
  sendMessage mtrActionsClusterEnableActionWithDurationParams setInvokeIDSelector (toNSNumber value)

-- | @- duration@
duration :: IsMTRActionsClusterEnableActionWithDurationParams mtrActionsClusterEnableActionWithDurationParams => mtrActionsClusterEnableActionWithDurationParams -> IO (Id NSNumber)
duration mtrActionsClusterEnableActionWithDurationParams =
  sendMessage mtrActionsClusterEnableActionWithDurationParams durationSelector

-- | @- setDuration:@
setDuration :: (IsMTRActionsClusterEnableActionWithDurationParams mtrActionsClusterEnableActionWithDurationParams, IsNSNumber value) => mtrActionsClusterEnableActionWithDurationParams -> value -> IO ()
setDuration mtrActionsClusterEnableActionWithDurationParams value =
  sendMessage mtrActionsClusterEnableActionWithDurationParams setDurationSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRActionsClusterEnableActionWithDurationParams mtrActionsClusterEnableActionWithDurationParams => mtrActionsClusterEnableActionWithDurationParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrActionsClusterEnableActionWithDurationParams =
  sendMessage mtrActionsClusterEnableActionWithDurationParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRActionsClusterEnableActionWithDurationParams mtrActionsClusterEnableActionWithDurationParams, IsNSNumber value) => mtrActionsClusterEnableActionWithDurationParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrActionsClusterEnableActionWithDurationParams value =
  sendMessage mtrActionsClusterEnableActionWithDurationParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRActionsClusterEnableActionWithDurationParams mtrActionsClusterEnableActionWithDurationParams => mtrActionsClusterEnableActionWithDurationParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrActionsClusterEnableActionWithDurationParams =
  sendMessage mtrActionsClusterEnableActionWithDurationParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRActionsClusterEnableActionWithDurationParams mtrActionsClusterEnableActionWithDurationParams, IsNSNumber value) => mtrActionsClusterEnableActionWithDurationParams -> value -> IO ()
setServerSideProcessingTimeout mtrActionsClusterEnableActionWithDurationParams value =
  sendMessage mtrActionsClusterEnableActionWithDurationParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @actionID@
actionIDSelector :: Selector '[] (Id NSNumber)
actionIDSelector = mkSelector "actionID"

-- | @Selector@ for @setActionID:@
setActionIDSelector :: Selector '[Id NSNumber] ()
setActionIDSelector = mkSelector "setActionID:"

-- | @Selector@ for @invokeID@
invokeIDSelector :: Selector '[] (Id NSNumber)
invokeIDSelector = mkSelector "invokeID"

-- | @Selector@ for @setInvokeID:@
setInvokeIDSelector :: Selector '[Id NSNumber] ()
setInvokeIDSelector = mkSelector "setInvokeID:"

-- | @Selector@ for @duration@
durationSelector :: Selector '[] (Id NSNumber)
durationSelector = mkSelector "duration"

-- | @Selector@ for @setDuration:@
setDurationSelector :: Selector '[Id NSNumber] ()
setDurationSelector = mkSelector "setDuration:"

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

