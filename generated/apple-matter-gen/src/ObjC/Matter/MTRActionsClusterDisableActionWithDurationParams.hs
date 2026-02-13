{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRActionsClusterDisableActionWithDurationParams@.
module ObjC.Matter.MTRActionsClusterDisableActionWithDurationParams
  ( MTRActionsClusterDisableActionWithDurationParams
  , IsMTRActionsClusterDisableActionWithDurationParams(..)
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
actionID :: IsMTRActionsClusterDisableActionWithDurationParams mtrActionsClusterDisableActionWithDurationParams => mtrActionsClusterDisableActionWithDurationParams -> IO (Id NSNumber)
actionID mtrActionsClusterDisableActionWithDurationParams =
  sendMessage mtrActionsClusterDisableActionWithDurationParams actionIDSelector

-- | @- setActionID:@
setActionID :: (IsMTRActionsClusterDisableActionWithDurationParams mtrActionsClusterDisableActionWithDurationParams, IsNSNumber value) => mtrActionsClusterDisableActionWithDurationParams -> value -> IO ()
setActionID mtrActionsClusterDisableActionWithDurationParams value =
  sendMessage mtrActionsClusterDisableActionWithDurationParams setActionIDSelector (toNSNumber value)

-- | @- invokeID@
invokeID :: IsMTRActionsClusterDisableActionWithDurationParams mtrActionsClusterDisableActionWithDurationParams => mtrActionsClusterDisableActionWithDurationParams -> IO (Id NSNumber)
invokeID mtrActionsClusterDisableActionWithDurationParams =
  sendMessage mtrActionsClusterDisableActionWithDurationParams invokeIDSelector

-- | @- setInvokeID:@
setInvokeID :: (IsMTRActionsClusterDisableActionWithDurationParams mtrActionsClusterDisableActionWithDurationParams, IsNSNumber value) => mtrActionsClusterDisableActionWithDurationParams -> value -> IO ()
setInvokeID mtrActionsClusterDisableActionWithDurationParams value =
  sendMessage mtrActionsClusterDisableActionWithDurationParams setInvokeIDSelector (toNSNumber value)

-- | @- duration@
duration :: IsMTRActionsClusterDisableActionWithDurationParams mtrActionsClusterDisableActionWithDurationParams => mtrActionsClusterDisableActionWithDurationParams -> IO (Id NSNumber)
duration mtrActionsClusterDisableActionWithDurationParams =
  sendMessage mtrActionsClusterDisableActionWithDurationParams durationSelector

-- | @- setDuration:@
setDuration :: (IsMTRActionsClusterDisableActionWithDurationParams mtrActionsClusterDisableActionWithDurationParams, IsNSNumber value) => mtrActionsClusterDisableActionWithDurationParams -> value -> IO ()
setDuration mtrActionsClusterDisableActionWithDurationParams value =
  sendMessage mtrActionsClusterDisableActionWithDurationParams setDurationSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRActionsClusterDisableActionWithDurationParams mtrActionsClusterDisableActionWithDurationParams => mtrActionsClusterDisableActionWithDurationParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrActionsClusterDisableActionWithDurationParams =
  sendMessage mtrActionsClusterDisableActionWithDurationParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRActionsClusterDisableActionWithDurationParams mtrActionsClusterDisableActionWithDurationParams, IsNSNumber value) => mtrActionsClusterDisableActionWithDurationParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrActionsClusterDisableActionWithDurationParams value =
  sendMessage mtrActionsClusterDisableActionWithDurationParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRActionsClusterDisableActionWithDurationParams mtrActionsClusterDisableActionWithDurationParams => mtrActionsClusterDisableActionWithDurationParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrActionsClusterDisableActionWithDurationParams =
  sendMessage mtrActionsClusterDisableActionWithDurationParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRActionsClusterDisableActionWithDurationParams mtrActionsClusterDisableActionWithDurationParams, IsNSNumber value) => mtrActionsClusterDisableActionWithDurationParams -> value -> IO ()
setServerSideProcessingTimeout mtrActionsClusterDisableActionWithDurationParams value =
  sendMessage mtrActionsClusterDisableActionWithDurationParams setServerSideProcessingTimeoutSelector (toNSNumber value)

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

