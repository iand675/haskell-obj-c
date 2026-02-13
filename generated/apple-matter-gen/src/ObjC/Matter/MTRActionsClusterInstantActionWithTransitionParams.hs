{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRActionsClusterInstantActionWithTransitionParams@.
module ObjC.Matter.MTRActionsClusterInstantActionWithTransitionParams
  ( MTRActionsClusterInstantActionWithTransitionParams
  , IsMTRActionsClusterInstantActionWithTransitionParams(..)
  , actionID
  , setActionID
  , invokeID
  , setInvokeID
  , transitionTime
  , setTransitionTime
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , actionIDSelector
  , invokeIDSelector
  , serverSideProcessingTimeoutSelector
  , setActionIDSelector
  , setInvokeIDSelector
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

-- | @- actionID@
actionID :: IsMTRActionsClusterInstantActionWithTransitionParams mtrActionsClusterInstantActionWithTransitionParams => mtrActionsClusterInstantActionWithTransitionParams -> IO (Id NSNumber)
actionID mtrActionsClusterInstantActionWithTransitionParams =
  sendMessage mtrActionsClusterInstantActionWithTransitionParams actionIDSelector

-- | @- setActionID:@
setActionID :: (IsMTRActionsClusterInstantActionWithTransitionParams mtrActionsClusterInstantActionWithTransitionParams, IsNSNumber value) => mtrActionsClusterInstantActionWithTransitionParams -> value -> IO ()
setActionID mtrActionsClusterInstantActionWithTransitionParams value =
  sendMessage mtrActionsClusterInstantActionWithTransitionParams setActionIDSelector (toNSNumber value)

-- | @- invokeID@
invokeID :: IsMTRActionsClusterInstantActionWithTransitionParams mtrActionsClusterInstantActionWithTransitionParams => mtrActionsClusterInstantActionWithTransitionParams -> IO (Id NSNumber)
invokeID mtrActionsClusterInstantActionWithTransitionParams =
  sendMessage mtrActionsClusterInstantActionWithTransitionParams invokeIDSelector

-- | @- setInvokeID:@
setInvokeID :: (IsMTRActionsClusterInstantActionWithTransitionParams mtrActionsClusterInstantActionWithTransitionParams, IsNSNumber value) => mtrActionsClusterInstantActionWithTransitionParams -> value -> IO ()
setInvokeID mtrActionsClusterInstantActionWithTransitionParams value =
  sendMessage mtrActionsClusterInstantActionWithTransitionParams setInvokeIDSelector (toNSNumber value)

-- | @- transitionTime@
transitionTime :: IsMTRActionsClusterInstantActionWithTransitionParams mtrActionsClusterInstantActionWithTransitionParams => mtrActionsClusterInstantActionWithTransitionParams -> IO (Id NSNumber)
transitionTime mtrActionsClusterInstantActionWithTransitionParams =
  sendMessage mtrActionsClusterInstantActionWithTransitionParams transitionTimeSelector

-- | @- setTransitionTime:@
setTransitionTime :: (IsMTRActionsClusterInstantActionWithTransitionParams mtrActionsClusterInstantActionWithTransitionParams, IsNSNumber value) => mtrActionsClusterInstantActionWithTransitionParams -> value -> IO ()
setTransitionTime mtrActionsClusterInstantActionWithTransitionParams value =
  sendMessage mtrActionsClusterInstantActionWithTransitionParams setTransitionTimeSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRActionsClusterInstantActionWithTransitionParams mtrActionsClusterInstantActionWithTransitionParams => mtrActionsClusterInstantActionWithTransitionParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrActionsClusterInstantActionWithTransitionParams =
  sendMessage mtrActionsClusterInstantActionWithTransitionParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRActionsClusterInstantActionWithTransitionParams mtrActionsClusterInstantActionWithTransitionParams, IsNSNumber value) => mtrActionsClusterInstantActionWithTransitionParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrActionsClusterInstantActionWithTransitionParams value =
  sendMessage mtrActionsClusterInstantActionWithTransitionParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRActionsClusterInstantActionWithTransitionParams mtrActionsClusterInstantActionWithTransitionParams => mtrActionsClusterInstantActionWithTransitionParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrActionsClusterInstantActionWithTransitionParams =
  sendMessage mtrActionsClusterInstantActionWithTransitionParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRActionsClusterInstantActionWithTransitionParams mtrActionsClusterInstantActionWithTransitionParams, IsNSNumber value) => mtrActionsClusterInstantActionWithTransitionParams -> value -> IO ()
setServerSideProcessingTimeout mtrActionsClusterInstantActionWithTransitionParams value =
  sendMessage mtrActionsClusterInstantActionWithTransitionParams setServerSideProcessingTimeoutSelector (toNSNumber value)

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

-- | @Selector@ for @transitionTime@
transitionTimeSelector :: Selector '[] (Id NSNumber)
transitionTimeSelector = mkSelector "transitionTime"

-- | @Selector@ for @setTransitionTime:@
setTransitionTimeSelector :: Selector '[Id NSNumber] ()
setTransitionTimeSelector = mkSelector "setTransitionTime:"

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

