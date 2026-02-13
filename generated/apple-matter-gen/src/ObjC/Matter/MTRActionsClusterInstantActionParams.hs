{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRActionsClusterInstantActionParams@.
module ObjC.Matter.MTRActionsClusterInstantActionParams
  ( MTRActionsClusterInstantActionParams
  , IsMTRActionsClusterInstantActionParams(..)
  , actionID
  , setActionID
  , invokeID
  , setInvokeID
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
actionID :: IsMTRActionsClusterInstantActionParams mtrActionsClusterInstantActionParams => mtrActionsClusterInstantActionParams -> IO (Id NSNumber)
actionID mtrActionsClusterInstantActionParams =
  sendMessage mtrActionsClusterInstantActionParams actionIDSelector

-- | @- setActionID:@
setActionID :: (IsMTRActionsClusterInstantActionParams mtrActionsClusterInstantActionParams, IsNSNumber value) => mtrActionsClusterInstantActionParams -> value -> IO ()
setActionID mtrActionsClusterInstantActionParams value =
  sendMessage mtrActionsClusterInstantActionParams setActionIDSelector (toNSNumber value)

-- | @- invokeID@
invokeID :: IsMTRActionsClusterInstantActionParams mtrActionsClusterInstantActionParams => mtrActionsClusterInstantActionParams -> IO (Id NSNumber)
invokeID mtrActionsClusterInstantActionParams =
  sendMessage mtrActionsClusterInstantActionParams invokeIDSelector

-- | @- setInvokeID:@
setInvokeID :: (IsMTRActionsClusterInstantActionParams mtrActionsClusterInstantActionParams, IsNSNumber value) => mtrActionsClusterInstantActionParams -> value -> IO ()
setInvokeID mtrActionsClusterInstantActionParams value =
  sendMessage mtrActionsClusterInstantActionParams setInvokeIDSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRActionsClusterInstantActionParams mtrActionsClusterInstantActionParams => mtrActionsClusterInstantActionParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrActionsClusterInstantActionParams =
  sendMessage mtrActionsClusterInstantActionParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRActionsClusterInstantActionParams mtrActionsClusterInstantActionParams, IsNSNumber value) => mtrActionsClusterInstantActionParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrActionsClusterInstantActionParams value =
  sendMessage mtrActionsClusterInstantActionParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRActionsClusterInstantActionParams mtrActionsClusterInstantActionParams => mtrActionsClusterInstantActionParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrActionsClusterInstantActionParams =
  sendMessage mtrActionsClusterInstantActionParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRActionsClusterInstantActionParams mtrActionsClusterInstantActionParams, IsNSNumber value) => mtrActionsClusterInstantActionParams -> value -> IO ()
setServerSideProcessingTimeout mtrActionsClusterInstantActionParams value =
  sendMessage mtrActionsClusterInstantActionParams setServerSideProcessingTimeoutSelector (toNSNumber value)

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

