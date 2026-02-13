{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRBarrierControlClusterBarrierControlGoToPercentParams@.
module ObjC.Matter.MTRBarrierControlClusterBarrierControlGoToPercentParams
  ( MTRBarrierControlClusterBarrierControlGoToPercentParams
  , IsMTRBarrierControlClusterBarrierControlGoToPercentParams(..)
  , percentOpen
  , setPercentOpen
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , percentOpenSelector
  , serverSideProcessingTimeoutSelector
  , setPercentOpenSelector
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

-- | @- percentOpen@
percentOpen :: IsMTRBarrierControlClusterBarrierControlGoToPercentParams mtrBarrierControlClusterBarrierControlGoToPercentParams => mtrBarrierControlClusterBarrierControlGoToPercentParams -> IO (Id NSNumber)
percentOpen mtrBarrierControlClusterBarrierControlGoToPercentParams =
  sendMessage mtrBarrierControlClusterBarrierControlGoToPercentParams percentOpenSelector

-- | @- setPercentOpen:@
setPercentOpen :: (IsMTRBarrierControlClusterBarrierControlGoToPercentParams mtrBarrierControlClusterBarrierControlGoToPercentParams, IsNSNumber value) => mtrBarrierControlClusterBarrierControlGoToPercentParams -> value -> IO ()
setPercentOpen mtrBarrierControlClusterBarrierControlGoToPercentParams value =
  sendMessage mtrBarrierControlClusterBarrierControlGoToPercentParams setPercentOpenSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRBarrierControlClusterBarrierControlGoToPercentParams mtrBarrierControlClusterBarrierControlGoToPercentParams => mtrBarrierControlClusterBarrierControlGoToPercentParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrBarrierControlClusterBarrierControlGoToPercentParams =
  sendMessage mtrBarrierControlClusterBarrierControlGoToPercentParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRBarrierControlClusterBarrierControlGoToPercentParams mtrBarrierControlClusterBarrierControlGoToPercentParams, IsNSNumber value) => mtrBarrierControlClusterBarrierControlGoToPercentParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrBarrierControlClusterBarrierControlGoToPercentParams value =
  sendMessage mtrBarrierControlClusterBarrierControlGoToPercentParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRBarrierControlClusterBarrierControlGoToPercentParams mtrBarrierControlClusterBarrierControlGoToPercentParams => mtrBarrierControlClusterBarrierControlGoToPercentParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrBarrierControlClusterBarrierControlGoToPercentParams =
  sendMessage mtrBarrierControlClusterBarrierControlGoToPercentParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRBarrierControlClusterBarrierControlGoToPercentParams mtrBarrierControlClusterBarrierControlGoToPercentParams, IsNSNumber value) => mtrBarrierControlClusterBarrierControlGoToPercentParams -> value -> IO ()
setServerSideProcessingTimeout mtrBarrierControlClusterBarrierControlGoToPercentParams value =
  sendMessage mtrBarrierControlClusterBarrierControlGoToPercentParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @percentOpen@
percentOpenSelector :: Selector '[] (Id NSNumber)
percentOpenSelector = mkSelector "percentOpen"

-- | @Selector@ for @setPercentOpen:@
setPercentOpenSelector :: Selector '[Id NSNumber] ()
setPercentOpenSelector = mkSelector "setPercentOpen:"

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

