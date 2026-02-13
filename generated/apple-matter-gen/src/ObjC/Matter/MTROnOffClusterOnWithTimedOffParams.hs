{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROnOffClusterOnWithTimedOffParams@.
module ObjC.Matter.MTROnOffClusterOnWithTimedOffParams
  ( MTROnOffClusterOnWithTimedOffParams
  , IsMTROnOffClusterOnWithTimedOffParams(..)
  , onOffControl
  , setOnOffControl
  , onTime
  , setOnTime
  , offWaitTime
  , setOffWaitTime
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , offWaitTimeSelector
  , onOffControlSelector
  , onTimeSelector
  , serverSideProcessingTimeoutSelector
  , setOffWaitTimeSelector
  , setOnOffControlSelector
  , setOnTimeSelector
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

-- | @- onOffControl@
onOffControl :: IsMTROnOffClusterOnWithTimedOffParams mtrOnOffClusterOnWithTimedOffParams => mtrOnOffClusterOnWithTimedOffParams -> IO (Id NSNumber)
onOffControl mtrOnOffClusterOnWithTimedOffParams =
  sendMessage mtrOnOffClusterOnWithTimedOffParams onOffControlSelector

-- | @- setOnOffControl:@
setOnOffControl :: (IsMTROnOffClusterOnWithTimedOffParams mtrOnOffClusterOnWithTimedOffParams, IsNSNumber value) => mtrOnOffClusterOnWithTimedOffParams -> value -> IO ()
setOnOffControl mtrOnOffClusterOnWithTimedOffParams value =
  sendMessage mtrOnOffClusterOnWithTimedOffParams setOnOffControlSelector (toNSNumber value)

-- | @- onTime@
onTime :: IsMTROnOffClusterOnWithTimedOffParams mtrOnOffClusterOnWithTimedOffParams => mtrOnOffClusterOnWithTimedOffParams -> IO (Id NSNumber)
onTime mtrOnOffClusterOnWithTimedOffParams =
  sendMessage mtrOnOffClusterOnWithTimedOffParams onTimeSelector

-- | @- setOnTime:@
setOnTime :: (IsMTROnOffClusterOnWithTimedOffParams mtrOnOffClusterOnWithTimedOffParams, IsNSNumber value) => mtrOnOffClusterOnWithTimedOffParams -> value -> IO ()
setOnTime mtrOnOffClusterOnWithTimedOffParams value =
  sendMessage mtrOnOffClusterOnWithTimedOffParams setOnTimeSelector (toNSNumber value)

-- | @- offWaitTime@
offWaitTime :: IsMTROnOffClusterOnWithTimedOffParams mtrOnOffClusterOnWithTimedOffParams => mtrOnOffClusterOnWithTimedOffParams -> IO (Id NSNumber)
offWaitTime mtrOnOffClusterOnWithTimedOffParams =
  sendMessage mtrOnOffClusterOnWithTimedOffParams offWaitTimeSelector

-- | @- setOffWaitTime:@
setOffWaitTime :: (IsMTROnOffClusterOnWithTimedOffParams mtrOnOffClusterOnWithTimedOffParams, IsNSNumber value) => mtrOnOffClusterOnWithTimedOffParams -> value -> IO ()
setOffWaitTime mtrOnOffClusterOnWithTimedOffParams value =
  sendMessage mtrOnOffClusterOnWithTimedOffParams setOffWaitTimeSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTROnOffClusterOnWithTimedOffParams mtrOnOffClusterOnWithTimedOffParams => mtrOnOffClusterOnWithTimedOffParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrOnOffClusterOnWithTimedOffParams =
  sendMessage mtrOnOffClusterOnWithTimedOffParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTROnOffClusterOnWithTimedOffParams mtrOnOffClusterOnWithTimedOffParams, IsNSNumber value) => mtrOnOffClusterOnWithTimedOffParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrOnOffClusterOnWithTimedOffParams value =
  sendMessage mtrOnOffClusterOnWithTimedOffParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTROnOffClusterOnWithTimedOffParams mtrOnOffClusterOnWithTimedOffParams => mtrOnOffClusterOnWithTimedOffParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrOnOffClusterOnWithTimedOffParams =
  sendMessage mtrOnOffClusterOnWithTimedOffParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTROnOffClusterOnWithTimedOffParams mtrOnOffClusterOnWithTimedOffParams, IsNSNumber value) => mtrOnOffClusterOnWithTimedOffParams -> value -> IO ()
setServerSideProcessingTimeout mtrOnOffClusterOnWithTimedOffParams value =
  sendMessage mtrOnOffClusterOnWithTimedOffParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @onOffControl@
onOffControlSelector :: Selector '[] (Id NSNumber)
onOffControlSelector = mkSelector "onOffControl"

-- | @Selector@ for @setOnOffControl:@
setOnOffControlSelector :: Selector '[Id NSNumber] ()
setOnOffControlSelector = mkSelector "setOnOffControl:"

-- | @Selector@ for @onTime@
onTimeSelector :: Selector '[] (Id NSNumber)
onTimeSelector = mkSelector "onTime"

-- | @Selector@ for @setOnTime:@
setOnTimeSelector :: Selector '[Id NSNumber] ()
setOnTimeSelector = mkSelector "setOnTime:"

-- | @Selector@ for @offWaitTime@
offWaitTimeSelector :: Selector '[] (Id NSNumber)
offWaitTimeSelector = mkSelector "offWaitTime"

-- | @Selector@ for @setOffWaitTime:@
setOffWaitTimeSelector :: Selector '[Id NSNumber] ()
setOffWaitTimeSelector = mkSelector "setOffWaitTime:"

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

