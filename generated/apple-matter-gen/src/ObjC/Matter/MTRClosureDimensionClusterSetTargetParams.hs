{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRClosureDimensionClusterSetTargetParams@.
module ObjC.Matter.MTRClosureDimensionClusterSetTargetParams
  ( MTRClosureDimensionClusterSetTargetParams
  , IsMTRClosureDimensionClusterSetTargetParams(..)
  , position
  , setPosition
  , latch
  , setLatch
  , speed
  , setSpeed
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , latchSelector
  , positionSelector
  , serverSideProcessingTimeoutSelector
  , setLatchSelector
  , setPositionSelector
  , setServerSideProcessingTimeoutSelector
  , setSpeedSelector
  , setTimedInvokeTimeoutMsSelector
  , speedSelector
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

-- | @- position@
position :: IsMTRClosureDimensionClusterSetTargetParams mtrClosureDimensionClusterSetTargetParams => mtrClosureDimensionClusterSetTargetParams -> IO (Id NSNumber)
position mtrClosureDimensionClusterSetTargetParams =
  sendMessage mtrClosureDimensionClusterSetTargetParams positionSelector

-- | @- setPosition:@
setPosition :: (IsMTRClosureDimensionClusterSetTargetParams mtrClosureDimensionClusterSetTargetParams, IsNSNumber value) => mtrClosureDimensionClusterSetTargetParams -> value -> IO ()
setPosition mtrClosureDimensionClusterSetTargetParams value =
  sendMessage mtrClosureDimensionClusterSetTargetParams setPositionSelector (toNSNumber value)

-- | @- latch@
latch :: IsMTRClosureDimensionClusterSetTargetParams mtrClosureDimensionClusterSetTargetParams => mtrClosureDimensionClusterSetTargetParams -> IO (Id NSNumber)
latch mtrClosureDimensionClusterSetTargetParams =
  sendMessage mtrClosureDimensionClusterSetTargetParams latchSelector

-- | @- setLatch:@
setLatch :: (IsMTRClosureDimensionClusterSetTargetParams mtrClosureDimensionClusterSetTargetParams, IsNSNumber value) => mtrClosureDimensionClusterSetTargetParams -> value -> IO ()
setLatch mtrClosureDimensionClusterSetTargetParams value =
  sendMessage mtrClosureDimensionClusterSetTargetParams setLatchSelector (toNSNumber value)

-- | @- speed@
speed :: IsMTRClosureDimensionClusterSetTargetParams mtrClosureDimensionClusterSetTargetParams => mtrClosureDimensionClusterSetTargetParams -> IO (Id NSNumber)
speed mtrClosureDimensionClusterSetTargetParams =
  sendMessage mtrClosureDimensionClusterSetTargetParams speedSelector

-- | @- setSpeed:@
setSpeed :: (IsMTRClosureDimensionClusterSetTargetParams mtrClosureDimensionClusterSetTargetParams, IsNSNumber value) => mtrClosureDimensionClusterSetTargetParams -> value -> IO ()
setSpeed mtrClosureDimensionClusterSetTargetParams value =
  sendMessage mtrClosureDimensionClusterSetTargetParams setSpeedSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRClosureDimensionClusterSetTargetParams mtrClosureDimensionClusterSetTargetParams => mtrClosureDimensionClusterSetTargetParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrClosureDimensionClusterSetTargetParams =
  sendMessage mtrClosureDimensionClusterSetTargetParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRClosureDimensionClusterSetTargetParams mtrClosureDimensionClusterSetTargetParams, IsNSNumber value) => mtrClosureDimensionClusterSetTargetParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrClosureDimensionClusterSetTargetParams value =
  sendMessage mtrClosureDimensionClusterSetTargetParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRClosureDimensionClusterSetTargetParams mtrClosureDimensionClusterSetTargetParams => mtrClosureDimensionClusterSetTargetParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrClosureDimensionClusterSetTargetParams =
  sendMessage mtrClosureDimensionClusterSetTargetParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRClosureDimensionClusterSetTargetParams mtrClosureDimensionClusterSetTargetParams, IsNSNumber value) => mtrClosureDimensionClusterSetTargetParams -> value -> IO ()
setServerSideProcessingTimeout mtrClosureDimensionClusterSetTargetParams value =
  sendMessage mtrClosureDimensionClusterSetTargetParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @position@
positionSelector :: Selector '[] (Id NSNumber)
positionSelector = mkSelector "position"

-- | @Selector@ for @setPosition:@
setPositionSelector :: Selector '[Id NSNumber] ()
setPositionSelector = mkSelector "setPosition:"

-- | @Selector@ for @latch@
latchSelector :: Selector '[] (Id NSNumber)
latchSelector = mkSelector "latch"

-- | @Selector@ for @setLatch:@
setLatchSelector :: Selector '[Id NSNumber] ()
setLatchSelector = mkSelector "setLatch:"

-- | @Selector@ for @speed@
speedSelector :: Selector '[] (Id NSNumber)
speedSelector = mkSelector "speed"

-- | @Selector@ for @setSpeed:@
setSpeedSelector :: Selector '[Id NSNumber] ()
setSpeedSelector = mkSelector "setSpeed:"

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

