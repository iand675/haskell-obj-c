{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRClosureControlClusterOverallTargetStateStruct@.
module ObjC.Matter.MTRClosureControlClusterOverallTargetStateStruct
  ( MTRClosureControlClusterOverallTargetStateStruct
  , IsMTRClosureControlClusterOverallTargetStateStruct(..)
  , position
  , setPosition
  , latch
  , setLatch
  , speed
  , setSpeed
  , latchSelector
  , positionSelector
  , setLatchSelector
  , setPositionSelector
  , setSpeedSelector
  , speedSelector


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
position :: IsMTRClosureControlClusterOverallTargetStateStruct mtrClosureControlClusterOverallTargetStateStruct => mtrClosureControlClusterOverallTargetStateStruct -> IO (Id NSNumber)
position mtrClosureControlClusterOverallTargetStateStruct =
  sendMessage mtrClosureControlClusterOverallTargetStateStruct positionSelector

-- | @- setPosition:@
setPosition :: (IsMTRClosureControlClusterOverallTargetStateStruct mtrClosureControlClusterOverallTargetStateStruct, IsNSNumber value) => mtrClosureControlClusterOverallTargetStateStruct -> value -> IO ()
setPosition mtrClosureControlClusterOverallTargetStateStruct value =
  sendMessage mtrClosureControlClusterOverallTargetStateStruct setPositionSelector (toNSNumber value)

-- | @- latch@
latch :: IsMTRClosureControlClusterOverallTargetStateStruct mtrClosureControlClusterOverallTargetStateStruct => mtrClosureControlClusterOverallTargetStateStruct -> IO (Id NSNumber)
latch mtrClosureControlClusterOverallTargetStateStruct =
  sendMessage mtrClosureControlClusterOverallTargetStateStruct latchSelector

-- | @- setLatch:@
setLatch :: (IsMTRClosureControlClusterOverallTargetStateStruct mtrClosureControlClusterOverallTargetStateStruct, IsNSNumber value) => mtrClosureControlClusterOverallTargetStateStruct -> value -> IO ()
setLatch mtrClosureControlClusterOverallTargetStateStruct value =
  sendMessage mtrClosureControlClusterOverallTargetStateStruct setLatchSelector (toNSNumber value)

-- | @- speed@
speed :: IsMTRClosureControlClusterOverallTargetStateStruct mtrClosureControlClusterOverallTargetStateStruct => mtrClosureControlClusterOverallTargetStateStruct -> IO (Id NSNumber)
speed mtrClosureControlClusterOverallTargetStateStruct =
  sendMessage mtrClosureControlClusterOverallTargetStateStruct speedSelector

-- | @- setSpeed:@
setSpeed :: (IsMTRClosureControlClusterOverallTargetStateStruct mtrClosureControlClusterOverallTargetStateStruct, IsNSNumber value) => mtrClosureControlClusterOverallTargetStateStruct -> value -> IO ()
setSpeed mtrClosureControlClusterOverallTargetStateStruct value =
  sendMessage mtrClosureControlClusterOverallTargetStateStruct setSpeedSelector (toNSNumber value)

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

