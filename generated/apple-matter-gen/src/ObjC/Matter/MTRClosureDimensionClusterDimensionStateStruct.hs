{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRClosureDimensionClusterDimensionStateStruct@.
module ObjC.Matter.MTRClosureDimensionClusterDimensionStateStruct
  ( MTRClosureDimensionClusterDimensionStateStruct
  , IsMTRClosureDimensionClusterDimensionStateStruct(..)
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
position :: IsMTRClosureDimensionClusterDimensionStateStruct mtrClosureDimensionClusterDimensionStateStruct => mtrClosureDimensionClusterDimensionStateStruct -> IO (Id NSNumber)
position mtrClosureDimensionClusterDimensionStateStruct =
  sendMessage mtrClosureDimensionClusterDimensionStateStruct positionSelector

-- | @- setPosition:@
setPosition :: (IsMTRClosureDimensionClusterDimensionStateStruct mtrClosureDimensionClusterDimensionStateStruct, IsNSNumber value) => mtrClosureDimensionClusterDimensionStateStruct -> value -> IO ()
setPosition mtrClosureDimensionClusterDimensionStateStruct value =
  sendMessage mtrClosureDimensionClusterDimensionStateStruct setPositionSelector (toNSNumber value)

-- | @- latch@
latch :: IsMTRClosureDimensionClusterDimensionStateStruct mtrClosureDimensionClusterDimensionStateStruct => mtrClosureDimensionClusterDimensionStateStruct -> IO (Id NSNumber)
latch mtrClosureDimensionClusterDimensionStateStruct =
  sendMessage mtrClosureDimensionClusterDimensionStateStruct latchSelector

-- | @- setLatch:@
setLatch :: (IsMTRClosureDimensionClusterDimensionStateStruct mtrClosureDimensionClusterDimensionStateStruct, IsNSNumber value) => mtrClosureDimensionClusterDimensionStateStruct -> value -> IO ()
setLatch mtrClosureDimensionClusterDimensionStateStruct value =
  sendMessage mtrClosureDimensionClusterDimensionStateStruct setLatchSelector (toNSNumber value)

-- | @- speed@
speed :: IsMTRClosureDimensionClusterDimensionStateStruct mtrClosureDimensionClusterDimensionStateStruct => mtrClosureDimensionClusterDimensionStateStruct -> IO (Id NSNumber)
speed mtrClosureDimensionClusterDimensionStateStruct =
  sendMessage mtrClosureDimensionClusterDimensionStateStruct speedSelector

-- | @- setSpeed:@
setSpeed :: (IsMTRClosureDimensionClusterDimensionStateStruct mtrClosureDimensionClusterDimensionStateStruct, IsNSNumber value) => mtrClosureDimensionClusterDimensionStateStruct -> value -> IO ()
setSpeed mtrClosureDimensionClusterDimensionStateStruct value =
  sendMessage mtrClosureDimensionClusterDimensionStateStruct setSpeedSelector (toNSNumber value)

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

