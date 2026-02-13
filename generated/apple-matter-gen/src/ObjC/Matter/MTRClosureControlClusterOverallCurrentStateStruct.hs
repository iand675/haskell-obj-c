{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRClosureControlClusterOverallCurrentStateStruct@.
module ObjC.Matter.MTRClosureControlClusterOverallCurrentStateStruct
  ( MTRClosureControlClusterOverallCurrentStateStruct
  , IsMTRClosureControlClusterOverallCurrentStateStruct(..)
  , position
  , setPosition
  , latch
  , setLatch
  , speed
  , setSpeed
  , secureState
  , setSecureState
  , latchSelector
  , positionSelector
  , secureStateSelector
  , setLatchSelector
  , setPositionSelector
  , setSecureStateSelector
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
position :: IsMTRClosureControlClusterOverallCurrentStateStruct mtrClosureControlClusterOverallCurrentStateStruct => mtrClosureControlClusterOverallCurrentStateStruct -> IO (Id NSNumber)
position mtrClosureControlClusterOverallCurrentStateStruct =
  sendMessage mtrClosureControlClusterOverallCurrentStateStruct positionSelector

-- | @- setPosition:@
setPosition :: (IsMTRClosureControlClusterOverallCurrentStateStruct mtrClosureControlClusterOverallCurrentStateStruct, IsNSNumber value) => mtrClosureControlClusterOverallCurrentStateStruct -> value -> IO ()
setPosition mtrClosureControlClusterOverallCurrentStateStruct value =
  sendMessage mtrClosureControlClusterOverallCurrentStateStruct setPositionSelector (toNSNumber value)

-- | @- latch@
latch :: IsMTRClosureControlClusterOverallCurrentStateStruct mtrClosureControlClusterOverallCurrentStateStruct => mtrClosureControlClusterOverallCurrentStateStruct -> IO (Id NSNumber)
latch mtrClosureControlClusterOverallCurrentStateStruct =
  sendMessage mtrClosureControlClusterOverallCurrentStateStruct latchSelector

-- | @- setLatch:@
setLatch :: (IsMTRClosureControlClusterOverallCurrentStateStruct mtrClosureControlClusterOverallCurrentStateStruct, IsNSNumber value) => mtrClosureControlClusterOverallCurrentStateStruct -> value -> IO ()
setLatch mtrClosureControlClusterOverallCurrentStateStruct value =
  sendMessage mtrClosureControlClusterOverallCurrentStateStruct setLatchSelector (toNSNumber value)

-- | @- speed@
speed :: IsMTRClosureControlClusterOverallCurrentStateStruct mtrClosureControlClusterOverallCurrentStateStruct => mtrClosureControlClusterOverallCurrentStateStruct -> IO (Id NSNumber)
speed mtrClosureControlClusterOverallCurrentStateStruct =
  sendMessage mtrClosureControlClusterOverallCurrentStateStruct speedSelector

-- | @- setSpeed:@
setSpeed :: (IsMTRClosureControlClusterOverallCurrentStateStruct mtrClosureControlClusterOverallCurrentStateStruct, IsNSNumber value) => mtrClosureControlClusterOverallCurrentStateStruct -> value -> IO ()
setSpeed mtrClosureControlClusterOverallCurrentStateStruct value =
  sendMessage mtrClosureControlClusterOverallCurrentStateStruct setSpeedSelector (toNSNumber value)

-- | @- secureState@
secureState :: IsMTRClosureControlClusterOverallCurrentStateStruct mtrClosureControlClusterOverallCurrentStateStruct => mtrClosureControlClusterOverallCurrentStateStruct -> IO (Id NSNumber)
secureState mtrClosureControlClusterOverallCurrentStateStruct =
  sendMessage mtrClosureControlClusterOverallCurrentStateStruct secureStateSelector

-- | @- setSecureState:@
setSecureState :: (IsMTRClosureControlClusterOverallCurrentStateStruct mtrClosureControlClusterOverallCurrentStateStruct, IsNSNumber value) => mtrClosureControlClusterOverallCurrentStateStruct -> value -> IO ()
setSecureState mtrClosureControlClusterOverallCurrentStateStruct value =
  sendMessage mtrClosureControlClusterOverallCurrentStateStruct setSecureStateSelector (toNSNumber value)

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

-- | @Selector@ for @secureState@
secureStateSelector :: Selector '[] (Id NSNumber)
secureStateSelector = mkSelector "secureState"

-- | @Selector@ for @setSecureState:@
setSecureStateSelector :: Selector '[Id NSNumber] ()
setSecureStateSelector = mkSelector "setSecureState:"

