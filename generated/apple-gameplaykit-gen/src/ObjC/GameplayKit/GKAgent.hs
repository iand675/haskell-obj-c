{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An agent is a point mass whose local coordinate system is aligned to its velocity.  Agents have a variety of steering functions that can be used to simulate vehicles or entities with agency. The units of mass, velocity and radius are dimensionless but related. The visual representation of these values are specific to each game's own situation.
--
-- Values close to 1.0 should be canonical and are expected to yield pleasing results. When applied to visuals these values should be scaled and biased into their target coordinate system and a simple filter on top ensures any noise generated from the steering logic doesn't affect the visual represtentation.
--
-- Generated bindings for @GKAgent@.
module ObjC.GameplayKit.GKAgent
  ( GKAgent
  , IsGKAgent(..)
  , delegate
  , setDelegate
  , behavior
  , setBehavior
  , mass
  , setMass
  , radius
  , setRadius
  , speed
  , setSpeed
  , maxAcceleration
  , setMaxAcceleration
  , maxSpeed
  , setMaxSpeed
  , behaviorSelector
  , delegateSelector
  , massSelector
  , maxAccelerationSelector
  , maxSpeedSelector
  , radiusSelector
  , setBehaviorSelector
  , setDelegateSelector
  , setMassSelector
  , setMaxAccelerationSelector
  , setMaxSpeedSelector
  , setRadiusSelector
  , setSpeedSelector
  , speedSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameplayKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Object which has agentDidUpdate called on it during this agent's behavior updatekbeha
--
-- ObjC selector: @- delegate@
delegate :: IsGKAgent gkAgent => gkAgent -> IO RawId
delegate gkAgent =
  sendMessage gkAgent delegateSelector

-- | Object which has agentDidUpdate called on it during this agent's behavior updatekbeha
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsGKAgent gkAgent => gkAgent -> RawId -> IO ()
setDelegate gkAgent value =
  sendMessage gkAgent setDelegateSelector value

-- | The behavior to apply when updateWithDeltaTime is called. All forces from the goals in the behavior are summed and then applied.
--
-- ObjC selector: @- behavior@
behavior :: IsGKAgent gkAgent => gkAgent -> IO (Id GKBehavior)
behavior gkAgent =
  sendMessage gkAgent behaviorSelector

-- | The behavior to apply when updateWithDeltaTime is called. All forces from the goals in the behavior are summed and then applied.
--
-- ObjC selector: @- setBehavior:@
setBehavior :: (IsGKAgent gkAgent, IsGKBehavior value) => gkAgent -> value -> IO ()
setBehavior gkAgent value =
  sendMessage gkAgent setBehaviorSelector (toGKBehavior value)

-- | Agent's mass. Used for agent impulse application purposes.
--
-- Defaults to 1.0
--
-- ObjC selector: @- mass@
mass :: IsGKAgent gkAgent => gkAgent -> IO CFloat
mass gkAgent =
  sendMessage gkAgent massSelector

-- | Agent's mass. Used for agent impulse application purposes.
--
-- Defaults to 1.0
--
-- ObjC selector: @- setMass:@
setMass :: IsGKAgent gkAgent => gkAgent -> CFloat -> IO ()
setMass gkAgent value =
  sendMessage gkAgent setMassSelector value

-- | Radius of the agent's bounding circle.  Used by the agent avoid steering functions.
--
-- Defaults to 0.5 for a canonical diameter of 1.0
--
-- ObjC selector: @- radius@
radius :: IsGKAgent gkAgent => gkAgent -> IO CFloat
radius gkAgent =
  sendMessage gkAgent radiusSelector

-- | Radius of the agent's bounding circle.  Used by the agent avoid steering functions.
--
-- Defaults to 0.5 for a canonical diameter of 1.0
--
-- ObjC selector: @- setRadius:@
setRadius :: IsGKAgent gkAgent => gkAgent -> CFloat -> IO ()
setRadius gkAgent value =
  sendMessage gkAgent setRadiusSelector value

-- | Current speed of the agent along its foward direction.
--
-- Defaults to 0.0
--
-- ObjC selector: @- speed@
speed :: IsGKAgent gkAgent => gkAgent -> IO CFloat
speed gkAgent =
  sendMessage gkAgent speedSelector

-- | Current speed of the agent along its foward direction.
--
-- Defaults to 0.0
--
-- ObjC selector: @- setSpeed:@
setSpeed :: IsGKAgent gkAgent => gkAgent -> CFloat -> IO ()
setSpeed gkAgent value =
  sendMessage gkAgent setSpeedSelector value

-- | Maximum amount of acceleration that can be applied to this agent.  All applied impulses are clipped to this amount.
--
-- Defaults to 1.0
--
-- ObjC selector: @- maxAcceleration@
maxAcceleration :: IsGKAgent gkAgent => gkAgent -> IO CFloat
maxAcceleration gkAgent =
  sendMessage gkAgent maxAccelerationSelector

-- | Maximum amount of acceleration that can be applied to this agent.  All applied impulses are clipped to this amount.
--
-- Defaults to 1.0
--
-- ObjC selector: @- setMaxAcceleration:@
setMaxAcceleration :: IsGKAgent gkAgent => gkAgent -> CFloat -> IO ()
setMaxAcceleration gkAgent value =
  sendMessage gkAgent setMaxAccelerationSelector value

-- | Maximum speed of this agent. Impulses cannot cause the agents speed to ever be greater than this value.
--
-- Defaults to 1.0
--
-- ObjC selector: @- maxSpeed@
maxSpeed :: IsGKAgent gkAgent => gkAgent -> IO CFloat
maxSpeed gkAgent =
  sendMessage gkAgent maxSpeedSelector

-- | Maximum speed of this agent. Impulses cannot cause the agents speed to ever be greater than this value.
--
-- Defaults to 1.0
--
-- ObjC selector: @- setMaxSpeed:@
setMaxSpeed :: IsGKAgent gkAgent => gkAgent -> CFloat -> IO ()
setMaxSpeed gkAgent value =
  sendMessage gkAgent setMaxSpeedSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @behavior@
behaviorSelector :: Selector '[] (Id GKBehavior)
behaviorSelector = mkSelector "behavior"

-- | @Selector@ for @setBehavior:@
setBehaviorSelector :: Selector '[Id GKBehavior] ()
setBehaviorSelector = mkSelector "setBehavior:"

-- | @Selector@ for @mass@
massSelector :: Selector '[] CFloat
massSelector = mkSelector "mass"

-- | @Selector@ for @setMass:@
setMassSelector :: Selector '[CFloat] ()
setMassSelector = mkSelector "setMass:"

-- | @Selector@ for @radius@
radiusSelector :: Selector '[] CFloat
radiusSelector = mkSelector "radius"

-- | @Selector@ for @setRadius:@
setRadiusSelector :: Selector '[CFloat] ()
setRadiusSelector = mkSelector "setRadius:"

-- | @Selector@ for @speed@
speedSelector :: Selector '[] CFloat
speedSelector = mkSelector "speed"

-- | @Selector@ for @setSpeed:@
setSpeedSelector :: Selector '[CFloat] ()
setSpeedSelector = mkSelector "setSpeed:"

-- | @Selector@ for @maxAcceleration@
maxAccelerationSelector :: Selector '[] CFloat
maxAccelerationSelector = mkSelector "maxAcceleration"

-- | @Selector@ for @setMaxAcceleration:@
setMaxAccelerationSelector :: Selector '[CFloat] ()
setMaxAccelerationSelector = mkSelector "setMaxAcceleration:"

-- | @Selector@ for @maxSpeed@
maxSpeedSelector :: Selector '[] CFloat
maxSpeedSelector = mkSelector "maxSpeed"

-- | @Selector@ for @setMaxSpeed:@
setMaxSpeedSelector :: Selector '[CFloat] ()
setMaxSpeedSelector = mkSelector "setMaxSpeed:"

