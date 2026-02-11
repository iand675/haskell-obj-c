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
  , delegateSelector
  , setDelegateSelector
  , behaviorSelector
  , setBehaviorSelector
  , massSelector
  , setMassSelector
  , radiusSelector
  , setRadiusSelector
  , speedSelector
  , setSpeedSelector
  , maxAccelerationSelector
  , setMaxAccelerationSelector
  , maxSpeedSelector
  , setMaxSpeedSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameplayKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Object which has agentDidUpdate called on it during this agent's behavior updatekbeha
--
-- ObjC selector: @- delegate@
delegate :: IsGKAgent gkAgent => gkAgent -> IO RawId
delegate gkAgent  =
    fmap (RawId . castPtr) $ sendMsg gkAgent (mkSelector "delegate") (retPtr retVoid) []

-- | Object which has agentDidUpdate called on it during this agent's behavior updatekbeha
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsGKAgent gkAgent => gkAgent -> RawId -> IO ()
setDelegate gkAgent  value =
    sendMsg gkAgent (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | The behavior to apply when updateWithDeltaTime is called. All forces from the goals in the behavior are summed and then applied.
--
-- ObjC selector: @- behavior@
behavior :: IsGKAgent gkAgent => gkAgent -> IO (Id GKBehavior)
behavior gkAgent  =
    sendMsg gkAgent (mkSelector "behavior") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The behavior to apply when updateWithDeltaTime is called. All forces from the goals in the behavior are summed and then applied.
--
-- ObjC selector: @- setBehavior:@
setBehavior :: (IsGKAgent gkAgent, IsGKBehavior value) => gkAgent -> value -> IO ()
setBehavior gkAgent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg gkAgent (mkSelector "setBehavior:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Agent's mass. Used for agent impulse application purposes.
--
-- Defaults to 1.0
--
-- ObjC selector: @- mass@
mass :: IsGKAgent gkAgent => gkAgent -> IO CFloat
mass gkAgent  =
    sendMsg gkAgent (mkSelector "mass") retCFloat []

-- | Agent's mass. Used for agent impulse application purposes.
--
-- Defaults to 1.0
--
-- ObjC selector: @- setMass:@
setMass :: IsGKAgent gkAgent => gkAgent -> CFloat -> IO ()
setMass gkAgent  value =
    sendMsg gkAgent (mkSelector "setMass:") retVoid [argCFloat value]

-- | Radius of the agent's bounding circle.  Used by the agent avoid steering functions.
--
-- Defaults to 0.5 for a canonical diameter of 1.0
--
-- ObjC selector: @- radius@
radius :: IsGKAgent gkAgent => gkAgent -> IO CFloat
radius gkAgent  =
    sendMsg gkAgent (mkSelector "radius") retCFloat []

-- | Radius of the agent's bounding circle.  Used by the agent avoid steering functions.
--
-- Defaults to 0.5 for a canonical diameter of 1.0
--
-- ObjC selector: @- setRadius:@
setRadius :: IsGKAgent gkAgent => gkAgent -> CFloat -> IO ()
setRadius gkAgent  value =
    sendMsg gkAgent (mkSelector "setRadius:") retVoid [argCFloat value]

-- | Current speed of the agent along its foward direction.
--
-- Defaults to 0.0
--
-- ObjC selector: @- speed@
speed :: IsGKAgent gkAgent => gkAgent -> IO CFloat
speed gkAgent  =
    sendMsg gkAgent (mkSelector "speed") retCFloat []

-- | Current speed of the agent along its foward direction.
--
-- Defaults to 0.0
--
-- ObjC selector: @- setSpeed:@
setSpeed :: IsGKAgent gkAgent => gkAgent -> CFloat -> IO ()
setSpeed gkAgent  value =
    sendMsg gkAgent (mkSelector "setSpeed:") retVoid [argCFloat value]

-- | Maximum amount of acceleration that can be applied to this agent.  All applied impulses are clipped to this amount.
--
-- Defaults to 1.0
--
-- ObjC selector: @- maxAcceleration@
maxAcceleration :: IsGKAgent gkAgent => gkAgent -> IO CFloat
maxAcceleration gkAgent  =
    sendMsg gkAgent (mkSelector "maxAcceleration") retCFloat []

-- | Maximum amount of acceleration that can be applied to this agent.  All applied impulses are clipped to this amount.
--
-- Defaults to 1.0
--
-- ObjC selector: @- setMaxAcceleration:@
setMaxAcceleration :: IsGKAgent gkAgent => gkAgent -> CFloat -> IO ()
setMaxAcceleration gkAgent  value =
    sendMsg gkAgent (mkSelector "setMaxAcceleration:") retVoid [argCFloat value]

-- | Maximum speed of this agent. Impulses cannot cause the agents speed to ever be greater than this value.
--
-- Defaults to 1.0
--
-- ObjC selector: @- maxSpeed@
maxSpeed :: IsGKAgent gkAgent => gkAgent -> IO CFloat
maxSpeed gkAgent  =
    sendMsg gkAgent (mkSelector "maxSpeed") retCFloat []

-- | Maximum speed of this agent. Impulses cannot cause the agents speed to ever be greater than this value.
--
-- Defaults to 1.0
--
-- ObjC selector: @- setMaxSpeed:@
setMaxSpeed :: IsGKAgent gkAgent => gkAgent -> CFloat -> IO ()
setMaxSpeed gkAgent  value =
    sendMsg gkAgent (mkSelector "setMaxSpeed:") retVoid [argCFloat value]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @behavior@
behaviorSelector :: Selector
behaviorSelector = mkSelector "behavior"

-- | @Selector@ for @setBehavior:@
setBehaviorSelector :: Selector
setBehaviorSelector = mkSelector "setBehavior:"

-- | @Selector@ for @mass@
massSelector :: Selector
massSelector = mkSelector "mass"

-- | @Selector@ for @setMass:@
setMassSelector :: Selector
setMassSelector = mkSelector "setMass:"

-- | @Selector@ for @radius@
radiusSelector :: Selector
radiusSelector = mkSelector "radius"

-- | @Selector@ for @setRadius:@
setRadiusSelector :: Selector
setRadiusSelector = mkSelector "setRadius:"

-- | @Selector@ for @speed@
speedSelector :: Selector
speedSelector = mkSelector "speed"

-- | @Selector@ for @setSpeed:@
setSpeedSelector :: Selector
setSpeedSelector = mkSelector "setSpeed:"

-- | @Selector@ for @maxAcceleration@
maxAccelerationSelector :: Selector
maxAccelerationSelector = mkSelector "maxAcceleration"

-- | @Selector@ for @setMaxAcceleration:@
setMaxAccelerationSelector :: Selector
setMaxAccelerationSelector = mkSelector "setMaxAcceleration:"

-- | @Selector@ for @maxSpeed@
maxSpeedSelector :: Selector
maxSpeedSelector = mkSelector "maxSpeed"

-- | @Selector@ for @setMaxSpeed:@
setMaxSpeedSelector :: Selector
setMaxSpeedSelector = mkSelector "setMaxSpeed:"

