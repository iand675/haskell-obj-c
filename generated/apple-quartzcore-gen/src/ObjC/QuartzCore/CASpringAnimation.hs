{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Subclass for mass-spring animations.
--
-- Generated bindings for @CASpringAnimation@.
module ObjC.QuartzCore.CASpringAnimation
  ( CASpringAnimation
  , IsCASpringAnimation(..)
  , initWithPerceptualDuration_bounce
  , mass
  , setMass
  , stiffness
  , setStiffness
  , damping
  , setDamping
  , initialVelocity
  , setInitialVelocity
  , allowsOverdamping
  , setAllowsOverdamping
  , settlingDuration
  , perceptualDuration
  , bounce
  , allowsOverdampingSelector
  , bounceSelector
  , dampingSelector
  , initWithPerceptualDuration_bounceSelector
  , initialVelocitySelector
  , massSelector
  , perceptualDurationSelector
  , setAllowsOverdampingSelector
  , setDampingSelector
  , setInitialVelocitySelector
  , setMassSelector
  , setStiffnessSelector
  , settlingDurationSelector
  , stiffnessSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.QuartzCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithPerceptualDuration:bounce:@
initWithPerceptualDuration_bounce :: IsCASpringAnimation caSpringAnimation => caSpringAnimation -> CDouble -> CDouble -> IO (Id CASpringAnimation)
initWithPerceptualDuration_bounce caSpringAnimation perceptualDuration bounce =
  sendOwnedMessage caSpringAnimation initWithPerceptualDuration_bounceSelector perceptualDuration bounce

-- | @- mass@
mass :: IsCASpringAnimation caSpringAnimation => caSpringAnimation -> IO CDouble
mass caSpringAnimation =
  sendMessage caSpringAnimation massSelector

-- | @- setMass:@
setMass :: IsCASpringAnimation caSpringAnimation => caSpringAnimation -> CDouble -> IO ()
setMass caSpringAnimation value =
  sendMessage caSpringAnimation setMassSelector value

-- | @- stiffness@
stiffness :: IsCASpringAnimation caSpringAnimation => caSpringAnimation -> IO CDouble
stiffness caSpringAnimation =
  sendMessage caSpringAnimation stiffnessSelector

-- | @- setStiffness:@
setStiffness :: IsCASpringAnimation caSpringAnimation => caSpringAnimation -> CDouble -> IO ()
setStiffness caSpringAnimation value =
  sendMessage caSpringAnimation setStiffnessSelector value

-- | @- damping@
damping :: IsCASpringAnimation caSpringAnimation => caSpringAnimation -> IO CDouble
damping caSpringAnimation =
  sendMessage caSpringAnimation dampingSelector

-- | @- setDamping:@
setDamping :: IsCASpringAnimation caSpringAnimation => caSpringAnimation -> CDouble -> IO ()
setDamping caSpringAnimation value =
  sendMessage caSpringAnimation setDampingSelector value

-- | @- initialVelocity@
initialVelocity :: IsCASpringAnimation caSpringAnimation => caSpringAnimation -> IO CDouble
initialVelocity caSpringAnimation =
  sendOwnedMessage caSpringAnimation initialVelocitySelector

-- | @- setInitialVelocity:@
setInitialVelocity :: IsCASpringAnimation caSpringAnimation => caSpringAnimation -> CDouble -> IO ()
setInitialVelocity caSpringAnimation value =
  sendMessage caSpringAnimation setInitialVelocitySelector value

-- | @- allowsOverdamping@
allowsOverdamping :: IsCASpringAnimation caSpringAnimation => caSpringAnimation -> IO Bool
allowsOverdamping caSpringAnimation =
  sendMessage caSpringAnimation allowsOverdampingSelector

-- | @- setAllowsOverdamping:@
setAllowsOverdamping :: IsCASpringAnimation caSpringAnimation => caSpringAnimation -> Bool -> IO ()
setAllowsOverdamping caSpringAnimation value =
  sendMessage caSpringAnimation setAllowsOverdampingSelector value

-- | @- settlingDuration@
settlingDuration :: IsCASpringAnimation caSpringAnimation => caSpringAnimation -> IO CDouble
settlingDuration caSpringAnimation =
  sendMessage caSpringAnimation settlingDurationSelector

-- | @- perceptualDuration@
perceptualDuration :: IsCASpringAnimation caSpringAnimation => caSpringAnimation -> IO CDouble
perceptualDuration caSpringAnimation =
  sendMessage caSpringAnimation perceptualDurationSelector

-- | @- bounce@
bounce :: IsCASpringAnimation caSpringAnimation => caSpringAnimation -> IO CDouble
bounce caSpringAnimation =
  sendMessage caSpringAnimation bounceSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPerceptualDuration:bounce:@
initWithPerceptualDuration_bounceSelector :: Selector '[CDouble, CDouble] (Id CASpringAnimation)
initWithPerceptualDuration_bounceSelector = mkSelector "initWithPerceptualDuration:bounce:"

-- | @Selector@ for @mass@
massSelector :: Selector '[] CDouble
massSelector = mkSelector "mass"

-- | @Selector@ for @setMass:@
setMassSelector :: Selector '[CDouble] ()
setMassSelector = mkSelector "setMass:"

-- | @Selector@ for @stiffness@
stiffnessSelector :: Selector '[] CDouble
stiffnessSelector = mkSelector "stiffness"

-- | @Selector@ for @setStiffness:@
setStiffnessSelector :: Selector '[CDouble] ()
setStiffnessSelector = mkSelector "setStiffness:"

-- | @Selector@ for @damping@
dampingSelector :: Selector '[] CDouble
dampingSelector = mkSelector "damping"

-- | @Selector@ for @setDamping:@
setDampingSelector :: Selector '[CDouble] ()
setDampingSelector = mkSelector "setDamping:"

-- | @Selector@ for @initialVelocity@
initialVelocitySelector :: Selector '[] CDouble
initialVelocitySelector = mkSelector "initialVelocity"

-- | @Selector@ for @setInitialVelocity:@
setInitialVelocitySelector :: Selector '[CDouble] ()
setInitialVelocitySelector = mkSelector "setInitialVelocity:"

-- | @Selector@ for @allowsOverdamping@
allowsOverdampingSelector :: Selector '[] Bool
allowsOverdampingSelector = mkSelector "allowsOverdamping"

-- | @Selector@ for @setAllowsOverdamping:@
setAllowsOverdampingSelector :: Selector '[Bool] ()
setAllowsOverdampingSelector = mkSelector "setAllowsOverdamping:"

-- | @Selector@ for @settlingDuration@
settlingDurationSelector :: Selector '[] CDouble
settlingDurationSelector = mkSelector "settlingDuration"

-- | @Selector@ for @perceptualDuration@
perceptualDurationSelector :: Selector '[] CDouble
perceptualDurationSelector = mkSelector "perceptualDuration"

-- | @Selector@ for @bounce@
bounceSelector :: Selector '[] CDouble
bounceSelector = mkSelector "bounce"

