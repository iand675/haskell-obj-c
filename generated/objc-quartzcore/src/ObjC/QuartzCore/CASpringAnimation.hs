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
  , initWithPerceptualDuration_bounceSelector
  , massSelector
  , setMassSelector
  , stiffnessSelector
  , setStiffnessSelector
  , dampingSelector
  , setDampingSelector
  , initialVelocitySelector
  , setInitialVelocitySelector
  , allowsOverdampingSelector
  , setAllowsOverdampingSelector
  , settlingDurationSelector
  , perceptualDurationSelector
  , bounceSelector


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

import ObjC.QuartzCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithPerceptualDuration:bounce:@
initWithPerceptualDuration_bounce :: IsCASpringAnimation caSpringAnimation => caSpringAnimation -> CDouble -> CDouble -> IO (Id CASpringAnimation)
initWithPerceptualDuration_bounce caSpringAnimation  perceptualDuration bounce =
  sendMsg caSpringAnimation (mkSelector "initWithPerceptualDuration:bounce:") (retPtr retVoid) [argCDouble (fromIntegral perceptualDuration), argCDouble (fromIntegral bounce)] >>= ownedObject . castPtr

-- | @- mass@
mass :: IsCASpringAnimation caSpringAnimation => caSpringAnimation -> IO CDouble
mass caSpringAnimation  =
  sendMsg caSpringAnimation (mkSelector "mass") retCDouble []

-- | @- setMass:@
setMass :: IsCASpringAnimation caSpringAnimation => caSpringAnimation -> CDouble -> IO ()
setMass caSpringAnimation  value =
  sendMsg caSpringAnimation (mkSelector "setMass:") retVoid [argCDouble (fromIntegral value)]

-- | @- stiffness@
stiffness :: IsCASpringAnimation caSpringAnimation => caSpringAnimation -> IO CDouble
stiffness caSpringAnimation  =
  sendMsg caSpringAnimation (mkSelector "stiffness") retCDouble []

-- | @- setStiffness:@
setStiffness :: IsCASpringAnimation caSpringAnimation => caSpringAnimation -> CDouble -> IO ()
setStiffness caSpringAnimation  value =
  sendMsg caSpringAnimation (mkSelector "setStiffness:") retVoid [argCDouble (fromIntegral value)]

-- | @- damping@
damping :: IsCASpringAnimation caSpringAnimation => caSpringAnimation -> IO CDouble
damping caSpringAnimation  =
  sendMsg caSpringAnimation (mkSelector "damping") retCDouble []

-- | @- setDamping:@
setDamping :: IsCASpringAnimation caSpringAnimation => caSpringAnimation -> CDouble -> IO ()
setDamping caSpringAnimation  value =
  sendMsg caSpringAnimation (mkSelector "setDamping:") retVoid [argCDouble (fromIntegral value)]

-- | @- initialVelocity@
initialVelocity :: IsCASpringAnimation caSpringAnimation => caSpringAnimation -> IO CDouble
initialVelocity caSpringAnimation  =
  sendMsg caSpringAnimation (mkSelector "initialVelocity") retCDouble []

-- | @- setInitialVelocity:@
setInitialVelocity :: IsCASpringAnimation caSpringAnimation => caSpringAnimation -> CDouble -> IO ()
setInitialVelocity caSpringAnimation  value =
  sendMsg caSpringAnimation (mkSelector "setInitialVelocity:") retVoid [argCDouble (fromIntegral value)]

-- | @- allowsOverdamping@
allowsOverdamping :: IsCASpringAnimation caSpringAnimation => caSpringAnimation -> IO Bool
allowsOverdamping caSpringAnimation  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg caSpringAnimation (mkSelector "allowsOverdamping") retCULong []

-- | @- setAllowsOverdamping:@
setAllowsOverdamping :: IsCASpringAnimation caSpringAnimation => caSpringAnimation -> Bool -> IO ()
setAllowsOverdamping caSpringAnimation  value =
  sendMsg caSpringAnimation (mkSelector "setAllowsOverdamping:") retVoid [argCULong (if value then 1 else 0)]

-- | @- settlingDuration@
settlingDuration :: IsCASpringAnimation caSpringAnimation => caSpringAnimation -> IO CDouble
settlingDuration caSpringAnimation  =
  sendMsg caSpringAnimation (mkSelector "settlingDuration") retCDouble []

-- | @- perceptualDuration@
perceptualDuration :: IsCASpringAnimation caSpringAnimation => caSpringAnimation -> IO CDouble
perceptualDuration caSpringAnimation  =
  sendMsg caSpringAnimation (mkSelector "perceptualDuration") retCDouble []

-- | @- bounce@
bounce :: IsCASpringAnimation caSpringAnimation => caSpringAnimation -> IO CDouble
bounce caSpringAnimation  =
  sendMsg caSpringAnimation (mkSelector "bounce") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPerceptualDuration:bounce:@
initWithPerceptualDuration_bounceSelector :: Selector
initWithPerceptualDuration_bounceSelector = mkSelector "initWithPerceptualDuration:bounce:"

-- | @Selector@ for @mass@
massSelector :: Selector
massSelector = mkSelector "mass"

-- | @Selector@ for @setMass:@
setMassSelector :: Selector
setMassSelector = mkSelector "setMass:"

-- | @Selector@ for @stiffness@
stiffnessSelector :: Selector
stiffnessSelector = mkSelector "stiffness"

-- | @Selector@ for @setStiffness:@
setStiffnessSelector :: Selector
setStiffnessSelector = mkSelector "setStiffness:"

-- | @Selector@ for @damping@
dampingSelector :: Selector
dampingSelector = mkSelector "damping"

-- | @Selector@ for @setDamping:@
setDampingSelector :: Selector
setDampingSelector = mkSelector "setDamping:"

-- | @Selector@ for @initialVelocity@
initialVelocitySelector :: Selector
initialVelocitySelector = mkSelector "initialVelocity"

-- | @Selector@ for @setInitialVelocity:@
setInitialVelocitySelector :: Selector
setInitialVelocitySelector = mkSelector "setInitialVelocity:"

-- | @Selector@ for @allowsOverdamping@
allowsOverdampingSelector :: Selector
allowsOverdampingSelector = mkSelector "allowsOverdamping"

-- | @Selector@ for @setAllowsOverdamping:@
setAllowsOverdampingSelector :: Selector
setAllowsOverdampingSelector = mkSelector "setAllowsOverdamping:"

-- | @Selector@ for @settlingDuration@
settlingDurationSelector :: Selector
settlingDurationSelector = mkSelector "settlingDuration"

-- | @Selector@ for @perceptualDuration@
perceptualDurationSelector :: Selector
perceptualDurationSelector = mkSelector "perceptualDuration"

-- | @Selector@ for @bounce@
bounceSelector :: Selector
bounceSelector = mkSelector "bounce"

