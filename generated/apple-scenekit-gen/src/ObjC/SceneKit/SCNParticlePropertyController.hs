{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNParticlePropertyController
--
-- The SCNParticlePropertyController class controls the variation over time or over distance of a particle property.
--
-- Generated bindings for @SCNParticlePropertyController@.
module ObjC.SceneKit.SCNParticlePropertyController
  ( SCNParticlePropertyController
  , IsSCNParticlePropertyController(..)
  , controllerWithAnimation
  , animation
  , setAnimation
  , inputMode
  , setInputMode
  , inputScale
  , setInputScale
  , inputBias
  , setInputBias
  , inputOrigin
  , setInputOrigin
  , inputProperty
  , setInputProperty
  , animationSelector
  , controllerWithAnimationSelector
  , inputBiasSelector
  , inputModeSelector
  , inputOriginSelector
  , inputPropertySelector
  , inputScaleSelector
  , setAnimationSelector
  , setInputBiasSelector
  , setInputModeSelector
  , setInputOriginSelector
  , setInputPropertySelector
  , setInputScaleSelector

  -- * Enum types
  , SCNParticleInputMode(SCNParticleInputMode)
  , pattern SCNParticleInputModeOverLife
  , pattern SCNParticleInputModeOverDistance
  , pattern SCNParticleInputModeOverOtherProperty

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SceneKit.Internal.Classes
import ObjC.SceneKit.Internal.Enums
import ObjC.Foundation.Internal.Classes
import ObjC.QuartzCore.Internal.Classes

-- | @+ controllerWithAnimation:@
controllerWithAnimation :: IsCAAnimation animation => animation -> IO (Id SCNParticlePropertyController)
controllerWithAnimation animation =
  do
    cls' <- getRequiredClass "SCNParticlePropertyController"
    sendClassMessage cls' controllerWithAnimationSelector (toCAAnimation animation)

-- | @- animation@
animation :: IsSCNParticlePropertyController scnParticlePropertyController => scnParticlePropertyController -> IO (Id CAAnimation)
animation scnParticlePropertyController =
  sendMessage scnParticlePropertyController animationSelector

-- | @- setAnimation:@
setAnimation :: (IsSCNParticlePropertyController scnParticlePropertyController, IsCAAnimation value) => scnParticlePropertyController -> value -> IO ()
setAnimation scnParticlePropertyController value =
  sendMessage scnParticlePropertyController setAnimationSelector (toCAAnimation value)

-- | @- inputMode@
inputMode :: IsSCNParticlePropertyController scnParticlePropertyController => scnParticlePropertyController -> IO SCNParticleInputMode
inputMode scnParticlePropertyController =
  sendMessage scnParticlePropertyController inputModeSelector

-- | @- setInputMode:@
setInputMode :: IsSCNParticlePropertyController scnParticlePropertyController => scnParticlePropertyController -> SCNParticleInputMode -> IO ()
setInputMode scnParticlePropertyController value =
  sendMessage scnParticlePropertyController setInputModeSelector value

-- | @- inputScale@
inputScale :: IsSCNParticlePropertyController scnParticlePropertyController => scnParticlePropertyController -> IO CDouble
inputScale scnParticlePropertyController =
  sendMessage scnParticlePropertyController inputScaleSelector

-- | @- setInputScale:@
setInputScale :: IsSCNParticlePropertyController scnParticlePropertyController => scnParticlePropertyController -> CDouble -> IO ()
setInputScale scnParticlePropertyController value =
  sendMessage scnParticlePropertyController setInputScaleSelector value

-- | @- inputBias@
inputBias :: IsSCNParticlePropertyController scnParticlePropertyController => scnParticlePropertyController -> IO CDouble
inputBias scnParticlePropertyController =
  sendMessage scnParticlePropertyController inputBiasSelector

-- | @- setInputBias:@
setInputBias :: IsSCNParticlePropertyController scnParticlePropertyController => scnParticlePropertyController -> CDouble -> IO ()
setInputBias scnParticlePropertyController value =
  sendMessage scnParticlePropertyController setInputBiasSelector value

-- | @- inputOrigin@
inputOrigin :: IsSCNParticlePropertyController scnParticlePropertyController => scnParticlePropertyController -> IO (Id SCNNode)
inputOrigin scnParticlePropertyController =
  sendMessage scnParticlePropertyController inputOriginSelector

-- | @- setInputOrigin:@
setInputOrigin :: (IsSCNParticlePropertyController scnParticlePropertyController, IsSCNNode value) => scnParticlePropertyController -> value -> IO ()
setInputOrigin scnParticlePropertyController value =
  sendMessage scnParticlePropertyController setInputOriginSelector (toSCNNode value)

-- | @- inputProperty@
inputProperty :: IsSCNParticlePropertyController scnParticlePropertyController => scnParticlePropertyController -> IO (Id NSString)
inputProperty scnParticlePropertyController =
  sendMessage scnParticlePropertyController inputPropertySelector

-- | @- setInputProperty:@
setInputProperty :: (IsSCNParticlePropertyController scnParticlePropertyController, IsNSString value) => scnParticlePropertyController -> value -> IO ()
setInputProperty scnParticlePropertyController value =
  sendMessage scnParticlePropertyController setInputPropertySelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @controllerWithAnimation:@
controllerWithAnimationSelector :: Selector '[Id CAAnimation] (Id SCNParticlePropertyController)
controllerWithAnimationSelector = mkSelector "controllerWithAnimation:"

-- | @Selector@ for @animation@
animationSelector :: Selector '[] (Id CAAnimation)
animationSelector = mkSelector "animation"

-- | @Selector@ for @setAnimation:@
setAnimationSelector :: Selector '[Id CAAnimation] ()
setAnimationSelector = mkSelector "setAnimation:"

-- | @Selector@ for @inputMode@
inputModeSelector :: Selector '[] SCNParticleInputMode
inputModeSelector = mkSelector "inputMode"

-- | @Selector@ for @setInputMode:@
setInputModeSelector :: Selector '[SCNParticleInputMode] ()
setInputModeSelector = mkSelector "setInputMode:"

-- | @Selector@ for @inputScale@
inputScaleSelector :: Selector '[] CDouble
inputScaleSelector = mkSelector "inputScale"

-- | @Selector@ for @setInputScale:@
setInputScaleSelector :: Selector '[CDouble] ()
setInputScaleSelector = mkSelector "setInputScale:"

-- | @Selector@ for @inputBias@
inputBiasSelector :: Selector '[] CDouble
inputBiasSelector = mkSelector "inputBias"

-- | @Selector@ for @setInputBias:@
setInputBiasSelector :: Selector '[CDouble] ()
setInputBiasSelector = mkSelector "setInputBias:"

-- | @Selector@ for @inputOrigin@
inputOriginSelector :: Selector '[] (Id SCNNode)
inputOriginSelector = mkSelector "inputOrigin"

-- | @Selector@ for @setInputOrigin:@
setInputOriginSelector :: Selector '[Id SCNNode] ()
setInputOriginSelector = mkSelector "setInputOrigin:"

-- | @Selector@ for @inputProperty@
inputPropertySelector :: Selector '[] (Id NSString)
inputPropertySelector = mkSelector "inputProperty"

-- | @Selector@ for @setInputProperty:@
setInputPropertySelector :: Selector '[Id NSString] ()
setInputPropertySelector = mkSelector "setInputProperty:"

