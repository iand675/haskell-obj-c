{-# LANGUAGE PatternSynonyms #-}
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
  , controllerWithAnimationSelector
  , animationSelector
  , setAnimationSelector
  , inputModeSelector
  , setInputModeSelector
  , inputScaleSelector
  , setInputScaleSelector
  , inputBiasSelector
  , setInputBiasSelector
  , inputOriginSelector
  , setInputOriginSelector
  , inputPropertySelector
  , setInputPropertySelector

  -- * Enum types
  , SCNParticleInputMode(SCNParticleInputMode)
  , pattern SCNParticleInputModeOverLife
  , pattern SCNParticleInputModeOverDistance
  , pattern SCNParticleInputModeOverOtherProperty

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

import ObjC.SceneKit.Internal.Classes
import ObjC.SceneKit.Internal.Enums
import ObjC.Foundation.Internal.Classes
import ObjC.QuartzCore.Internal.Classes

-- | @+ controllerWithAnimation:@
controllerWithAnimation :: IsCAAnimation animation => animation -> IO (Id SCNParticlePropertyController)
controllerWithAnimation animation =
  do
    cls' <- getRequiredClass "SCNParticlePropertyController"
    withObjCPtr animation $ \raw_animation ->
      sendClassMsg cls' (mkSelector "controllerWithAnimation:") (retPtr retVoid) [argPtr (castPtr raw_animation :: Ptr ())] >>= retainedObject . castPtr

-- | @- animation@
animation :: IsSCNParticlePropertyController scnParticlePropertyController => scnParticlePropertyController -> IO (Id CAAnimation)
animation scnParticlePropertyController  =
    sendMsg scnParticlePropertyController (mkSelector "animation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAnimation:@
setAnimation :: (IsSCNParticlePropertyController scnParticlePropertyController, IsCAAnimation value) => scnParticlePropertyController -> value -> IO ()
setAnimation scnParticlePropertyController  value =
  withObjCPtr value $ \raw_value ->
      sendMsg scnParticlePropertyController (mkSelector "setAnimation:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- inputMode@
inputMode :: IsSCNParticlePropertyController scnParticlePropertyController => scnParticlePropertyController -> IO SCNParticleInputMode
inputMode scnParticlePropertyController  =
    fmap (coerce :: CLong -> SCNParticleInputMode) $ sendMsg scnParticlePropertyController (mkSelector "inputMode") retCLong []

-- | @- setInputMode:@
setInputMode :: IsSCNParticlePropertyController scnParticlePropertyController => scnParticlePropertyController -> SCNParticleInputMode -> IO ()
setInputMode scnParticlePropertyController  value =
    sendMsg scnParticlePropertyController (mkSelector "setInputMode:") retVoid [argCLong (coerce value)]

-- | @- inputScale@
inputScale :: IsSCNParticlePropertyController scnParticlePropertyController => scnParticlePropertyController -> IO CDouble
inputScale scnParticlePropertyController  =
    sendMsg scnParticlePropertyController (mkSelector "inputScale") retCDouble []

-- | @- setInputScale:@
setInputScale :: IsSCNParticlePropertyController scnParticlePropertyController => scnParticlePropertyController -> CDouble -> IO ()
setInputScale scnParticlePropertyController  value =
    sendMsg scnParticlePropertyController (mkSelector "setInputScale:") retVoid [argCDouble value]

-- | @- inputBias@
inputBias :: IsSCNParticlePropertyController scnParticlePropertyController => scnParticlePropertyController -> IO CDouble
inputBias scnParticlePropertyController  =
    sendMsg scnParticlePropertyController (mkSelector "inputBias") retCDouble []

-- | @- setInputBias:@
setInputBias :: IsSCNParticlePropertyController scnParticlePropertyController => scnParticlePropertyController -> CDouble -> IO ()
setInputBias scnParticlePropertyController  value =
    sendMsg scnParticlePropertyController (mkSelector "setInputBias:") retVoid [argCDouble value]

-- | @- inputOrigin@
inputOrigin :: IsSCNParticlePropertyController scnParticlePropertyController => scnParticlePropertyController -> IO (Id SCNNode)
inputOrigin scnParticlePropertyController  =
    sendMsg scnParticlePropertyController (mkSelector "inputOrigin") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setInputOrigin:@
setInputOrigin :: (IsSCNParticlePropertyController scnParticlePropertyController, IsSCNNode value) => scnParticlePropertyController -> value -> IO ()
setInputOrigin scnParticlePropertyController  value =
  withObjCPtr value $ \raw_value ->
      sendMsg scnParticlePropertyController (mkSelector "setInputOrigin:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- inputProperty@
inputProperty :: IsSCNParticlePropertyController scnParticlePropertyController => scnParticlePropertyController -> IO (Id NSString)
inputProperty scnParticlePropertyController  =
    sendMsg scnParticlePropertyController (mkSelector "inputProperty") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setInputProperty:@
setInputProperty :: (IsSCNParticlePropertyController scnParticlePropertyController, IsNSString value) => scnParticlePropertyController -> value -> IO ()
setInputProperty scnParticlePropertyController  value =
  withObjCPtr value $ \raw_value ->
      sendMsg scnParticlePropertyController (mkSelector "setInputProperty:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @controllerWithAnimation:@
controllerWithAnimationSelector :: Selector
controllerWithAnimationSelector = mkSelector "controllerWithAnimation:"

-- | @Selector@ for @animation@
animationSelector :: Selector
animationSelector = mkSelector "animation"

-- | @Selector@ for @setAnimation:@
setAnimationSelector :: Selector
setAnimationSelector = mkSelector "setAnimation:"

-- | @Selector@ for @inputMode@
inputModeSelector :: Selector
inputModeSelector = mkSelector "inputMode"

-- | @Selector@ for @setInputMode:@
setInputModeSelector :: Selector
setInputModeSelector = mkSelector "setInputMode:"

-- | @Selector@ for @inputScale@
inputScaleSelector :: Selector
inputScaleSelector = mkSelector "inputScale"

-- | @Selector@ for @setInputScale:@
setInputScaleSelector :: Selector
setInputScaleSelector = mkSelector "setInputScale:"

-- | @Selector@ for @inputBias@
inputBiasSelector :: Selector
inputBiasSelector = mkSelector "inputBias"

-- | @Selector@ for @setInputBias:@
setInputBiasSelector :: Selector
setInputBiasSelector = mkSelector "setInputBias:"

-- | @Selector@ for @inputOrigin@
inputOriginSelector :: Selector
inputOriginSelector = mkSelector "inputOrigin"

-- | @Selector@ for @setInputOrigin:@
setInputOriginSelector :: Selector
setInputOriginSelector = mkSelector "setInputOrigin:"

-- | @Selector@ for @inputProperty@
inputPropertySelector :: Selector
inputPropertySelector = mkSelector "inputProperty"

-- | @Selector@ for @setInputProperty:@
setInputPropertySelector :: Selector
setInputPropertySelector = mkSelector "setInputProperty:"

