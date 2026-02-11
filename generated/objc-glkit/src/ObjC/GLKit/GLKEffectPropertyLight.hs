{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @GLKEffectPropertyLight@.
module ObjC.GLKit.GLKEffectPropertyLight
  ( GLKEffectPropertyLight
  , IsGLKEffectPropertyLight(..)
  , enabled
  , setEnabled
  , spotExponent
  , setSpotExponent
  , spotCutoff
  , setSpotCutoff
  , constantAttenuation
  , setConstantAttenuation
  , linearAttenuation
  , setLinearAttenuation
  , quadraticAttenuation
  , setQuadraticAttenuation
  , transform
  , setTransform
  , enabledSelector
  , setEnabledSelector
  , spotExponentSelector
  , setSpotExponentSelector
  , spotCutoffSelector
  , setSpotCutoffSelector
  , constantAttenuationSelector
  , setConstantAttenuationSelector
  , linearAttenuationSelector
  , setLinearAttenuationSelector
  , quadraticAttenuationSelector
  , setQuadraticAttenuationSelector
  , transformSelector
  , setTransformSelector


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

import ObjC.GLKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- enabled@
enabled :: IsGLKEffectPropertyLight glkEffectPropertyLight => glkEffectPropertyLight -> IO CUChar
enabled glkEffectPropertyLight  =
  sendMsg glkEffectPropertyLight (mkSelector "enabled") retCUChar []

-- | @- setEnabled:@
setEnabled :: IsGLKEffectPropertyLight glkEffectPropertyLight => glkEffectPropertyLight -> CUChar -> IO ()
setEnabled glkEffectPropertyLight  value =
  sendMsg glkEffectPropertyLight (mkSelector "setEnabled:") retVoid [argCUChar (fromIntegral value)]

-- | @- spotExponent@
spotExponent :: IsGLKEffectPropertyLight glkEffectPropertyLight => glkEffectPropertyLight -> IO CFloat
spotExponent glkEffectPropertyLight  =
  sendMsg glkEffectPropertyLight (mkSelector "spotExponent") retCFloat []

-- | @- setSpotExponent:@
setSpotExponent :: IsGLKEffectPropertyLight glkEffectPropertyLight => glkEffectPropertyLight -> CFloat -> IO ()
setSpotExponent glkEffectPropertyLight  value =
  sendMsg glkEffectPropertyLight (mkSelector "setSpotExponent:") retVoid [argCFloat (fromIntegral value)]

-- | @- spotCutoff@
spotCutoff :: IsGLKEffectPropertyLight glkEffectPropertyLight => glkEffectPropertyLight -> IO CFloat
spotCutoff glkEffectPropertyLight  =
  sendMsg glkEffectPropertyLight (mkSelector "spotCutoff") retCFloat []

-- | @- setSpotCutoff:@
setSpotCutoff :: IsGLKEffectPropertyLight glkEffectPropertyLight => glkEffectPropertyLight -> CFloat -> IO ()
setSpotCutoff glkEffectPropertyLight  value =
  sendMsg glkEffectPropertyLight (mkSelector "setSpotCutoff:") retVoid [argCFloat (fromIntegral value)]

-- | @- constantAttenuation@
constantAttenuation :: IsGLKEffectPropertyLight glkEffectPropertyLight => glkEffectPropertyLight -> IO CFloat
constantAttenuation glkEffectPropertyLight  =
  sendMsg glkEffectPropertyLight (mkSelector "constantAttenuation") retCFloat []

-- | @- setConstantAttenuation:@
setConstantAttenuation :: IsGLKEffectPropertyLight glkEffectPropertyLight => glkEffectPropertyLight -> CFloat -> IO ()
setConstantAttenuation glkEffectPropertyLight  value =
  sendMsg glkEffectPropertyLight (mkSelector "setConstantAttenuation:") retVoid [argCFloat (fromIntegral value)]

-- | @- linearAttenuation@
linearAttenuation :: IsGLKEffectPropertyLight glkEffectPropertyLight => glkEffectPropertyLight -> IO CFloat
linearAttenuation glkEffectPropertyLight  =
  sendMsg glkEffectPropertyLight (mkSelector "linearAttenuation") retCFloat []

-- | @- setLinearAttenuation:@
setLinearAttenuation :: IsGLKEffectPropertyLight glkEffectPropertyLight => glkEffectPropertyLight -> CFloat -> IO ()
setLinearAttenuation glkEffectPropertyLight  value =
  sendMsg glkEffectPropertyLight (mkSelector "setLinearAttenuation:") retVoid [argCFloat (fromIntegral value)]

-- | @- quadraticAttenuation@
quadraticAttenuation :: IsGLKEffectPropertyLight glkEffectPropertyLight => glkEffectPropertyLight -> IO CFloat
quadraticAttenuation glkEffectPropertyLight  =
  sendMsg glkEffectPropertyLight (mkSelector "quadraticAttenuation") retCFloat []

-- | @- setQuadraticAttenuation:@
setQuadraticAttenuation :: IsGLKEffectPropertyLight glkEffectPropertyLight => glkEffectPropertyLight -> CFloat -> IO ()
setQuadraticAttenuation glkEffectPropertyLight  value =
  sendMsg glkEffectPropertyLight (mkSelector "setQuadraticAttenuation:") retVoid [argCFloat (fromIntegral value)]

-- | @- transform@
transform :: IsGLKEffectPropertyLight glkEffectPropertyLight => glkEffectPropertyLight -> IO (Id GLKEffectPropertyTransform)
transform glkEffectPropertyLight  =
  sendMsg glkEffectPropertyLight (mkSelector "transform") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTransform:@
setTransform :: (IsGLKEffectPropertyLight glkEffectPropertyLight, IsGLKEffectPropertyTransform value) => glkEffectPropertyLight -> value -> IO ()
setTransform glkEffectPropertyLight  value =
withObjCPtr value $ \raw_value ->
    sendMsg glkEffectPropertyLight (mkSelector "setTransform:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @enabled@
enabledSelector :: Selector
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @spotExponent@
spotExponentSelector :: Selector
spotExponentSelector = mkSelector "spotExponent"

-- | @Selector@ for @setSpotExponent:@
setSpotExponentSelector :: Selector
setSpotExponentSelector = mkSelector "setSpotExponent:"

-- | @Selector@ for @spotCutoff@
spotCutoffSelector :: Selector
spotCutoffSelector = mkSelector "spotCutoff"

-- | @Selector@ for @setSpotCutoff:@
setSpotCutoffSelector :: Selector
setSpotCutoffSelector = mkSelector "setSpotCutoff:"

-- | @Selector@ for @constantAttenuation@
constantAttenuationSelector :: Selector
constantAttenuationSelector = mkSelector "constantAttenuation"

-- | @Selector@ for @setConstantAttenuation:@
setConstantAttenuationSelector :: Selector
setConstantAttenuationSelector = mkSelector "setConstantAttenuation:"

-- | @Selector@ for @linearAttenuation@
linearAttenuationSelector :: Selector
linearAttenuationSelector = mkSelector "linearAttenuation"

-- | @Selector@ for @setLinearAttenuation:@
setLinearAttenuationSelector :: Selector
setLinearAttenuationSelector = mkSelector "setLinearAttenuation:"

-- | @Selector@ for @quadraticAttenuation@
quadraticAttenuationSelector :: Selector
quadraticAttenuationSelector = mkSelector "quadraticAttenuation"

-- | @Selector@ for @setQuadraticAttenuation:@
setQuadraticAttenuationSelector :: Selector
setQuadraticAttenuationSelector = mkSelector "setQuadraticAttenuation:"

-- | @Selector@ for @transform@
transformSelector :: Selector
transformSelector = mkSelector "transform"

-- | @Selector@ for @setTransform:@
setTransformSelector :: Selector
setTransformSelector = mkSelector "setTransform:"

