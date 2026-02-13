{-# LANGUAGE DataKinds #-}
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
  , constantAttenuationSelector
  , enabledSelector
  , linearAttenuationSelector
  , quadraticAttenuationSelector
  , setConstantAttenuationSelector
  , setEnabledSelector
  , setLinearAttenuationSelector
  , setQuadraticAttenuationSelector
  , setSpotCutoffSelector
  , setSpotExponentSelector
  , setTransformSelector
  , spotCutoffSelector
  , spotExponentSelector
  , transformSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GLKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- enabled@
enabled :: IsGLKEffectPropertyLight glkEffectPropertyLight => glkEffectPropertyLight -> IO CUChar
enabled glkEffectPropertyLight =
  sendMessage glkEffectPropertyLight enabledSelector

-- | @- setEnabled:@
setEnabled :: IsGLKEffectPropertyLight glkEffectPropertyLight => glkEffectPropertyLight -> CUChar -> IO ()
setEnabled glkEffectPropertyLight value =
  sendMessage glkEffectPropertyLight setEnabledSelector value

-- | @- spotExponent@
spotExponent :: IsGLKEffectPropertyLight glkEffectPropertyLight => glkEffectPropertyLight -> IO CFloat
spotExponent glkEffectPropertyLight =
  sendMessage glkEffectPropertyLight spotExponentSelector

-- | @- setSpotExponent:@
setSpotExponent :: IsGLKEffectPropertyLight glkEffectPropertyLight => glkEffectPropertyLight -> CFloat -> IO ()
setSpotExponent glkEffectPropertyLight value =
  sendMessage glkEffectPropertyLight setSpotExponentSelector value

-- | @- spotCutoff@
spotCutoff :: IsGLKEffectPropertyLight glkEffectPropertyLight => glkEffectPropertyLight -> IO CFloat
spotCutoff glkEffectPropertyLight =
  sendMessage glkEffectPropertyLight spotCutoffSelector

-- | @- setSpotCutoff:@
setSpotCutoff :: IsGLKEffectPropertyLight glkEffectPropertyLight => glkEffectPropertyLight -> CFloat -> IO ()
setSpotCutoff glkEffectPropertyLight value =
  sendMessage glkEffectPropertyLight setSpotCutoffSelector value

-- | @- constantAttenuation@
constantAttenuation :: IsGLKEffectPropertyLight glkEffectPropertyLight => glkEffectPropertyLight -> IO CFloat
constantAttenuation glkEffectPropertyLight =
  sendMessage glkEffectPropertyLight constantAttenuationSelector

-- | @- setConstantAttenuation:@
setConstantAttenuation :: IsGLKEffectPropertyLight glkEffectPropertyLight => glkEffectPropertyLight -> CFloat -> IO ()
setConstantAttenuation glkEffectPropertyLight value =
  sendMessage glkEffectPropertyLight setConstantAttenuationSelector value

-- | @- linearAttenuation@
linearAttenuation :: IsGLKEffectPropertyLight glkEffectPropertyLight => glkEffectPropertyLight -> IO CFloat
linearAttenuation glkEffectPropertyLight =
  sendMessage glkEffectPropertyLight linearAttenuationSelector

-- | @- setLinearAttenuation:@
setLinearAttenuation :: IsGLKEffectPropertyLight glkEffectPropertyLight => glkEffectPropertyLight -> CFloat -> IO ()
setLinearAttenuation glkEffectPropertyLight value =
  sendMessage glkEffectPropertyLight setLinearAttenuationSelector value

-- | @- quadraticAttenuation@
quadraticAttenuation :: IsGLKEffectPropertyLight glkEffectPropertyLight => glkEffectPropertyLight -> IO CFloat
quadraticAttenuation glkEffectPropertyLight =
  sendMessage glkEffectPropertyLight quadraticAttenuationSelector

-- | @- setQuadraticAttenuation:@
setQuadraticAttenuation :: IsGLKEffectPropertyLight glkEffectPropertyLight => glkEffectPropertyLight -> CFloat -> IO ()
setQuadraticAttenuation glkEffectPropertyLight value =
  sendMessage glkEffectPropertyLight setQuadraticAttenuationSelector value

-- | @- transform@
transform :: IsGLKEffectPropertyLight glkEffectPropertyLight => glkEffectPropertyLight -> IO (Id GLKEffectPropertyTransform)
transform glkEffectPropertyLight =
  sendMessage glkEffectPropertyLight transformSelector

-- | @- setTransform:@
setTransform :: (IsGLKEffectPropertyLight glkEffectPropertyLight, IsGLKEffectPropertyTransform value) => glkEffectPropertyLight -> value -> IO ()
setTransform glkEffectPropertyLight value =
  sendMessage glkEffectPropertyLight setTransformSelector (toGLKEffectPropertyTransform value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @enabled@
enabledSelector :: Selector '[] CUChar
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector '[CUChar] ()
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @spotExponent@
spotExponentSelector :: Selector '[] CFloat
spotExponentSelector = mkSelector "spotExponent"

-- | @Selector@ for @setSpotExponent:@
setSpotExponentSelector :: Selector '[CFloat] ()
setSpotExponentSelector = mkSelector "setSpotExponent:"

-- | @Selector@ for @spotCutoff@
spotCutoffSelector :: Selector '[] CFloat
spotCutoffSelector = mkSelector "spotCutoff"

-- | @Selector@ for @setSpotCutoff:@
setSpotCutoffSelector :: Selector '[CFloat] ()
setSpotCutoffSelector = mkSelector "setSpotCutoff:"

-- | @Selector@ for @constantAttenuation@
constantAttenuationSelector :: Selector '[] CFloat
constantAttenuationSelector = mkSelector "constantAttenuation"

-- | @Selector@ for @setConstantAttenuation:@
setConstantAttenuationSelector :: Selector '[CFloat] ()
setConstantAttenuationSelector = mkSelector "setConstantAttenuation:"

-- | @Selector@ for @linearAttenuation@
linearAttenuationSelector :: Selector '[] CFloat
linearAttenuationSelector = mkSelector "linearAttenuation"

-- | @Selector@ for @setLinearAttenuation:@
setLinearAttenuationSelector :: Selector '[CFloat] ()
setLinearAttenuationSelector = mkSelector "setLinearAttenuation:"

-- | @Selector@ for @quadraticAttenuation@
quadraticAttenuationSelector :: Selector '[] CFloat
quadraticAttenuationSelector = mkSelector "quadraticAttenuation"

-- | @Selector@ for @setQuadraticAttenuation:@
setQuadraticAttenuationSelector :: Selector '[CFloat] ()
setQuadraticAttenuationSelector = mkSelector "setQuadraticAttenuation:"

-- | @Selector@ for @transform@
transformSelector :: Selector '[] (Id GLKEffectPropertyTransform)
transformSelector = mkSelector "transform"

-- | @Selector@ for @setTransform:@
setTransformSelector :: Selector '[Id GLKEffectPropertyTransform] ()
setTransformSelector = mkSelector "setTransform:"

