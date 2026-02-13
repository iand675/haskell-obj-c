{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @GLKEffectPropertyMaterial@.
module ObjC.GLKit.GLKEffectPropertyMaterial
  ( GLKEffectPropertyMaterial
  , IsGLKEffectPropertyMaterial(..)
  , shininess
  , setShininess
  , setShininessSelector
  , shininessSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GLKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- shininess@
shininess :: IsGLKEffectPropertyMaterial glkEffectPropertyMaterial => glkEffectPropertyMaterial -> IO CFloat
shininess glkEffectPropertyMaterial =
  sendMessage glkEffectPropertyMaterial shininessSelector

-- | @- setShininess:@
setShininess :: IsGLKEffectPropertyMaterial glkEffectPropertyMaterial => glkEffectPropertyMaterial -> CFloat -> IO ()
setShininess glkEffectPropertyMaterial value =
  sendMessage glkEffectPropertyMaterial setShininessSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @shininess@
shininessSelector :: Selector '[] CFloat
shininessSelector = mkSelector "shininess"

-- | @Selector@ for @setShininess:@
setShininessSelector :: Selector '[CFloat] ()
setShininessSelector = mkSelector "setShininess:"

