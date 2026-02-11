{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @GLKEffectPropertyMaterial@.
module ObjC.GLKit.GLKEffectPropertyMaterial
  ( GLKEffectPropertyMaterial
  , IsGLKEffectPropertyMaterial(..)
  , shininess
  , setShininess
  , shininessSelector
  , setShininessSelector


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

-- | @- shininess@
shininess :: IsGLKEffectPropertyMaterial glkEffectPropertyMaterial => glkEffectPropertyMaterial -> IO CFloat
shininess glkEffectPropertyMaterial  =
  sendMsg glkEffectPropertyMaterial (mkSelector "shininess") retCFloat []

-- | @- setShininess:@
setShininess :: IsGLKEffectPropertyMaterial glkEffectPropertyMaterial => glkEffectPropertyMaterial -> CFloat -> IO ()
setShininess glkEffectPropertyMaterial  value =
  sendMsg glkEffectPropertyMaterial (mkSelector "setShininess:") retVoid [argCFloat (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @shininess@
shininessSelector :: Selector
shininessSelector = mkSelector "shininess"

-- | @Selector@ for @setShininess:@
setShininessSelector :: Selector
setShininessSelector = mkSelector "setShininess:"

