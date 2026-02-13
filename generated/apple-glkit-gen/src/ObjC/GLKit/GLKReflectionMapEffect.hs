{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @GLKReflectionMapEffect@.
module ObjC.GLKit.GLKReflectionMapEffect
  ( GLKReflectionMapEffect
  , IsGLKReflectionMapEffect(..)
  , prepareToDraw
  , textureCubeMap
  , prepareToDrawSelector
  , textureCubeMapSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GLKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- prepareToDraw@
prepareToDraw :: IsGLKReflectionMapEffect glkReflectionMapEffect => glkReflectionMapEffect -> IO ()
prepareToDraw glkReflectionMapEffect =
  sendMessage glkReflectionMapEffect prepareToDrawSelector

-- | @- textureCubeMap@
textureCubeMap :: IsGLKReflectionMapEffect glkReflectionMapEffect => glkReflectionMapEffect -> IO (Id GLKEffectPropertyTexture)
textureCubeMap glkReflectionMapEffect =
  sendMessage glkReflectionMapEffect textureCubeMapSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @prepareToDraw@
prepareToDrawSelector :: Selector '[] ()
prepareToDrawSelector = mkSelector "prepareToDraw"

-- | @Selector@ for @textureCubeMap@
textureCubeMapSelector :: Selector '[] (Id GLKEffectPropertyTexture)
textureCubeMapSelector = mkSelector "textureCubeMap"

