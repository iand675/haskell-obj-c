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

-- | @- prepareToDraw@
prepareToDraw :: IsGLKReflectionMapEffect glkReflectionMapEffect => glkReflectionMapEffect -> IO ()
prepareToDraw glkReflectionMapEffect  =
  sendMsg glkReflectionMapEffect (mkSelector "prepareToDraw") retVoid []

-- | @- textureCubeMap@
textureCubeMap :: IsGLKReflectionMapEffect glkReflectionMapEffect => glkReflectionMapEffect -> IO (Id GLKEffectPropertyTexture)
textureCubeMap glkReflectionMapEffect  =
  sendMsg glkReflectionMapEffect (mkSelector "textureCubeMap") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @prepareToDraw@
prepareToDrawSelector :: Selector
prepareToDrawSelector = mkSelector "prepareToDraw"

-- | @Selector@ for @textureCubeMap@
textureCubeMapSelector :: Selector
textureCubeMapSelector = mkSelector "textureCubeMap"

