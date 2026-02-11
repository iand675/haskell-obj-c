{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A texture to be mapped onto SKSpriteNode instances.
--
-- Generated bindings for @SKTexture@.
module ObjC.GameplayKit.SKTexture
  ( SKTexture
  , IsSKTexture(..)
  , textureWithNoiseMap
  , textureWithNoiseMapSelector


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

import ObjC.GameplayKit.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.SpriteKit.Internal.Classes

-- | Create a texture from a GKNoiseMap.
--
-- @noiseMap@ — the GKNoiseMap from which to create the texture.
--
-- ObjC selector: @+ textureWithNoiseMap:@
textureWithNoiseMap :: IsGKNoiseMap noiseMap => noiseMap -> IO (Id SKTexture)
textureWithNoiseMap noiseMap =
  do
    cls' <- getRequiredClass "SKTexture"
    withObjCPtr noiseMap $ \raw_noiseMap ->
      sendClassMsg cls' (mkSelector "textureWithNoiseMap:") (retPtr retVoid) [argPtr (castPtr raw_noiseMap :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @textureWithNoiseMap:@
textureWithNoiseMapSelector :: Selector
textureWithNoiseMapSelector = mkSelector "textureWithNoiseMap:"

