{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' textureWithNoiseMapSelector (toGKNoiseMap noiseMap)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @textureWithNoiseMap:@
textureWithNoiseMapSelector :: Selector '[Id GKNoiseMap] (Id SKTexture)
textureWithNoiseMapSelector = mkSelector "textureWithNoiseMap:"

