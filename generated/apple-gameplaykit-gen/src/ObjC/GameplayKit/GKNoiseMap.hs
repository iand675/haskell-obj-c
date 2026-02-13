{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | GKNoiseMap represents an extracted portion of sampled points from continuous 3D noise.  Extracted values are useful for 2D and 3D games.  Noise values may be queried, set to explicit values or used as input for other uses, including textures and tile maps.
--
-- See: GKNoiseSource
--
-- See: GKNoise
--
-- See: SKTexture
--
-- See: SKTileMapNode
--
-- Generated bindings for @GKNoiseMap@.
module ObjC.GameplayKit.GKNoiseMap
  ( GKNoiseMap
  , IsGKNoiseMap(..)
  , init_
  , noiseMapWithNoise
  , initWithNoise
  , seamless
  , initSelector
  , initWithNoiseSelector
  , noiseMapWithNoiseSelector
  , seamlessSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameplayKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initializes a noise map with constant noise of 0.0 at all positions.
--
-- ObjC selector: @- init@
init_ :: IsGKNoiseMap gkNoiseMap => gkNoiseMap -> IO (Id GKNoiseMap)
init_ gkNoiseMap =
  sendOwnedMessage gkNoiseMap initSelector

-- | Initializes a noise map with specified noise.
--
-- @noise@ — The 3D noise from which to sample a 2D plane.
--
-- ObjC selector: @+ noiseMapWithNoise:@
noiseMapWithNoise :: IsGKNoise noise => noise -> IO (Id GKNoiseMap)
noiseMapWithNoise noise =
  do
    cls' <- getRequiredClass "GKNoiseMap"
    sendClassMessage cls' noiseMapWithNoiseSelector (toGKNoise noise)

-- | Initializes a noise map with specified noise.
--
-- @noise@ — The 3D noise from which to sample a 2D plane.
--
-- ObjC selector: @- initWithNoise:@
initWithNoise :: (IsGKNoiseMap gkNoiseMap, IsGKNoise noise) => gkNoiseMap -> noise -> IO (Id GKNoiseMap)
initWithNoise gkNoiseMap noise =
  sendOwnedMessage gkNoiseMap initWithNoiseSelector (toGKNoise noise)

-- | Whether the values at the edges of the 2D plane are modified to allow seamless tiling of the extracted noise map.
--
-- ObjC selector: @- seamless@
seamless :: IsGKNoiseMap gkNoiseMap => gkNoiseMap -> IO Bool
seamless gkNoiseMap =
  sendMessage gkNoiseMap seamlessSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id GKNoiseMap)
initSelector = mkSelector "init"

-- | @Selector@ for @noiseMapWithNoise:@
noiseMapWithNoiseSelector :: Selector '[Id GKNoise] (Id GKNoiseMap)
noiseMapWithNoiseSelector = mkSelector "noiseMapWithNoise:"

-- | @Selector@ for @initWithNoise:@
initWithNoiseSelector :: Selector '[Id GKNoise] (Id GKNoiseMap)
initWithNoiseSelector = mkSelector "initWithNoise:"

-- | @Selector@ for @seamless@
seamlessSelector :: Selector '[] Bool
seamlessSelector = mkSelector "seamless"

