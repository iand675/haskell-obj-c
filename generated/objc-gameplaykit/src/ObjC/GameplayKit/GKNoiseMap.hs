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
  , noiseMapWithNoiseSelector
  , initWithNoiseSelector
  , seamlessSelector


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

-- | Initializes a noise map with constant noise of 0.0 at all positions.
--
-- ObjC selector: @- init@
init_ :: IsGKNoiseMap gkNoiseMap => gkNoiseMap -> IO (Id GKNoiseMap)
init_ gkNoiseMap  =
  sendMsg gkNoiseMap (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Initializes a noise map with specified noise.
--
-- @noise@ — The 3D noise from which to sample a 2D plane.
--
-- ObjC selector: @+ noiseMapWithNoise:@
noiseMapWithNoise :: IsGKNoise noise => noise -> IO (Id GKNoiseMap)
noiseMapWithNoise noise =
  do
    cls' <- getRequiredClass "GKNoiseMap"
    withObjCPtr noise $ \raw_noise ->
      sendClassMsg cls' (mkSelector "noiseMapWithNoise:") (retPtr retVoid) [argPtr (castPtr raw_noise :: Ptr ())] >>= retainedObject . castPtr

-- | Initializes a noise map with specified noise.
--
-- @noise@ — The 3D noise from which to sample a 2D plane.
--
-- ObjC selector: @- initWithNoise:@
initWithNoise :: (IsGKNoiseMap gkNoiseMap, IsGKNoise noise) => gkNoiseMap -> noise -> IO (Id GKNoiseMap)
initWithNoise gkNoiseMap  noise =
withObjCPtr noise $ \raw_noise ->
    sendMsg gkNoiseMap (mkSelector "initWithNoise:") (retPtr retVoid) [argPtr (castPtr raw_noise :: Ptr ())] >>= ownedObject . castPtr

-- | Whether the values at the edges of the 2D plane are modified to allow seamless tiling of the extracted noise map.
--
-- ObjC selector: @- seamless@
seamless :: IsGKNoiseMap gkNoiseMap => gkNoiseMap -> IO Bool
seamless gkNoiseMap  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkNoiseMap (mkSelector "seamless") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @noiseMapWithNoise:@
noiseMapWithNoiseSelector :: Selector
noiseMapWithNoiseSelector = mkSelector "noiseMapWithNoise:"

-- | @Selector@ for @initWithNoise:@
initWithNoiseSelector :: Selector
initWithNoiseSelector = mkSelector "initWithNoise:"

-- | @Selector@ for @seamless@
seamlessSelector :: Selector
seamlessSelector = mkSelector "seamless"

