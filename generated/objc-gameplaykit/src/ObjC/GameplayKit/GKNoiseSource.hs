{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A GKNoiseSource instance is a description of procedural noise in 3D space.  Noise sources generate values between -1.0 and 1.0, inclusive, for any position in continuous 3D space. Subclasses represent specific types of noise, each with their own parameters that affect the nature of the noise. Noise sources are the starting point for generating and using procedural noise.  The 3D noise values may be manipulated and combined with the GKNoise class.  Portions of this 3D noise can be extracted and utilized via the GKNoiseMap class. Extracted portions of noise are useful in both 2D and 3D games.  Applications include creating realistic textures, height maps for 2D and 3D game world terrain, tile maps for 2D games, and intentionally imperfect game object and camera movements in 2D and 3D games. This class is not intended to be instantiated.
--
-- See: GKNoise
--
-- See: GKNoiseMap
--
-- Generated bindings for @GKNoiseSource@.
module ObjC.GameplayKit.GKNoiseSource
  ( GKNoiseSource
  , IsGKNoiseSource(..)


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

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

