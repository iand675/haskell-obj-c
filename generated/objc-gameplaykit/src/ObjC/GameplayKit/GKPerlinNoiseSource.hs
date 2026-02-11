{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Perlin noise is useful for creating natural-looking textures and realistic-looking terrain.
--
-- Generated bindings for @GKPerlinNoiseSource@.
module ObjC.GameplayKit.GKPerlinNoiseSource
  ( GKPerlinNoiseSource
  , IsGKPerlinNoiseSource(..)
  , perlinNoiseSourceWithFrequency_octaveCount_persistence_lacunarity_seed
  , initWithFrequency_octaveCount_persistence_lacunarity_seed
  , persistence
  , setPersistence
  , perlinNoiseSourceWithFrequency_octaveCount_persistence_lacunarity_seedSelector
  , initWithFrequency_octaveCount_persistence_lacunarity_seedSelector
  , persistenceSelector
  , setPersistenceSelector


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

-- | @+ perlinNoiseSourceWithFrequency:octaveCount:persistence:lacunarity:seed:@
perlinNoiseSourceWithFrequency_octaveCount_persistence_lacunarity_seed :: CDouble -> CLong -> CDouble -> CDouble -> CInt -> IO (Id GKPerlinNoiseSource)
perlinNoiseSourceWithFrequency_octaveCount_persistence_lacunarity_seed frequency octaveCount persistence lacunarity seed =
  do
    cls' <- getRequiredClass "GKPerlinNoiseSource"
    sendClassMsg cls' (mkSelector "perlinNoiseSourceWithFrequency:octaveCount:persistence:lacunarity:seed:") (retPtr retVoid) [argCDouble (fromIntegral frequency), argCLong (fromIntegral octaveCount), argCDouble (fromIntegral persistence), argCDouble (fromIntegral lacunarity), argCInt (fromIntegral seed)] >>= retainedObject . castPtr

-- | @- initWithFrequency:octaveCount:persistence:lacunarity:seed:@
initWithFrequency_octaveCount_persistence_lacunarity_seed :: IsGKPerlinNoiseSource gkPerlinNoiseSource => gkPerlinNoiseSource -> CDouble -> CLong -> CDouble -> CDouble -> CInt -> IO (Id GKPerlinNoiseSource)
initWithFrequency_octaveCount_persistence_lacunarity_seed gkPerlinNoiseSource  frequency octaveCount persistence lacunarity seed =
  sendMsg gkPerlinNoiseSource (mkSelector "initWithFrequency:octaveCount:persistence:lacunarity:seed:") (retPtr retVoid) [argCDouble (fromIntegral frequency), argCLong (fromIntegral octaveCount), argCDouble (fromIntegral persistence), argCDouble (fromIntegral lacunarity), argCInt (fromIntegral seed)] >>= ownedObject . castPtr

-- | @- persistence@
persistence :: IsGKPerlinNoiseSource gkPerlinNoiseSource => gkPerlinNoiseSource -> IO CDouble
persistence gkPerlinNoiseSource  =
  sendMsg gkPerlinNoiseSource (mkSelector "persistence") retCDouble []

-- | @- setPersistence:@
setPersistence :: IsGKPerlinNoiseSource gkPerlinNoiseSource => gkPerlinNoiseSource -> CDouble -> IO ()
setPersistence gkPerlinNoiseSource  value =
  sendMsg gkPerlinNoiseSource (mkSelector "setPersistence:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @perlinNoiseSourceWithFrequency:octaveCount:persistence:lacunarity:seed:@
perlinNoiseSourceWithFrequency_octaveCount_persistence_lacunarity_seedSelector :: Selector
perlinNoiseSourceWithFrequency_octaveCount_persistence_lacunarity_seedSelector = mkSelector "perlinNoiseSourceWithFrequency:octaveCount:persistence:lacunarity:seed:"

-- | @Selector@ for @initWithFrequency:octaveCount:persistence:lacunarity:seed:@
initWithFrequency_octaveCount_persistence_lacunarity_seedSelector :: Selector
initWithFrequency_octaveCount_persistence_lacunarity_seedSelector = mkSelector "initWithFrequency:octaveCount:persistence:lacunarity:seed:"

-- | @Selector@ for @persistence@
persistenceSelector :: Selector
persistenceSelector = mkSelector "persistence"

-- | @Selector@ for @setPersistence:@
setPersistenceSelector :: Selector
setPersistenceSelector = mkSelector "setPersistence:"

