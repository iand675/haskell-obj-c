{-# LANGUAGE DataKinds #-}
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
  , initWithFrequency_octaveCount_persistence_lacunarity_seedSelector
  , perlinNoiseSourceWithFrequency_octaveCount_persistence_lacunarity_seedSelector
  , persistenceSelector
  , setPersistenceSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameplayKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ perlinNoiseSourceWithFrequency:octaveCount:persistence:lacunarity:seed:@
perlinNoiseSourceWithFrequency_octaveCount_persistence_lacunarity_seed :: CDouble -> CLong -> CDouble -> CDouble -> CInt -> IO (Id GKPerlinNoiseSource)
perlinNoiseSourceWithFrequency_octaveCount_persistence_lacunarity_seed frequency octaveCount persistence lacunarity seed =
  do
    cls' <- getRequiredClass "GKPerlinNoiseSource"
    sendClassMessage cls' perlinNoiseSourceWithFrequency_octaveCount_persistence_lacunarity_seedSelector frequency octaveCount persistence lacunarity seed

-- | @- initWithFrequency:octaveCount:persistence:lacunarity:seed:@
initWithFrequency_octaveCount_persistence_lacunarity_seed :: IsGKPerlinNoiseSource gkPerlinNoiseSource => gkPerlinNoiseSource -> CDouble -> CLong -> CDouble -> CDouble -> CInt -> IO (Id GKPerlinNoiseSource)
initWithFrequency_octaveCount_persistence_lacunarity_seed gkPerlinNoiseSource frequency octaveCount persistence lacunarity seed =
  sendOwnedMessage gkPerlinNoiseSource initWithFrequency_octaveCount_persistence_lacunarity_seedSelector frequency octaveCount persistence lacunarity seed

-- | @- persistence@
persistence :: IsGKPerlinNoiseSource gkPerlinNoiseSource => gkPerlinNoiseSource -> IO CDouble
persistence gkPerlinNoiseSource =
  sendMessage gkPerlinNoiseSource persistenceSelector

-- | @- setPersistence:@
setPersistence :: IsGKPerlinNoiseSource gkPerlinNoiseSource => gkPerlinNoiseSource -> CDouble -> IO ()
setPersistence gkPerlinNoiseSource value =
  sendMessage gkPerlinNoiseSource setPersistenceSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @perlinNoiseSourceWithFrequency:octaveCount:persistence:lacunarity:seed:@
perlinNoiseSourceWithFrequency_octaveCount_persistence_lacunarity_seedSelector :: Selector '[CDouble, CLong, CDouble, CDouble, CInt] (Id GKPerlinNoiseSource)
perlinNoiseSourceWithFrequency_octaveCount_persistence_lacunarity_seedSelector = mkSelector "perlinNoiseSourceWithFrequency:octaveCount:persistence:lacunarity:seed:"

-- | @Selector@ for @initWithFrequency:octaveCount:persistence:lacunarity:seed:@
initWithFrequency_octaveCount_persistence_lacunarity_seedSelector :: Selector '[CDouble, CLong, CDouble, CDouble, CInt] (Id GKPerlinNoiseSource)
initWithFrequency_octaveCount_persistence_lacunarity_seedSelector = mkSelector "initWithFrequency:octaveCount:persistence:lacunarity:seed:"

-- | @Selector@ for @persistence@
persistenceSelector :: Selector '[] CDouble
persistenceSelector = mkSelector "persistence"

-- | @Selector@ for @setPersistence:@
setPersistenceSelector :: Selector '[CDouble] ()
setPersistenceSelector = mkSelector "setPersistence:"

