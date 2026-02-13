{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Billow noise is similar to Perlin noise, with more rounded shapes and clearly-defined transitions beween values.
--
-- Generated bindings for @GKBillowNoiseSource@.
module ObjC.GameplayKit.GKBillowNoiseSource
  ( GKBillowNoiseSource
  , IsGKBillowNoiseSource(..)
  , billowNoiseSourceWithFrequency_octaveCount_persistence_lacunarity_seed
  , initWithFrequency_octaveCount_persistence_lacunarity_seed
  , persistence
  , setPersistence
  , billowNoiseSourceWithFrequency_octaveCount_persistence_lacunarity_seedSelector
  , initWithFrequency_octaveCount_persistence_lacunarity_seedSelector
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

-- | @+ billowNoiseSourceWithFrequency:octaveCount:persistence:lacunarity:seed:@
billowNoiseSourceWithFrequency_octaveCount_persistence_lacunarity_seed :: CDouble -> CLong -> CDouble -> CDouble -> CInt -> IO (Id GKBillowNoiseSource)
billowNoiseSourceWithFrequency_octaveCount_persistence_lacunarity_seed frequency octaveCount persistence lacunarity seed =
  do
    cls' <- getRequiredClass "GKBillowNoiseSource"
    sendClassMessage cls' billowNoiseSourceWithFrequency_octaveCount_persistence_lacunarity_seedSelector frequency octaveCount persistence lacunarity seed

-- | @- initWithFrequency:octaveCount:persistence:lacunarity:seed:@
initWithFrequency_octaveCount_persistence_lacunarity_seed :: IsGKBillowNoiseSource gkBillowNoiseSource => gkBillowNoiseSource -> CDouble -> CLong -> CDouble -> CDouble -> CInt -> IO (Id GKBillowNoiseSource)
initWithFrequency_octaveCount_persistence_lacunarity_seed gkBillowNoiseSource frequency octaveCount persistence lacunarity seed =
  sendOwnedMessage gkBillowNoiseSource initWithFrequency_octaveCount_persistence_lacunarity_seedSelector frequency octaveCount persistence lacunarity seed

-- | @- persistence@
persistence :: IsGKBillowNoiseSource gkBillowNoiseSource => gkBillowNoiseSource -> IO CDouble
persistence gkBillowNoiseSource =
  sendMessage gkBillowNoiseSource persistenceSelector

-- | @- setPersistence:@
setPersistence :: IsGKBillowNoiseSource gkBillowNoiseSource => gkBillowNoiseSource -> CDouble -> IO ()
setPersistence gkBillowNoiseSource value =
  sendMessage gkBillowNoiseSource setPersistenceSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @billowNoiseSourceWithFrequency:octaveCount:persistence:lacunarity:seed:@
billowNoiseSourceWithFrequency_octaveCount_persistence_lacunarity_seedSelector :: Selector '[CDouble, CLong, CDouble, CDouble, CInt] (Id GKBillowNoiseSource)
billowNoiseSourceWithFrequency_octaveCount_persistence_lacunarity_seedSelector = mkSelector "billowNoiseSourceWithFrequency:octaveCount:persistence:lacunarity:seed:"

-- | @Selector@ for @initWithFrequency:octaveCount:persistence:lacunarity:seed:@
initWithFrequency_octaveCount_persistence_lacunarity_seedSelector :: Selector '[CDouble, CLong, CDouble, CDouble, CInt] (Id GKBillowNoiseSource)
initWithFrequency_octaveCount_persistence_lacunarity_seedSelector = mkSelector "initWithFrequency:octaveCount:persistence:lacunarity:seed:"

-- | @Selector@ for @persistence@
persistenceSelector :: Selector '[] CDouble
persistenceSelector = mkSelector "persistence"

-- | @Selector@ for @setPersistence:@
setPersistenceSelector :: Selector '[CDouble] ()
setPersistenceSelector = mkSelector "setPersistence:"

