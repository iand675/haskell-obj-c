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

-- | @+ billowNoiseSourceWithFrequency:octaveCount:persistence:lacunarity:seed:@
billowNoiseSourceWithFrequency_octaveCount_persistence_lacunarity_seed :: CDouble -> CLong -> CDouble -> CDouble -> CInt -> IO (Id GKBillowNoiseSource)
billowNoiseSourceWithFrequency_octaveCount_persistence_lacunarity_seed frequency octaveCount persistence lacunarity seed =
  do
    cls' <- getRequiredClass "GKBillowNoiseSource"
    sendClassMsg cls' (mkSelector "billowNoiseSourceWithFrequency:octaveCount:persistence:lacunarity:seed:") (retPtr retVoid) [argCDouble (fromIntegral frequency), argCLong (fromIntegral octaveCount), argCDouble (fromIntegral persistence), argCDouble (fromIntegral lacunarity), argCInt (fromIntegral seed)] >>= retainedObject . castPtr

-- | @- initWithFrequency:octaveCount:persistence:lacunarity:seed:@
initWithFrequency_octaveCount_persistence_lacunarity_seed :: IsGKBillowNoiseSource gkBillowNoiseSource => gkBillowNoiseSource -> CDouble -> CLong -> CDouble -> CDouble -> CInt -> IO (Id GKBillowNoiseSource)
initWithFrequency_octaveCount_persistence_lacunarity_seed gkBillowNoiseSource  frequency octaveCount persistence lacunarity seed =
  sendMsg gkBillowNoiseSource (mkSelector "initWithFrequency:octaveCount:persistence:lacunarity:seed:") (retPtr retVoid) [argCDouble (fromIntegral frequency), argCLong (fromIntegral octaveCount), argCDouble (fromIntegral persistence), argCDouble (fromIntegral lacunarity), argCInt (fromIntegral seed)] >>= ownedObject . castPtr

-- | @- persistence@
persistence :: IsGKBillowNoiseSource gkBillowNoiseSource => gkBillowNoiseSource -> IO CDouble
persistence gkBillowNoiseSource  =
  sendMsg gkBillowNoiseSource (mkSelector "persistence") retCDouble []

-- | @- setPersistence:@
setPersistence :: IsGKBillowNoiseSource gkBillowNoiseSource => gkBillowNoiseSource -> CDouble -> IO ()
setPersistence gkBillowNoiseSource  value =
  sendMsg gkBillowNoiseSource (mkSelector "setPersistence:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @billowNoiseSourceWithFrequency:octaveCount:persistence:lacunarity:seed:@
billowNoiseSourceWithFrequency_octaveCount_persistence_lacunarity_seedSelector :: Selector
billowNoiseSourceWithFrequency_octaveCount_persistence_lacunarity_seedSelector = mkSelector "billowNoiseSourceWithFrequency:octaveCount:persistence:lacunarity:seed:"

-- | @Selector@ for @initWithFrequency:octaveCount:persistence:lacunarity:seed:@
initWithFrequency_octaveCount_persistence_lacunarity_seedSelector :: Selector
initWithFrequency_octaveCount_persistence_lacunarity_seedSelector = mkSelector "initWithFrequency:octaveCount:persistence:lacunarity:seed:"

-- | @Selector@ for @persistence@
persistenceSelector :: Selector
persistenceSelector = mkSelector "persistence"

-- | @Selector@ for @setPersistence:@
setPersistenceSelector :: Selector
setPersistenceSelector = mkSelector "setPersistence:"

