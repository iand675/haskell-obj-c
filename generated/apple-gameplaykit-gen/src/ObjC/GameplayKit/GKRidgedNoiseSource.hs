{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Ridged noise is similar to Perlin noise, with sharply-defined, relatively thin peaks.
--
-- Generated bindings for @GKRidgedNoiseSource@.
module ObjC.GameplayKit.GKRidgedNoiseSource
  ( GKRidgedNoiseSource
  , IsGKRidgedNoiseSource(..)
  , ridgedNoiseSourceWithFrequency_octaveCount_lacunarity_seed
  , initWithFrequency_octaveCount_lacunarity_seed
  , initWithFrequency_octaveCount_lacunarity_seedSelector
  , ridgedNoiseSourceWithFrequency_octaveCount_lacunarity_seedSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameplayKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ ridgedNoiseSourceWithFrequency:octaveCount:lacunarity:seed:@
ridgedNoiseSourceWithFrequency_octaveCount_lacunarity_seed :: CDouble -> CLong -> CDouble -> CInt -> IO (Id GKRidgedNoiseSource)
ridgedNoiseSourceWithFrequency_octaveCount_lacunarity_seed frequency octaveCount lacunarity seed =
  do
    cls' <- getRequiredClass "GKRidgedNoiseSource"
    sendClassMessage cls' ridgedNoiseSourceWithFrequency_octaveCount_lacunarity_seedSelector frequency octaveCount lacunarity seed

-- | @- initWithFrequency:octaveCount:lacunarity:seed:@
initWithFrequency_octaveCount_lacunarity_seed :: IsGKRidgedNoiseSource gkRidgedNoiseSource => gkRidgedNoiseSource -> CDouble -> CLong -> CDouble -> CInt -> IO (Id GKRidgedNoiseSource)
initWithFrequency_octaveCount_lacunarity_seed gkRidgedNoiseSource frequency octaveCount lacunarity seed =
  sendOwnedMessage gkRidgedNoiseSource initWithFrequency_octaveCount_lacunarity_seedSelector frequency octaveCount lacunarity seed

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @ridgedNoiseSourceWithFrequency:octaveCount:lacunarity:seed:@
ridgedNoiseSourceWithFrequency_octaveCount_lacunarity_seedSelector :: Selector '[CDouble, CLong, CDouble, CInt] (Id GKRidgedNoiseSource)
ridgedNoiseSourceWithFrequency_octaveCount_lacunarity_seedSelector = mkSelector "ridgedNoiseSourceWithFrequency:octaveCount:lacunarity:seed:"

-- | @Selector@ for @initWithFrequency:octaveCount:lacunarity:seed:@
initWithFrequency_octaveCount_lacunarity_seedSelector :: Selector '[CDouble, CLong, CDouble, CInt] (Id GKRidgedNoiseSource)
initWithFrequency_octaveCount_lacunarity_seedSelector = mkSelector "initWithFrequency:octaveCount:lacunarity:seed:"

