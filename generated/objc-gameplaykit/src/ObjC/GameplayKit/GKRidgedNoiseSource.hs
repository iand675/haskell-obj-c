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
  , ridgedNoiseSourceWithFrequency_octaveCount_lacunarity_seedSelector
  , initWithFrequency_octaveCount_lacunarity_seedSelector


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

-- | @+ ridgedNoiseSourceWithFrequency:octaveCount:lacunarity:seed:@
ridgedNoiseSourceWithFrequency_octaveCount_lacunarity_seed :: CDouble -> CLong -> CDouble -> CInt -> IO (Id GKRidgedNoiseSource)
ridgedNoiseSourceWithFrequency_octaveCount_lacunarity_seed frequency octaveCount lacunarity seed =
  do
    cls' <- getRequiredClass "GKRidgedNoiseSource"
    sendClassMsg cls' (mkSelector "ridgedNoiseSourceWithFrequency:octaveCount:lacunarity:seed:") (retPtr retVoid) [argCDouble (fromIntegral frequency), argCLong (fromIntegral octaveCount), argCDouble (fromIntegral lacunarity), argCInt (fromIntegral seed)] >>= retainedObject . castPtr

-- | @- initWithFrequency:octaveCount:lacunarity:seed:@
initWithFrequency_octaveCount_lacunarity_seed :: IsGKRidgedNoiseSource gkRidgedNoiseSource => gkRidgedNoiseSource -> CDouble -> CLong -> CDouble -> CInt -> IO (Id GKRidgedNoiseSource)
initWithFrequency_octaveCount_lacunarity_seed gkRidgedNoiseSource  frequency octaveCount lacunarity seed =
  sendMsg gkRidgedNoiseSource (mkSelector "initWithFrequency:octaveCount:lacunarity:seed:") (retPtr retVoid) [argCDouble (fromIntegral frequency), argCLong (fromIntegral octaveCount), argCDouble (fromIntegral lacunarity), argCInt (fromIntegral seed)] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @ridgedNoiseSourceWithFrequency:octaveCount:lacunarity:seed:@
ridgedNoiseSourceWithFrequency_octaveCount_lacunarity_seedSelector :: Selector
ridgedNoiseSourceWithFrequency_octaveCount_lacunarity_seedSelector = mkSelector "ridgedNoiseSourceWithFrequency:octaveCount:lacunarity:seed:"

-- | @Selector@ for @initWithFrequency:octaveCount:lacunarity:seed:@
initWithFrequency_octaveCount_lacunarity_seedSelector :: Selector
initWithFrequency_octaveCount_lacunarity_seedSelector = mkSelector "initWithFrequency:octaveCount:lacunarity:seed:"

