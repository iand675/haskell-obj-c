{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVAudioEnvironmentDistanceAttenuationParameters
--
-- Parameters specifying the amount of distance attenuation
--
-- A standalone instance of AVAudioEnvironmentDistanceAttenuationParameters cannot be created.         Only an instance vended out by a source object (e.g. AVAudioEnvironmentNode) can be used.
--
-- Generated bindings for @AVAudioEnvironmentDistanceAttenuationParameters@.
module ObjC.AVFAudio.AVAudioEnvironmentDistanceAttenuationParameters
  ( AVAudioEnvironmentDistanceAttenuationParameters
  , IsAVAudioEnvironmentDistanceAttenuationParameters(..)
  , init_
  , distanceAttenuationModel
  , setDistanceAttenuationModel
  , referenceDistance
  , setReferenceDistance
  , maximumDistance
  , setMaximumDistance
  , rolloffFactor
  , setRolloffFactor
  , distanceAttenuationModelSelector
  , initSelector
  , maximumDistanceSelector
  , referenceDistanceSelector
  , rolloffFactorSelector
  , setDistanceAttenuationModelSelector
  , setMaximumDistanceSelector
  , setReferenceDistanceSelector
  , setRolloffFactorSelector

  -- * Enum types
  , AVAudioEnvironmentDistanceAttenuationModel(AVAudioEnvironmentDistanceAttenuationModel)
  , pattern AVAudioEnvironmentDistanceAttenuationModelExponential
  , pattern AVAudioEnvironmentDistanceAttenuationModelInverse
  , pattern AVAudioEnvironmentDistanceAttenuationModelLinear

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFAudio.Internal.Classes
import ObjC.AVFAudio.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVAudioEnvironmentDistanceAttenuationParameters avAudioEnvironmentDistanceAttenuationParameters => avAudioEnvironmentDistanceAttenuationParameters -> IO (Id AVAudioEnvironmentDistanceAttenuationParameters)
init_ avAudioEnvironmentDistanceAttenuationParameters =
  sendOwnedMessage avAudioEnvironmentDistanceAttenuationParameters initSelector

-- | distanceAttenuationModel
--
-- Type of distance attenuation model
--
-- Default:    AVAudioEnvironmentDistanceAttenuationModelInverse
--
-- ObjC selector: @- distanceAttenuationModel@
distanceAttenuationModel :: IsAVAudioEnvironmentDistanceAttenuationParameters avAudioEnvironmentDistanceAttenuationParameters => avAudioEnvironmentDistanceAttenuationParameters -> IO AVAudioEnvironmentDistanceAttenuationModel
distanceAttenuationModel avAudioEnvironmentDistanceAttenuationParameters =
  sendMessage avAudioEnvironmentDistanceAttenuationParameters distanceAttenuationModelSelector

-- | distanceAttenuationModel
--
-- Type of distance attenuation model
--
-- Default:    AVAudioEnvironmentDistanceAttenuationModelInverse
--
-- ObjC selector: @- setDistanceAttenuationModel:@
setDistanceAttenuationModel :: IsAVAudioEnvironmentDistanceAttenuationParameters avAudioEnvironmentDistanceAttenuationParameters => avAudioEnvironmentDistanceAttenuationParameters -> AVAudioEnvironmentDistanceAttenuationModel -> IO ()
setDistanceAttenuationModel avAudioEnvironmentDistanceAttenuationParameters value =
  sendMessage avAudioEnvironmentDistanceAttenuationParameters setDistanceAttenuationModelSelector value

-- | referenceDistance
--
-- The minimum distance at which attenuation is applied
--
-- Default:    1.0 meter        Models:     AVAudioEnvironmentDistanceAttenuationModelInverse,                    AVAudioEnvironmentDistanceAttenuationModelLinear
--
-- ObjC selector: @- referenceDistance@
referenceDistance :: IsAVAudioEnvironmentDistanceAttenuationParameters avAudioEnvironmentDistanceAttenuationParameters => avAudioEnvironmentDistanceAttenuationParameters -> IO CFloat
referenceDistance avAudioEnvironmentDistanceAttenuationParameters =
  sendMessage avAudioEnvironmentDistanceAttenuationParameters referenceDistanceSelector

-- | referenceDistance
--
-- The minimum distance at which attenuation is applied
--
-- Default:    1.0 meter        Models:     AVAudioEnvironmentDistanceAttenuationModelInverse,                    AVAudioEnvironmentDistanceAttenuationModelLinear
--
-- ObjC selector: @- setReferenceDistance:@
setReferenceDistance :: IsAVAudioEnvironmentDistanceAttenuationParameters avAudioEnvironmentDistanceAttenuationParameters => avAudioEnvironmentDistanceAttenuationParameters -> CFloat -> IO ()
setReferenceDistance avAudioEnvironmentDistanceAttenuationParameters value =
  sendMessage avAudioEnvironmentDistanceAttenuationParameters setReferenceDistanceSelector value

-- | maximumDistance
--
-- The distance beyond which no further attenuation is applied
--
-- Default:    100000.0 meters        Models:     AVAudioEnvironmentDistanceAttenuationModelLinear
--
-- ObjC selector: @- maximumDistance@
maximumDistance :: IsAVAudioEnvironmentDistanceAttenuationParameters avAudioEnvironmentDistanceAttenuationParameters => avAudioEnvironmentDistanceAttenuationParameters -> IO CFloat
maximumDistance avAudioEnvironmentDistanceAttenuationParameters =
  sendMessage avAudioEnvironmentDistanceAttenuationParameters maximumDistanceSelector

-- | maximumDistance
--
-- The distance beyond which no further attenuation is applied
--
-- Default:    100000.0 meters        Models:     AVAudioEnvironmentDistanceAttenuationModelLinear
--
-- ObjC selector: @- setMaximumDistance:@
setMaximumDistance :: IsAVAudioEnvironmentDistanceAttenuationParameters avAudioEnvironmentDistanceAttenuationParameters => avAudioEnvironmentDistanceAttenuationParameters -> CFloat -> IO ()
setMaximumDistance avAudioEnvironmentDistanceAttenuationParameters value =
  sendMessage avAudioEnvironmentDistanceAttenuationParameters setMaximumDistanceSelector value

-- | rolloffFactor
--
-- Determines the attenuation curve
--
-- A higher value results in a steeper attenuation curve.        The rolloff factor should be a value greater than 0.        Default:    1.0        Models:     AVAudioEnvironmentDistanceAttenuationModelExponential                    AVAudioEnvironmentDistanceAttenuationModelInverse                    AVAudioEnvironmentDistanceAttenuationModelLinear
--
-- ObjC selector: @- rolloffFactor@
rolloffFactor :: IsAVAudioEnvironmentDistanceAttenuationParameters avAudioEnvironmentDistanceAttenuationParameters => avAudioEnvironmentDistanceAttenuationParameters -> IO CFloat
rolloffFactor avAudioEnvironmentDistanceAttenuationParameters =
  sendMessage avAudioEnvironmentDistanceAttenuationParameters rolloffFactorSelector

-- | rolloffFactor
--
-- Determines the attenuation curve
--
-- A higher value results in a steeper attenuation curve.        The rolloff factor should be a value greater than 0.        Default:    1.0        Models:     AVAudioEnvironmentDistanceAttenuationModelExponential                    AVAudioEnvironmentDistanceAttenuationModelInverse                    AVAudioEnvironmentDistanceAttenuationModelLinear
--
-- ObjC selector: @- setRolloffFactor:@
setRolloffFactor :: IsAVAudioEnvironmentDistanceAttenuationParameters avAudioEnvironmentDistanceAttenuationParameters => avAudioEnvironmentDistanceAttenuationParameters -> CFloat -> IO ()
setRolloffFactor avAudioEnvironmentDistanceAttenuationParameters value =
  sendMessage avAudioEnvironmentDistanceAttenuationParameters setRolloffFactorSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVAudioEnvironmentDistanceAttenuationParameters)
initSelector = mkSelector "init"

-- | @Selector@ for @distanceAttenuationModel@
distanceAttenuationModelSelector :: Selector '[] AVAudioEnvironmentDistanceAttenuationModel
distanceAttenuationModelSelector = mkSelector "distanceAttenuationModel"

-- | @Selector@ for @setDistanceAttenuationModel:@
setDistanceAttenuationModelSelector :: Selector '[AVAudioEnvironmentDistanceAttenuationModel] ()
setDistanceAttenuationModelSelector = mkSelector "setDistanceAttenuationModel:"

-- | @Selector@ for @referenceDistance@
referenceDistanceSelector :: Selector '[] CFloat
referenceDistanceSelector = mkSelector "referenceDistance"

-- | @Selector@ for @setReferenceDistance:@
setReferenceDistanceSelector :: Selector '[CFloat] ()
setReferenceDistanceSelector = mkSelector "setReferenceDistance:"

-- | @Selector@ for @maximumDistance@
maximumDistanceSelector :: Selector '[] CFloat
maximumDistanceSelector = mkSelector "maximumDistance"

-- | @Selector@ for @setMaximumDistance:@
setMaximumDistanceSelector :: Selector '[CFloat] ()
setMaximumDistanceSelector = mkSelector "setMaximumDistance:"

-- | @Selector@ for @rolloffFactor@
rolloffFactorSelector :: Selector '[] CFloat
rolloffFactorSelector = mkSelector "rolloffFactor"

-- | @Selector@ for @setRolloffFactor:@
setRolloffFactorSelector :: Selector '[CFloat] ()
setRolloffFactorSelector = mkSelector "setRolloffFactor:"

