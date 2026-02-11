{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , distanceAttenuationModelSelector
  , setDistanceAttenuationModelSelector
  , referenceDistanceSelector
  , setReferenceDistanceSelector
  , maximumDistanceSelector
  , setMaximumDistanceSelector
  , rolloffFactorSelector
  , setRolloffFactorSelector

  -- * Enum types
  , AVAudioEnvironmentDistanceAttenuationModel(AVAudioEnvironmentDistanceAttenuationModel)
  , pattern AVAudioEnvironmentDistanceAttenuationModelExponential
  , pattern AVAudioEnvironmentDistanceAttenuationModelInverse
  , pattern AVAudioEnvironmentDistanceAttenuationModelLinear

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

import ObjC.AVFAudio.Internal.Classes
import ObjC.AVFAudio.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVAudioEnvironmentDistanceAttenuationParameters avAudioEnvironmentDistanceAttenuationParameters => avAudioEnvironmentDistanceAttenuationParameters -> IO (Id AVAudioEnvironmentDistanceAttenuationParameters)
init_ avAudioEnvironmentDistanceAttenuationParameters  =
  sendMsg avAudioEnvironmentDistanceAttenuationParameters (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | distanceAttenuationModel
--
-- Type of distance attenuation model
--
-- Default:    AVAudioEnvironmentDistanceAttenuationModelInverse
--
-- ObjC selector: @- distanceAttenuationModel@
distanceAttenuationModel :: IsAVAudioEnvironmentDistanceAttenuationParameters avAudioEnvironmentDistanceAttenuationParameters => avAudioEnvironmentDistanceAttenuationParameters -> IO AVAudioEnvironmentDistanceAttenuationModel
distanceAttenuationModel avAudioEnvironmentDistanceAttenuationParameters  =
  fmap (coerce :: CLong -> AVAudioEnvironmentDistanceAttenuationModel) $ sendMsg avAudioEnvironmentDistanceAttenuationParameters (mkSelector "distanceAttenuationModel") retCLong []

-- | distanceAttenuationModel
--
-- Type of distance attenuation model
--
-- Default:    AVAudioEnvironmentDistanceAttenuationModelInverse
--
-- ObjC selector: @- setDistanceAttenuationModel:@
setDistanceAttenuationModel :: IsAVAudioEnvironmentDistanceAttenuationParameters avAudioEnvironmentDistanceAttenuationParameters => avAudioEnvironmentDistanceAttenuationParameters -> AVAudioEnvironmentDistanceAttenuationModel -> IO ()
setDistanceAttenuationModel avAudioEnvironmentDistanceAttenuationParameters  value =
  sendMsg avAudioEnvironmentDistanceAttenuationParameters (mkSelector "setDistanceAttenuationModel:") retVoid [argCLong (coerce value)]

-- | referenceDistance
--
-- The minimum distance at which attenuation is applied
--
-- Default:    1.0 meter        Models:     AVAudioEnvironmentDistanceAttenuationModelInverse,                    AVAudioEnvironmentDistanceAttenuationModelLinear
--
-- ObjC selector: @- referenceDistance@
referenceDistance :: IsAVAudioEnvironmentDistanceAttenuationParameters avAudioEnvironmentDistanceAttenuationParameters => avAudioEnvironmentDistanceAttenuationParameters -> IO CFloat
referenceDistance avAudioEnvironmentDistanceAttenuationParameters  =
  sendMsg avAudioEnvironmentDistanceAttenuationParameters (mkSelector "referenceDistance") retCFloat []

-- | referenceDistance
--
-- The minimum distance at which attenuation is applied
--
-- Default:    1.0 meter        Models:     AVAudioEnvironmentDistanceAttenuationModelInverse,                    AVAudioEnvironmentDistanceAttenuationModelLinear
--
-- ObjC selector: @- setReferenceDistance:@
setReferenceDistance :: IsAVAudioEnvironmentDistanceAttenuationParameters avAudioEnvironmentDistanceAttenuationParameters => avAudioEnvironmentDistanceAttenuationParameters -> CFloat -> IO ()
setReferenceDistance avAudioEnvironmentDistanceAttenuationParameters  value =
  sendMsg avAudioEnvironmentDistanceAttenuationParameters (mkSelector "setReferenceDistance:") retVoid [argCFloat (fromIntegral value)]

-- | maximumDistance
--
-- The distance beyond which no further attenuation is applied
--
-- Default:    100000.0 meters        Models:     AVAudioEnvironmentDistanceAttenuationModelLinear
--
-- ObjC selector: @- maximumDistance@
maximumDistance :: IsAVAudioEnvironmentDistanceAttenuationParameters avAudioEnvironmentDistanceAttenuationParameters => avAudioEnvironmentDistanceAttenuationParameters -> IO CFloat
maximumDistance avAudioEnvironmentDistanceAttenuationParameters  =
  sendMsg avAudioEnvironmentDistanceAttenuationParameters (mkSelector "maximumDistance") retCFloat []

-- | maximumDistance
--
-- The distance beyond which no further attenuation is applied
--
-- Default:    100000.0 meters        Models:     AVAudioEnvironmentDistanceAttenuationModelLinear
--
-- ObjC selector: @- setMaximumDistance:@
setMaximumDistance :: IsAVAudioEnvironmentDistanceAttenuationParameters avAudioEnvironmentDistanceAttenuationParameters => avAudioEnvironmentDistanceAttenuationParameters -> CFloat -> IO ()
setMaximumDistance avAudioEnvironmentDistanceAttenuationParameters  value =
  sendMsg avAudioEnvironmentDistanceAttenuationParameters (mkSelector "setMaximumDistance:") retVoid [argCFloat (fromIntegral value)]

-- | rolloffFactor
--
-- Determines the attenuation curve
--
-- A higher value results in a steeper attenuation curve.        The rolloff factor should be a value greater than 0.        Default:    1.0        Models:     AVAudioEnvironmentDistanceAttenuationModelExponential                    AVAudioEnvironmentDistanceAttenuationModelInverse                    AVAudioEnvironmentDistanceAttenuationModelLinear
--
-- ObjC selector: @- rolloffFactor@
rolloffFactor :: IsAVAudioEnvironmentDistanceAttenuationParameters avAudioEnvironmentDistanceAttenuationParameters => avAudioEnvironmentDistanceAttenuationParameters -> IO CFloat
rolloffFactor avAudioEnvironmentDistanceAttenuationParameters  =
  sendMsg avAudioEnvironmentDistanceAttenuationParameters (mkSelector "rolloffFactor") retCFloat []

-- | rolloffFactor
--
-- Determines the attenuation curve
--
-- A higher value results in a steeper attenuation curve.        The rolloff factor should be a value greater than 0.        Default:    1.0        Models:     AVAudioEnvironmentDistanceAttenuationModelExponential                    AVAudioEnvironmentDistanceAttenuationModelInverse                    AVAudioEnvironmentDistanceAttenuationModelLinear
--
-- ObjC selector: @- setRolloffFactor:@
setRolloffFactor :: IsAVAudioEnvironmentDistanceAttenuationParameters avAudioEnvironmentDistanceAttenuationParameters => avAudioEnvironmentDistanceAttenuationParameters -> CFloat -> IO ()
setRolloffFactor avAudioEnvironmentDistanceAttenuationParameters  value =
  sendMsg avAudioEnvironmentDistanceAttenuationParameters (mkSelector "setRolloffFactor:") retVoid [argCFloat (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @distanceAttenuationModel@
distanceAttenuationModelSelector :: Selector
distanceAttenuationModelSelector = mkSelector "distanceAttenuationModel"

-- | @Selector@ for @setDistanceAttenuationModel:@
setDistanceAttenuationModelSelector :: Selector
setDistanceAttenuationModelSelector = mkSelector "setDistanceAttenuationModel:"

-- | @Selector@ for @referenceDistance@
referenceDistanceSelector :: Selector
referenceDistanceSelector = mkSelector "referenceDistance"

-- | @Selector@ for @setReferenceDistance:@
setReferenceDistanceSelector :: Selector
setReferenceDistanceSelector = mkSelector "setReferenceDistance:"

-- | @Selector@ for @maximumDistance@
maximumDistanceSelector :: Selector
maximumDistanceSelector = mkSelector "maximumDistance"

-- | @Selector@ for @setMaximumDistance:@
setMaximumDistanceSelector :: Selector
setMaximumDistanceSelector = mkSelector "setMaximumDistance:"

-- | @Selector@ for @rolloffFactor@
rolloffFactorSelector :: Selector
rolloffFactorSelector = mkSelector "rolloffFactor"

-- | @Selector@ for @setRolloffFactor:@
setRolloffFactorSelector :: Selector
setRolloffFactorSelector = mkSelector "setRolloffFactor:"

