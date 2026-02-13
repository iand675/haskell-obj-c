{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASECardioidDirectivityModelSubbandParameters
--
-- Cardioid directivity model subband parameters.
--
-- Generated bindings for @PHASECardioidDirectivityModelSubbandParameters@.
module ObjC.PHASE.PHASECardioidDirectivityModelSubbandParameters
  ( PHASECardioidDirectivityModelSubbandParameters
  , IsPHASECardioidDirectivityModelSubbandParameters(..)
  , init_
  , frequency
  , setFrequency
  , pattern_
  , setPattern
  , sharpness
  , setSharpness
  , frequencySelector
  , initSelector
  , patternSelector
  , setFrequencySelector
  , setPatternSelector
  , setSharpnessSelector
  , sharpnessSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PHASE.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPHASECardioidDirectivityModelSubbandParameters phaseCardioidDirectivityModelSubbandParameters => phaseCardioidDirectivityModelSubbandParameters -> IO (Id PHASECardioidDirectivityModelSubbandParameters)
init_ phaseCardioidDirectivityModelSubbandParameters =
  sendOwnedMessage phaseCardioidDirectivityModelSubbandParameters initSelector

-- | frequency
--
-- The frequency of the subband, in hertz.
--
-- Note: Values are clamped to the range [20.0, 20000.0]. Default value is 1000.0.
--
-- ObjC selector: @- frequency@
frequency :: IsPHASECardioidDirectivityModelSubbandParameters phaseCardioidDirectivityModelSubbandParameters => phaseCardioidDirectivityModelSubbandParameters -> IO CDouble
frequency phaseCardioidDirectivityModelSubbandParameters =
  sendMessage phaseCardioidDirectivityModelSubbandParameters frequencySelector

-- | frequency
--
-- The frequency of the subband, in hertz.
--
-- Note: Values are clamped to the range [20.0, 20000.0]. Default value is 1000.0.
--
-- ObjC selector: @- setFrequency:@
setFrequency :: IsPHASECardioidDirectivityModelSubbandParameters phaseCardioidDirectivityModelSubbandParameters => phaseCardioidDirectivityModelSubbandParameters -> CDouble -> IO ()
setFrequency phaseCardioidDirectivityModelSubbandParameters value =
  sendMessage phaseCardioidDirectivityModelSubbandParameters setFrequencySelector value

-- | pattern
--
-- The directivity pattern.
--
-- Note: Values are clamped to the range [0.0, 1.0]. Default value is 0.0. 0.0 is omnidirectional. 0.5 is cardioid. 1.0 is dipole.
--
-- ObjC selector: @- pattern@
pattern_ :: IsPHASECardioidDirectivityModelSubbandParameters phaseCardioidDirectivityModelSubbandParameters => phaseCardioidDirectivityModelSubbandParameters -> IO CDouble
pattern_ phaseCardioidDirectivityModelSubbandParameters =
  sendMessage phaseCardioidDirectivityModelSubbandParameters patternSelector

-- | pattern
--
-- The directivity pattern.
--
-- Note: Values are clamped to the range [0.0, 1.0]. Default value is 0.0. 0.0 is omnidirectional. 0.5 is cardioid. 1.0 is dipole.
--
-- ObjC selector: @- setPattern:@
setPattern :: IsPHASECardioidDirectivityModelSubbandParameters phaseCardioidDirectivityModelSubbandParameters => phaseCardioidDirectivityModelSubbandParameters -> CDouble -> IO ()
setPattern phaseCardioidDirectivityModelSubbandParameters value =
  sendMessage phaseCardioidDirectivityModelSubbandParameters setPatternSelector value

-- | sharpness
--
-- The sharpness of the directivity pattern.
--
-- Note: Values are clamped to the range [1.0, DBL_MAX]. Default value is 1.0. Values > 1.0 increase sharpness.
--
-- ObjC selector: @- sharpness@
sharpness :: IsPHASECardioidDirectivityModelSubbandParameters phaseCardioidDirectivityModelSubbandParameters => phaseCardioidDirectivityModelSubbandParameters -> IO CDouble
sharpness phaseCardioidDirectivityModelSubbandParameters =
  sendMessage phaseCardioidDirectivityModelSubbandParameters sharpnessSelector

-- | sharpness
--
-- The sharpness of the directivity pattern.
--
-- Note: Values are clamped to the range [1.0, DBL_MAX]. Default value is 1.0. Values > 1.0 increase sharpness.
--
-- ObjC selector: @- setSharpness:@
setSharpness :: IsPHASECardioidDirectivityModelSubbandParameters phaseCardioidDirectivityModelSubbandParameters => phaseCardioidDirectivityModelSubbandParameters -> CDouble -> IO ()
setSharpness phaseCardioidDirectivityModelSubbandParameters value =
  sendMessage phaseCardioidDirectivityModelSubbandParameters setSharpnessSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHASECardioidDirectivityModelSubbandParameters)
initSelector = mkSelector "init"

-- | @Selector@ for @frequency@
frequencySelector :: Selector '[] CDouble
frequencySelector = mkSelector "frequency"

-- | @Selector@ for @setFrequency:@
setFrequencySelector :: Selector '[CDouble] ()
setFrequencySelector = mkSelector "setFrequency:"

-- | @Selector@ for @pattern@
patternSelector :: Selector '[] CDouble
patternSelector = mkSelector "pattern"

-- | @Selector@ for @setPattern:@
setPatternSelector :: Selector '[CDouble] ()
setPatternSelector = mkSelector "setPattern:"

-- | @Selector@ for @sharpness@
sharpnessSelector :: Selector '[] CDouble
sharpnessSelector = mkSelector "sharpness"

-- | @Selector@ for @setSharpness:@
setSharpnessSelector :: Selector '[CDouble] ()
setSharpnessSelector = mkSelector "setSharpness:"

