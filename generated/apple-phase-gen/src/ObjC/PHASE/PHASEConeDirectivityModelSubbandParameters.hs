{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASEConeDirectivityModelSubbandParameters
--
-- Cone directivity model subband parameters.
--
-- Generated bindings for @PHASEConeDirectivityModelSubbandParameters@.
module ObjC.PHASE.PHASEConeDirectivityModelSubbandParameters
  ( PHASEConeDirectivityModelSubbandParameters
  , IsPHASEConeDirectivityModelSubbandParameters(..)
  , init_
  , setInnerAngle_outerAngle
  , frequency
  , setFrequency
  , innerAngle
  , outerAngle
  , outerGain
  , setOuterGain
  , frequencySelector
  , initSelector
  , innerAngleSelector
  , outerAngleSelector
  , outerGainSelector
  , setFrequencySelector
  , setInnerAngle_outerAngleSelector
  , setOuterGainSelector


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
init_ :: IsPHASEConeDirectivityModelSubbandParameters phaseConeDirectivityModelSubbandParameters => phaseConeDirectivityModelSubbandParameters -> IO (Id PHASEConeDirectivityModelSubbandParameters)
init_ phaseConeDirectivityModelSubbandParameters =
  sendOwnedMessage phaseConeDirectivityModelSubbandParameters initSelector

-- | setInnerAngle:outerAngle
--
-- Set the innerAngle and outerAngle of the Cone Directivity Model Subband
--
-- @innerAngle@ — The inner angle of the cone directivity model, must be less than or equal to outer angle.
--
-- @outerAngle@ — The outer angle of the cone directivity model, must be greater than or equal to inner angle.
--
-- Note: The default value for each angle is 360.0. outerAngle must be >= innerAngle.
--
-- ObjC selector: @- setInnerAngle:outerAngle:@
setInnerAngle_outerAngle :: IsPHASEConeDirectivityModelSubbandParameters phaseConeDirectivityModelSubbandParameters => phaseConeDirectivityModelSubbandParameters -> CDouble -> CDouble -> IO ()
setInnerAngle_outerAngle phaseConeDirectivityModelSubbandParameters innerAngle outerAngle =
  sendMessage phaseConeDirectivityModelSubbandParameters setInnerAngle_outerAngleSelector innerAngle outerAngle

-- | frequency
--
-- The frequency of the subband, in hertz.
--
-- Note: Default value is 1000.0.
--
-- ObjC selector: @- frequency@
frequency :: IsPHASEConeDirectivityModelSubbandParameters phaseConeDirectivityModelSubbandParameters => phaseConeDirectivityModelSubbandParameters -> IO CDouble
frequency phaseConeDirectivityModelSubbandParameters =
  sendMessage phaseConeDirectivityModelSubbandParameters frequencySelector

-- | frequency
--
-- The frequency of the subband, in hertz.
--
-- Note: Default value is 1000.0.
--
-- ObjC selector: @- setFrequency:@
setFrequency :: IsPHASEConeDirectivityModelSubbandParameters phaseConeDirectivityModelSubbandParameters => phaseConeDirectivityModelSubbandParameters -> CDouble -> IO ()
setFrequency phaseConeDirectivityModelSubbandParameters value =
  sendMessage phaseConeDirectivityModelSubbandParameters setFrequencySelector value

-- | innerAngle
--
-- The inner angle, in degrees.
--
-- Note: Default value is 360.0.
--
-- ObjC selector: @- innerAngle@
innerAngle :: IsPHASEConeDirectivityModelSubbandParameters phaseConeDirectivityModelSubbandParameters => phaseConeDirectivityModelSubbandParameters -> IO CDouble
innerAngle phaseConeDirectivityModelSubbandParameters =
  sendMessage phaseConeDirectivityModelSubbandParameters innerAngleSelector

-- | outerAngle
--
-- The outer angle, in degrees.
--
-- Note: Default value is 360.0.
--
-- ObjC selector: @- outerAngle@
outerAngle :: IsPHASEConeDirectivityModelSubbandParameters phaseConeDirectivityModelSubbandParameters => phaseConeDirectivityModelSubbandParameters -> IO CDouble
outerAngle phaseConeDirectivityModelSubbandParameters =
  sendMessage phaseConeDirectivityModelSubbandParameters outerAngleSelector

-- | outerGain
--
-- The outer gain.
--
-- Note: Values are clamped to the range [0.0, 1.0]. Default value is 1.0.
--
-- ObjC selector: @- outerGain@
outerGain :: IsPHASEConeDirectivityModelSubbandParameters phaseConeDirectivityModelSubbandParameters => phaseConeDirectivityModelSubbandParameters -> IO CDouble
outerGain phaseConeDirectivityModelSubbandParameters =
  sendMessage phaseConeDirectivityModelSubbandParameters outerGainSelector

-- | outerGain
--
-- The outer gain.
--
-- Note: Values are clamped to the range [0.0, 1.0]. Default value is 1.0.
--
-- ObjC selector: @- setOuterGain:@
setOuterGain :: IsPHASEConeDirectivityModelSubbandParameters phaseConeDirectivityModelSubbandParameters => phaseConeDirectivityModelSubbandParameters -> CDouble -> IO ()
setOuterGain phaseConeDirectivityModelSubbandParameters value =
  sendMessage phaseConeDirectivityModelSubbandParameters setOuterGainSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHASEConeDirectivityModelSubbandParameters)
initSelector = mkSelector "init"

-- | @Selector@ for @setInnerAngle:outerAngle:@
setInnerAngle_outerAngleSelector :: Selector '[CDouble, CDouble] ()
setInnerAngle_outerAngleSelector = mkSelector "setInnerAngle:outerAngle:"

-- | @Selector@ for @frequency@
frequencySelector :: Selector '[] CDouble
frequencySelector = mkSelector "frequency"

-- | @Selector@ for @setFrequency:@
setFrequencySelector :: Selector '[CDouble] ()
setFrequencySelector = mkSelector "setFrequency:"

-- | @Selector@ for @innerAngle@
innerAngleSelector :: Selector '[] CDouble
innerAngleSelector = mkSelector "innerAngle"

-- | @Selector@ for @outerAngle@
outerAngleSelector :: Selector '[] CDouble
outerAngleSelector = mkSelector "outerAngle"

-- | @Selector@ for @outerGain@
outerGainSelector :: Selector '[] CDouble
outerGainSelector = mkSelector "outerGain"

-- | @Selector@ for @setOuterGain:@
setOuterGainSelector :: Selector '[CDouble] ()
setOuterGainSelector = mkSelector "setOuterGain:"

