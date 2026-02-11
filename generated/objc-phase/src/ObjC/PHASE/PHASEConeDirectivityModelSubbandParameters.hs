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
  , initSelector
  , setInnerAngle_outerAngleSelector
  , frequencySelector
  , setFrequencySelector
  , innerAngleSelector
  , outerAngleSelector
  , outerGainSelector
  , setOuterGainSelector


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

import ObjC.PHASE.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPHASEConeDirectivityModelSubbandParameters phaseConeDirectivityModelSubbandParameters => phaseConeDirectivityModelSubbandParameters -> IO (Id PHASEConeDirectivityModelSubbandParameters)
init_ phaseConeDirectivityModelSubbandParameters  =
  sendMsg phaseConeDirectivityModelSubbandParameters (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

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
setInnerAngle_outerAngle phaseConeDirectivityModelSubbandParameters  innerAngle outerAngle =
  sendMsg phaseConeDirectivityModelSubbandParameters (mkSelector "setInnerAngle:outerAngle:") retVoid [argCDouble (fromIntegral innerAngle), argCDouble (fromIntegral outerAngle)]

-- | frequency
--
-- The frequency of the subband, in hertz.
--
-- Note: Default value is 1000.0.
--
-- ObjC selector: @- frequency@
frequency :: IsPHASEConeDirectivityModelSubbandParameters phaseConeDirectivityModelSubbandParameters => phaseConeDirectivityModelSubbandParameters -> IO CDouble
frequency phaseConeDirectivityModelSubbandParameters  =
  sendMsg phaseConeDirectivityModelSubbandParameters (mkSelector "frequency") retCDouble []

-- | frequency
--
-- The frequency of the subband, in hertz.
--
-- Note: Default value is 1000.0.
--
-- ObjC selector: @- setFrequency:@
setFrequency :: IsPHASEConeDirectivityModelSubbandParameters phaseConeDirectivityModelSubbandParameters => phaseConeDirectivityModelSubbandParameters -> CDouble -> IO ()
setFrequency phaseConeDirectivityModelSubbandParameters  value =
  sendMsg phaseConeDirectivityModelSubbandParameters (mkSelector "setFrequency:") retVoid [argCDouble (fromIntegral value)]

-- | innerAngle
--
-- The inner angle, in degrees.
--
-- Note: Default value is 360.0.
--
-- ObjC selector: @- innerAngle@
innerAngle :: IsPHASEConeDirectivityModelSubbandParameters phaseConeDirectivityModelSubbandParameters => phaseConeDirectivityModelSubbandParameters -> IO CDouble
innerAngle phaseConeDirectivityModelSubbandParameters  =
  sendMsg phaseConeDirectivityModelSubbandParameters (mkSelector "innerAngle") retCDouble []

-- | outerAngle
--
-- The outer angle, in degrees.
--
-- Note: Default value is 360.0.
--
-- ObjC selector: @- outerAngle@
outerAngle :: IsPHASEConeDirectivityModelSubbandParameters phaseConeDirectivityModelSubbandParameters => phaseConeDirectivityModelSubbandParameters -> IO CDouble
outerAngle phaseConeDirectivityModelSubbandParameters  =
  sendMsg phaseConeDirectivityModelSubbandParameters (mkSelector "outerAngle") retCDouble []

-- | outerGain
--
-- The outer gain.
--
-- Note: Values are clamped to the range [0.0, 1.0]. Default value is 1.0.
--
-- ObjC selector: @- outerGain@
outerGain :: IsPHASEConeDirectivityModelSubbandParameters phaseConeDirectivityModelSubbandParameters => phaseConeDirectivityModelSubbandParameters -> IO CDouble
outerGain phaseConeDirectivityModelSubbandParameters  =
  sendMsg phaseConeDirectivityModelSubbandParameters (mkSelector "outerGain") retCDouble []

-- | outerGain
--
-- The outer gain.
--
-- Note: Values are clamped to the range [0.0, 1.0]. Default value is 1.0.
--
-- ObjC selector: @- setOuterGain:@
setOuterGain :: IsPHASEConeDirectivityModelSubbandParameters phaseConeDirectivityModelSubbandParameters => phaseConeDirectivityModelSubbandParameters -> CDouble -> IO ()
setOuterGain phaseConeDirectivityModelSubbandParameters  value =
  sendMsg phaseConeDirectivityModelSubbandParameters (mkSelector "setOuterGain:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @setInnerAngle:outerAngle:@
setInnerAngle_outerAngleSelector :: Selector
setInnerAngle_outerAngleSelector = mkSelector "setInnerAngle:outerAngle:"

-- | @Selector@ for @frequency@
frequencySelector :: Selector
frequencySelector = mkSelector "frequency"

-- | @Selector@ for @setFrequency:@
setFrequencySelector :: Selector
setFrequencySelector = mkSelector "setFrequency:"

-- | @Selector@ for @innerAngle@
innerAngleSelector :: Selector
innerAngleSelector = mkSelector "innerAngle"

-- | @Selector@ for @outerAngle@
outerAngleSelector :: Selector
outerAngleSelector = mkSelector "outerAngle"

-- | @Selector@ for @outerGain@
outerGainSelector :: Selector
outerGainSelector = mkSelector "outerGain"

-- | @Selector@ for @setOuterGain:@
setOuterGainSelector :: Selector
setOuterGainSelector = mkSelector "setOuterGain:"

