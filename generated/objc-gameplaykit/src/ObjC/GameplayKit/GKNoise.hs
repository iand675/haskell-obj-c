{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | GKNoise is the object used to manipulate and combine noise in continuous 3D space.  It takes a GKNoiseSource as input. To extract and use a portion of the noise within the 3D space use the GKNoiseMap class.
--
-- See: GKNoiseSource
--
-- See: GKNoiseMap
--
-- Generated bindings for @GKNoise@.
module ObjC.GameplayKit.GKNoise
  ( GKNoise
  , IsGKNoise(..)
  , init_
  , noiseWithNoiseSource
  , noiseWithNoiseSource_gradientColors
  , initWithNoiseSource
  , initWithNoiseSource_gradientColors
  , noiseWithComponentNoises_selectionNoise
  , noiseWithComponentNoises_selectionNoise_componentBoundaries_boundaryBlendDistances
  , applyAbsoluteValue
  , clampWithLowerBound_upperBound
  , raiseToPower
  , invert
  , applyTurbulenceWithFrequency_power_roughness_seed
  , remapValuesToCurveWithControlPoints
  , remapValuesToTerracesWithPeaks_terracesInverted
  , addWithNoise
  , multiplyWithNoise
  , minimumWithNoise
  , maximumWithNoise
  , raiseToPowerWithNoise
  , displaceXWithNoise_yWithNoise_zWithNoise
  , gradientColors
  , setGradientColors
  , initSelector
  , noiseWithNoiseSourceSelector
  , noiseWithNoiseSource_gradientColorsSelector
  , initWithNoiseSourceSelector
  , initWithNoiseSource_gradientColorsSelector
  , noiseWithComponentNoises_selectionNoiseSelector
  , noiseWithComponentNoises_selectionNoise_componentBoundaries_boundaryBlendDistancesSelector
  , applyAbsoluteValueSelector
  , clampWithLowerBound_upperBoundSelector
  , raiseToPowerSelector
  , invertSelector
  , applyTurbulenceWithFrequency_power_roughness_seedSelector
  , remapValuesToCurveWithControlPointsSelector
  , remapValuesToTerracesWithPeaks_terracesInvertedSelector
  , addWithNoiseSelector
  , multiplyWithNoiseSelector
  , minimumWithNoiseSelector
  , maximumWithNoiseSelector
  , raiseToPowerWithNoiseSelector
  , displaceXWithNoise_yWithNoise_zWithNoiseSelector
  , gradientColorsSelector
  , setGradientColorsSelector


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
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initializes a constant noise of 0.0 at all positions.
--
-- ObjC selector: @- init@
init_ :: IsGKNoise gkNoise => gkNoise -> IO (Id GKNoise)
init_ gkNoise  =
  sendMsg gkNoise (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Initializes a noise with the specified noise source.
--
-- @noiseSource@ — The noise source to use to initially populate the 3D noise space.
--
-- ObjC selector: @+ noiseWithNoiseSource:@
noiseWithNoiseSource :: IsGKNoiseSource noiseSource => noiseSource -> IO (Id GKNoise)
noiseWithNoiseSource noiseSource =
  do
    cls' <- getRequiredClass "GKNoise"
    withObjCPtr noiseSource $ \raw_noiseSource ->
      sendClassMsg cls' (mkSelector "noiseWithNoiseSource:") (retPtr retVoid) [argPtr (castPtr raw_noiseSource :: Ptr ())] >>= retainedObject . castPtr

-- | Initializes a noise with the specified noise source and parameters.
--
-- @noiseSource@ — The noise source to use to initially populate the 3D noise space.
--
-- @gradientColors@ — The color gradient to use for this noise in 'value : color' pairs.
--
-- ObjC selector: @+ noiseWithNoiseSource:gradientColors:@
noiseWithNoiseSource_gradientColors :: (IsGKNoiseSource noiseSource, IsNSDictionary gradientColors) => noiseSource -> gradientColors -> IO (Id GKNoise)
noiseWithNoiseSource_gradientColors noiseSource gradientColors =
  do
    cls' <- getRequiredClass "GKNoise"
    withObjCPtr noiseSource $ \raw_noiseSource ->
      withObjCPtr gradientColors $ \raw_gradientColors ->
        sendClassMsg cls' (mkSelector "noiseWithNoiseSource:gradientColors:") (retPtr retVoid) [argPtr (castPtr raw_noiseSource :: Ptr ()), argPtr (castPtr raw_gradientColors :: Ptr ())] >>= retainedObject . castPtr

-- | Initializes a noise with the specified noise source.
--
-- @noiseSource@ — The noise source to use to initially populate the 3D noise space.
--
-- ObjC selector: @- initWithNoiseSource:@
initWithNoiseSource :: (IsGKNoise gkNoise, IsGKNoiseSource noiseSource) => gkNoise -> noiseSource -> IO (Id GKNoise)
initWithNoiseSource gkNoise  noiseSource =
withObjCPtr noiseSource $ \raw_noiseSource ->
    sendMsg gkNoise (mkSelector "initWithNoiseSource:") (retPtr retVoid) [argPtr (castPtr raw_noiseSource :: Ptr ())] >>= ownedObject . castPtr

-- | Initializes a noise with the specified noise source and parameters.
--
-- @noiseSource@ — The noise source to use to initially populate the 3D noise space.
--
-- @gradientColors@ — The color gradient to use for this noise in 'value : color' pairs.
--
-- ObjC selector: @- initWithNoiseSource:gradientColors:@
initWithNoiseSource_gradientColors :: (IsGKNoise gkNoise, IsGKNoiseSource noiseSource, IsNSDictionary gradientColors) => gkNoise -> noiseSource -> gradientColors -> IO (Id GKNoise)
initWithNoiseSource_gradientColors gkNoise  noiseSource gradientColors =
withObjCPtr noiseSource $ \raw_noiseSource ->
  withObjCPtr gradientColors $ \raw_gradientColors ->
      sendMsg gkNoise (mkSelector "initWithNoiseSource:gradientColors:") (retPtr retVoid) [argPtr (castPtr raw_noiseSource :: Ptr ()), argPtr (castPtr raw_gradientColors :: Ptr ())] >>= ownedObject . castPtr

-- | Initializes a composite noise from one or more component noises.  Useful for combining and layering noises together.
--
-- @noises@ — The component noises to combine.
--
-- @selectionNoise@ — The noise that governs which component noise is chosen for each position of the resulting noise. The range of values is equally-subdivided for each component noise.
--
-- ObjC selector: @+ noiseWithComponentNoises:selectionNoise:@
noiseWithComponentNoises_selectionNoise :: (IsNSArray noises, IsGKNoise selectionNoise) => noises -> selectionNoise -> IO (Id GKNoise)
noiseWithComponentNoises_selectionNoise noises selectionNoise =
  do
    cls' <- getRequiredClass "GKNoise"
    withObjCPtr noises $ \raw_noises ->
      withObjCPtr selectionNoise $ \raw_selectionNoise ->
        sendClassMsg cls' (mkSelector "noiseWithComponentNoises:selectionNoise:") (retPtr retVoid) [argPtr (castPtr raw_noises :: Ptr ()), argPtr (castPtr raw_selectionNoise :: Ptr ())] >>= retainedObject . castPtr

-- | Initializes a composite noise from one or more component noises.  Useful for combining and layering noises together.
--
-- @noises@ — The component noises to combine.
--
-- @selectionNoise@ — The noise that governs which component noise is chosen for each position of the resulting noise. The range of values is equally-subdivided for each component noise.
--
-- @componentBoundaries@ — The noise value boundaries of the selection noise to use for the component noises.  Specify one less boundary than the number of component noises.  This is a parallel array to blendDistances.
--
-- @blendDistances@ — The size of smoothing that is applied to boundaries where two component noises meet.  Specify one less blend distance than the number of component noises.  This is a parallel array to componentBoundaries.
--
-- ObjC selector: @+ noiseWithComponentNoises:selectionNoise:componentBoundaries:boundaryBlendDistances:@
noiseWithComponentNoises_selectionNoise_componentBoundaries_boundaryBlendDistances :: (IsNSArray noises, IsGKNoise selectionNoise, IsNSArray componentBoundaries, IsNSArray blendDistances) => noises -> selectionNoise -> componentBoundaries -> blendDistances -> IO (Id GKNoise)
noiseWithComponentNoises_selectionNoise_componentBoundaries_boundaryBlendDistances noises selectionNoise componentBoundaries blendDistances =
  do
    cls' <- getRequiredClass "GKNoise"
    withObjCPtr noises $ \raw_noises ->
      withObjCPtr selectionNoise $ \raw_selectionNoise ->
        withObjCPtr componentBoundaries $ \raw_componentBoundaries ->
          withObjCPtr blendDistances $ \raw_blendDistances ->
            sendClassMsg cls' (mkSelector "noiseWithComponentNoises:selectionNoise:componentBoundaries:boundaryBlendDistances:") (retPtr retVoid) [argPtr (castPtr raw_noises :: Ptr ()), argPtr (castPtr raw_selectionNoise :: Ptr ()), argPtr (castPtr raw_componentBoundaries :: Ptr ()), argPtr (castPtr raw_blendDistances :: Ptr ())] >>= retainedObject . castPtr

-- | Takes the absoltue value of all noise positions.
--
-- ObjC selector: @- applyAbsoluteValue@
applyAbsoluteValue :: IsGKNoise gkNoise => gkNoise -> IO ()
applyAbsoluteValue gkNoise  =
  sendMsg gkNoise (mkSelector "applyAbsoluteValue") retVoid []

-- | Clamps all noise values to the specified bounds.
--
-- @lowerBound@ — The noise value lower bound.
--
-- @upperBound@ — The noise value upper bound.
--
-- ObjC selector: @- clampWithLowerBound:upperBound:@
clampWithLowerBound_upperBound :: IsGKNoise gkNoise => gkNoise -> CDouble -> CDouble -> IO ()
clampWithLowerBound_upperBound gkNoise  lowerBound upperBound =
  sendMsg gkNoise (mkSelector "clampWithLowerBound:upperBound:") retVoid [argCDouble (fromIntegral lowerBound), argCDouble (fromIntegral upperBound)]

-- | Raises all noise values to the specified power.
--
-- @power@ — The power to which to raise all noise values.
--
-- ObjC selector: @- raiseToPower:@
raiseToPower :: IsGKNoise gkNoise => gkNoise -> CDouble -> IO ()
raiseToPower gkNoise  power =
  sendMsg gkNoise (mkSelector "raiseToPower:") retVoid [argCDouble (fromIntegral power)]

-- | Inverts all noise values, from positive to negative and vice versa.
--
-- ObjC selector: @- invert@
invert :: IsGKNoise gkNoise => gkNoise -> IO ()
invert gkNoise  =
  sendMsg gkNoise (mkSelector "invert") retVoid []

-- | Applies a turbulent displacement to all noise values.
--
-- ObjC selector: @- applyTurbulenceWithFrequency:power:roughness:seed:@
applyTurbulenceWithFrequency_power_roughness_seed :: IsGKNoise gkNoise => gkNoise -> CDouble -> CDouble -> CInt -> CInt -> IO ()
applyTurbulenceWithFrequency_power_roughness_seed gkNoise  frequency power roughness seed =
  sendMsg gkNoise (mkSelector "applyTurbulenceWithFrequency:power:roughness:seed:") retVoid [argCDouble (fromIntegral frequency), argCDouble (fromIntegral power), argCInt (fromIntegral roughness), argCInt (fromIntegral seed)]

-- | Remaps all noise values to a smooth curve that passes through the specified control points.
--
-- @controlPoints@ — Pairs of 'input : output' values to use as control points for the smooth remapping curve. Duplicate input values are not permitted.
--
-- ObjC selector: @- remapValuesToCurveWithControlPoints:@
remapValuesToCurveWithControlPoints :: (IsGKNoise gkNoise, IsNSDictionary controlPoints) => gkNoise -> controlPoints -> IO ()
remapValuesToCurveWithControlPoints gkNoise  controlPoints =
withObjCPtr controlPoints $ \raw_controlPoints ->
    sendMsg gkNoise (mkSelector "remapValuesToCurveWithControlPoints:") retVoid [argPtr (castPtr raw_controlPoints :: Ptr ())]

-- | Remaps all noise values to one or more terraces with peaks.  Useful for creating valleys and trenches.
--
-- @peakInputValues@ — Inputs positions of terrace peaks.
--
-- @inverted@ — Governs the curve direction from peak to peak.
--
-- ObjC selector: @- remapValuesToTerracesWithPeaks:terracesInverted:@
remapValuesToTerracesWithPeaks_terracesInverted :: (IsGKNoise gkNoise, IsNSArray peakInputValues) => gkNoise -> peakInputValues -> Bool -> IO ()
remapValuesToTerracesWithPeaks_terracesInverted gkNoise  peakInputValues inverted =
withObjCPtr peakInputValues $ \raw_peakInputValues ->
    sendMsg gkNoise (mkSelector "remapValuesToTerracesWithPeaks:terracesInverted:") retVoid [argPtr (castPtr raw_peakInputValues :: Ptr ()), argCULong (if inverted then 1 else 0)]

-- | Adds all noise values by the noise values at the same position in specified noise.
--
-- @noise@ — The noise from which to add values to this noise.
--
-- ObjC selector: @- addWithNoise:@
addWithNoise :: (IsGKNoise gkNoise, IsGKNoise noise) => gkNoise -> noise -> IO ()
addWithNoise gkNoise  noise =
withObjCPtr noise $ \raw_noise ->
    sendMsg gkNoise (mkSelector "addWithNoise:") retVoid [argPtr (castPtr raw_noise :: Ptr ())]

-- | Multiplies all noise values by the noise values at the same position in specified noise.
--
-- @noise@ — The noise from which to multiply values to this noise.
--
-- ObjC selector: @- multiplyWithNoise:@
multiplyWithNoise :: (IsGKNoise gkNoise, IsGKNoise noise) => gkNoise -> noise -> IO ()
multiplyWithNoise gkNoise  noise =
withObjCPtr noise $ \raw_noise ->
    sendMsg gkNoise (mkSelector "multiplyWithNoise:") retVoid [argPtr (castPtr raw_noise :: Ptr ())]

-- | Takes the minimum value between this noise and the specified noise at each position.
--
-- @noise@ — The noise to compare against this noise at each position in determining which to take the minimum value from.
--
-- ObjC selector: @- minimumWithNoise:@
minimumWithNoise :: (IsGKNoise gkNoise, IsGKNoise noise) => gkNoise -> noise -> IO ()
minimumWithNoise gkNoise  noise =
withObjCPtr noise $ \raw_noise ->
    sendMsg gkNoise (mkSelector "minimumWithNoise:") retVoid [argPtr (castPtr raw_noise :: Ptr ())]

-- | Takes the maximum value between this noise and the specified noise at each position.
--
-- @noise@ — The noise to compare against this noise at each position in determining which to take the maximum value from.
--
-- ObjC selector: @- maximumWithNoise:@
maximumWithNoise :: (IsGKNoise gkNoise, IsGKNoise noise) => gkNoise -> noise -> IO ()
maximumWithNoise gkNoise  noise =
withObjCPtr noise $ \raw_noise ->
    sendMsg gkNoise (mkSelector "maximumWithNoise:") retVoid [argPtr (castPtr raw_noise :: Ptr ())]

-- | Raises all noise values to the power of the value at the same position of the specified noise.
--
-- @noise@ — The noise from which to raise this noise's values by.
--
-- ObjC selector: @- raiseToPowerWithNoise:@
raiseToPowerWithNoise :: (IsGKNoise gkNoise, IsGKNoise noise) => gkNoise -> noise -> IO ()
raiseToPowerWithNoise gkNoise  noise =
withObjCPtr noise $ \raw_noise ->
    sendMsg gkNoise (mkSelector "raiseToPowerWithNoise:") retVoid [argPtr (castPtr raw_noise :: Ptr ())]

-- | Displaces all noise values by the values at the same positions of the specified noises.
--
-- @xDisplacementNoise@ — The noise from which to displace along the x-axis this noise's values at the same positions.
--
-- @yDisplacementNoise@ — The noise from which to displace along the y-axis this noise's values at the same positions.
--
-- @zDisplacementNoise@ — The noise from which to displace along the z-axis this noise's values at the same positions.
--
-- ObjC selector: @- displaceXWithNoise:yWithNoise:zWithNoise:@
displaceXWithNoise_yWithNoise_zWithNoise :: (IsGKNoise gkNoise, IsGKNoise xDisplacementNoise, IsGKNoise yDisplacementNoise, IsGKNoise zDisplacementNoise) => gkNoise -> xDisplacementNoise -> yDisplacementNoise -> zDisplacementNoise -> IO ()
displaceXWithNoise_yWithNoise_zWithNoise gkNoise  xDisplacementNoise yDisplacementNoise zDisplacementNoise =
withObjCPtr xDisplacementNoise $ \raw_xDisplacementNoise ->
  withObjCPtr yDisplacementNoise $ \raw_yDisplacementNoise ->
    withObjCPtr zDisplacementNoise $ \raw_zDisplacementNoise ->
        sendMsg gkNoise (mkSelector "displaceXWithNoise:yWithNoise:zWithNoise:") retVoid [argPtr (castPtr raw_xDisplacementNoise :: Ptr ()), argPtr (castPtr raw_yDisplacementNoise :: Ptr ()), argPtr (castPtr raw_zDisplacementNoise :: Ptr ())]

-- | Color gradient of this noise, represented as 'value : color' pairs.  Utilized when this noise is rendered to a texture.
--
-- ObjC selector: @- gradientColors@
gradientColors :: IsGKNoise gkNoise => gkNoise -> IO (Id NSDictionary)
gradientColors gkNoise  =
  sendMsg gkNoise (mkSelector "gradientColors") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Color gradient of this noise, represented as 'value : color' pairs.  Utilized when this noise is rendered to a texture.
--
-- ObjC selector: @- setGradientColors:@
setGradientColors :: (IsGKNoise gkNoise, IsNSDictionary value) => gkNoise -> value -> IO ()
setGradientColors gkNoise  value =
withObjCPtr value $ \raw_value ->
    sendMsg gkNoise (mkSelector "setGradientColors:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @noiseWithNoiseSource:@
noiseWithNoiseSourceSelector :: Selector
noiseWithNoiseSourceSelector = mkSelector "noiseWithNoiseSource:"

-- | @Selector@ for @noiseWithNoiseSource:gradientColors:@
noiseWithNoiseSource_gradientColorsSelector :: Selector
noiseWithNoiseSource_gradientColorsSelector = mkSelector "noiseWithNoiseSource:gradientColors:"

-- | @Selector@ for @initWithNoiseSource:@
initWithNoiseSourceSelector :: Selector
initWithNoiseSourceSelector = mkSelector "initWithNoiseSource:"

-- | @Selector@ for @initWithNoiseSource:gradientColors:@
initWithNoiseSource_gradientColorsSelector :: Selector
initWithNoiseSource_gradientColorsSelector = mkSelector "initWithNoiseSource:gradientColors:"

-- | @Selector@ for @noiseWithComponentNoises:selectionNoise:@
noiseWithComponentNoises_selectionNoiseSelector :: Selector
noiseWithComponentNoises_selectionNoiseSelector = mkSelector "noiseWithComponentNoises:selectionNoise:"

-- | @Selector@ for @noiseWithComponentNoises:selectionNoise:componentBoundaries:boundaryBlendDistances:@
noiseWithComponentNoises_selectionNoise_componentBoundaries_boundaryBlendDistancesSelector :: Selector
noiseWithComponentNoises_selectionNoise_componentBoundaries_boundaryBlendDistancesSelector = mkSelector "noiseWithComponentNoises:selectionNoise:componentBoundaries:boundaryBlendDistances:"

-- | @Selector@ for @applyAbsoluteValue@
applyAbsoluteValueSelector :: Selector
applyAbsoluteValueSelector = mkSelector "applyAbsoluteValue"

-- | @Selector@ for @clampWithLowerBound:upperBound:@
clampWithLowerBound_upperBoundSelector :: Selector
clampWithLowerBound_upperBoundSelector = mkSelector "clampWithLowerBound:upperBound:"

-- | @Selector@ for @raiseToPower:@
raiseToPowerSelector :: Selector
raiseToPowerSelector = mkSelector "raiseToPower:"

-- | @Selector@ for @invert@
invertSelector :: Selector
invertSelector = mkSelector "invert"

-- | @Selector@ for @applyTurbulenceWithFrequency:power:roughness:seed:@
applyTurbulenceWithFrequency_power_roughness_seedSelector :: Selector
applyTurbulenceWithFrequency_power_roughness_seedSelector = mkSelector "applyTurbulenceWithFrequency:power:roughness:seed:"

-- | @Selector@ for @remapValuesToCurveWithControlPoints:@
remapValuesToCurveWithControlPointsSelector :: Selector
remapValuesToCurveWithControlPointsSelector = mkSelector "remapValuesToCurveWithControlPoints:"

-- | @Selector@ for @remapValuesToTerracesWithPeaks:terracesInverted:@
remapValuesToTerracesWithPeaks_terracesInvertedSelector :: Selector
remapValuesToTerracesWithPeaks_terracesInvertedSelector = mkSelector "remapValuesToTerracesWithPeaks:terracesInverted:"

-- | @Selector@ for @addWithNoise:@
addWithNoiseSelector :: Selector
addWithNoiseSelector = mkSelector "addWithNoise:"

-- | @Selector@ for @multiplyWithNoise:@
multiplyWithNoiseSelector :: Selector
multiplyWithNoiseSelector = mkSelector "multiplyWithNoise:"

-- | @Selector@ for @minimumWithNoise:@
minimumWithNoiseSelector :: Selector
minimumWithNoiseSelector = mkSelector "minimumWithNoise:"

-- | @Selector@ for @maximumWithNoise:@
maximumWithNoiseSelector :: Selector
maximumWithNoiseSelector = mkSelector "maximumWithNoise:"

-- | @Selector@ for @raiseToPowerWithNoise:@
raiseToPowerWithNoiseSelector :: Selector
raiseToPowerWithNoiseSelector = mkSelector "raiseToPowerWithNoise:"

-- | @Selector@ for @displaceXWithNoise:yWithNoise:zWithNoise:@
displaceXWithNoise_yWithNoise_zWithNoiseSelector :: Selector
displaceXWithNoise_yWithNoise_zWithNoiseSelector = mkSelector "displaceXWithNoise:yWithNoise:zWithNoise:"

-- | @Selector@ for @gradientColors@
gradientColorsSelector :: Selector
gradientColorsSelector = mkSelector "gradientColors"

-- | @Selector@ for @setGradientColors:@
setGradientColorsSelector :: Selector
setGradientColorsSelector = mkSelector "setGradientColors:"

