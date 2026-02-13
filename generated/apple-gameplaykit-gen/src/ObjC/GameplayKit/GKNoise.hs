{-# LANGUAGE DataKinds #-}
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
  , addWithNoiseSelector
  , applyAbsoluteValueSelector
  , applyTurbulenceWithFrequency_power_roughness_seedSelector
  , clampWithLowerBound_upperBoundSelector
  , displaceXWithNoise_yWithNoise_zWithNoiseSelector
  , initSelector
  , initWithNoiseSourceSelector
  , initWithNoiseSource_gradientColorsSelector
  , invertSelector
  , maximumWithNoiseSelector
  , minimumWithNoiseSelector
  , multiplyWithNoiseSelector
  , noiseWithComponentNoises_selectionNoiseSelector
  , noiseWithComponentNoises_selectionNoise_componentBoundaries_boundaryBlendDistancesSelector
  , noiseWithNoiseSourceSelector
  , noiseWithNoiseSource_gradientColorsSelector
  , raiseToPowerSelector
  , raiseToPowerWithNoiseSelector
  , remapValuesToCurveWithControlPointsSelector
  , remapValuesToTerracesWithPeaks_terracesInvertedSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameplayKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initializes a constant noise of 0.0 at all positions.
--
-- ObjC selector: @- init@
init_ :: IsGKNoise gkNoise => gkNoise -> IO (Id GKNoise)
init_ gkNoise =
  sendOwnedMessage gkNoise initSelector

-- | Initializes a noise with the specified noise source.
--
-- @noiseSource@ — The noise source to use to initially populate the 3D noise space.
--
-- ObjC selector: @+ noiseWithNoiseSource:@
noiseWithNoiseSource :: IsGKNoiseSource noiseSource => noiseSource -> IO (Id GKNoise)
noiseWithNoiseSource noiseSource =
  do
    cls' <- getRequiredClass "GKNoise"
    sendClassMessage cls' noiseWithNoiseSourceSelector (toGKNoiseSource noiseSource)

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
    sendClassMessage cls' noiseWithNoiseSource_gradientColorsSelector (toGKNoiseSource noiseSource) (toNSDictionary gradientColors)

-- | Initializes a noise with the specified noise source.
--
-- @noiseSource@ — The noise source to use to initially populate the 3D noise space.
--
-- ObjC selector: @- initWithNoiseSource:@
initWithNoiseSource :: (IsGKNoise gkNoise, IsGKNoiseSource noiseSource) => gkNoise -> noiseSource -> IO (Id GKNoise)
initWithNoiseSource gkNoise noiseSource =
  sendOwnedMessage gkNoise initWithNoiseSourceSelector (toGKNoiseSource noiseSource)

-- | Initializes a noise with the specified noise source and parameters.
--
-- @noiseSource@ — The noise source to use to initially populate the 3D noise space.
--
-- @gradientColors@ — The color gradient to use for this noise in 'value : color' pairs.
--
-- ObjC selector: @- initWithNoiseSource:gradientColors:@
initWithNoiseSource_gradientColors :: (IsGKNoise gkNoise, IsGKNoiseSource noiseSource, IsNSDictionary gradientColors) => gkNoise -> noiseSource -> gradientColors -> IO (Id GKNoise)
initWithNoiseSource_gradientColors gkNoise noiseSource gradientColors =
  sendOwnedMessage gkNoise initWithNoiseSource_gradientColorsSelector (toGKNoiseSource noiseSource) (toNSDictionary gradientColors)

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
    sendClassMessage cls' noiseWithComponentNoises_selectionNoiseSelector (toNSArray noises) (toGKNoise selectionNoise)

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
    sendClassMessage cls' noiseWithComponentNoises_selectionNoise_componentBoundaries_boundaryBlendDistancesSelector (toNSArray noises) (toGKNoise selectionNoise) (toNSArray componentBoundaries) (toNSArray blendDistances)

-- | Takes the absoltue value of all noise positions.
--
-- ObjC selector: @- applyAbsoluteValue@
applyAbsoluteValue :: IsGKNoise gkNoise => gkNoise -> IO ()
applyAbsoluteValue gkNoise =
  sendMessage gkNoise applyAbsoluteValueSelector

-- | Clamps all noise values to the specified bounds.
--
-- @lowerBound@ — The noise value lower bound.
--
-- @upperBound@ — The noise value upper bound.
--
-- ObjC selector: @- clampWithLowerBound:upperBound:@
clampWithLowerBound_upperBound :: IsGKNoise gkNoise => gkNoise -> CDouble -> CDouble -> IO ()
clampWithLowerBound_upperBound gkNoise lowerBound upperBound =
  sendMessage gkNoise clampWithLowerBound_upperBoundSelector lowerBound upperBound

-- | Raises all noise values to the specified power.
--
-- @power@ — The power to which to raise all noise values.
--
-- ObjC selector: @- raiseToPower:@
raiseToPower :: IsGKNoise gkNoise => gkNoise -> CDouble -> IO ()
raiseToPower gkNoise power =
  sendMessage gkNoise raiseToPowerSelector power

-- | Inverts all noise values, from positive to negative and vice versa.
--
-- ObjC selector: @- invert@
invert :: IsGKNoise gkNoise => gkNoise -> IO ()
invert gkNoise =
  sendMessage gkNoise invertSelector

-- | Applies a turbulent displacement to all noise values.
--
-- ObjC selector: @- applyTurbulenceWithFrequency:power:roughness:seed:@
applyTurbulenceWithFrequency_power_roughness_seed :: IsGKNoise gkNoise => gkNoise -> CDouble -> CDouble -> CInt -> CInt -> IO ()
applyTurbulenceWithFrequency_power_roughness_seed gkNoise frequency power roughness seed =
  sendMessage gkNoise applyTurbulenceWithFrequency_power_roughness_seedSelector frequency power roughness seed

-- | Remaps all noise values to a smooth curve that passes through the specified control points.
--
-- @controlPoints@ — Pairs of 'input : output' values to use as control points for the smooth remapping curve. Duplicate input values are not permitted.
--
-- ObjC selector: @- remapValuesToCurveWithControlPoints:@
remapValuesToCurveWithControlPoints :: (IsGKNoise gkNoise, IsNSDictionary controlPoints) => gkNoise -> controlPoints -> IO ()
remapValuesToCurveWithControlPoints gkNoise controlPoints =
  sendMessage gkNoise remapValuesToCurveWithControlPointsSelector (toNSDictionary controlPoints)

-- | Remaps all noise values to one or more terraces with peaks.  Useful for creating valleys and trenches.
--
-- @peakInputValues@ — Inputs positions of terrace peaks.
--
-- @inverted@ — Governs the curve direction from peak to peak.
--
-- ObjC selector: @- remapValuesToTerracesWithPeaks:terracesInverted:@
remapValuesToTerracesWithPeaks_terracesInverted :: (IsGKNoise gkNoise, IsNSArray peakInputValues) => gkNoise -> peakInputValues -> Bool -> IO ()
remapValuesToTerracesWithPeaks_terracesInverted gkNoise peakInputValues inverted =
  sendMessage gkNoise remapValuesToTerracesWithPeaks_terracesInvertedSelector (toNSArray peakInputValues) inverted

-- | Adds all noise values by the noise values at the same position in specified noise.
--
-- @noise@ — The noise from which to add values to this noise.
--
-- ObjC selector: @- addWithNoise:@
addWithNoise :: (IsGKNoise gkNoise, IsGKNoise noise) => gkNoise -> noise -> IO ()
addWithNoise gkNoise noise =
  sendMessage gkNoise addWithNoiseSelector (toGKNoise noise)

-- | Multiplies all noise values by the noise values at the same position in specified noise.
--
-- @noise@ — The noise from which to multiply values to this noise.
--
-- ObjC selector: @- multiplyWithNoise:@
multiplyWithNoise :: (IsGKNoise gkNoise, IsGKNoise noise) => gkNoise -> noise -> IO ()
multiplyWithNoise gkNoise noise =
  sendMessage gkNoise multiplyWithNoiseSelector (toGKNoise noise)

-- | Takes the minimum value between this noise and the specified noise at each position.
--
-- @noise@ — The noise to compare against this noise at each position in determining which to take the minimum value from.
--
-- ObjC selector: @- minimumWithNoise:@
minimumWithNoise :: (IsGKNoise gkNoise, IsGKNoise noise) => gkNoise -> noise -> IO ()
minimumWithNoise gkNoise noise =
  sendMessage gkNoise minimumWithNoiseSelector (toGKNoise noise)

-- | Takes the maximum value between this noise and the specified noise at each position.
--
-- @noise@ — The noise to compare against this noise at each position in determining which to take the maximum value from.
--
-- ObjC selector: @- maximumWithNoise:@
maximumWithNoise :: (IsGKNoise gkNoise, IsGKNoise noise) => gkNoise -> noise -> IO ()
maximumWithNoise gkNoise noise =
  sendMessage gkNoise maximumWithNoiseSelector (toGKNoise noise)

-- | Raises all noise values to the power of the value at the same position of the specified noise.
--
-- @noise@ — The noise from which to raise this noise's values by.
--
-- ObjC selector: @- raiseToPowerWithNoise:@
raiseToPowerWithNoise :: (IsGKNoise gkNoise, IsGKNoise noise) => gkNoise -> noise -> IO ()
raiseToPowerWithNoise gkNoise noise =
  sendMessage gkNoise raiseToPowerWithNoiseSelector (toGKNoise noise)

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
displaceXWithNoise_yWithNoise_zWithNoise gkNoise xDisplacementNoise yDisplacementNoise zDisplacementNoise =
  sendMessage gkNoise displaceXWithNoise_yWithNoise_zWithNoiseSelector (toGKNoise xDisplacementNoise) (toGKNoise yDisplacementNoise) (toGKNoise zDisplacementNoise)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id GKNoise)
initSelector = mkSelector "init"

-- | @Selector@ for @noiseWithNoiseSource:@
noiseWithNoiseSourceSelector :: Selector '[Id GKNoiseSource] (Id GKNoise)
noiseWithNoiseSourceSelector = mkSelector "noiseWithNoiseSource:"

-- | @Selector@ for @noiseWithNoiseSource:gradientColors:@
noiseWithNoiseSource_gradientColorsSelector :: Selector '[Id GKNoiseSource, Id NSDictionary] (Id GKNoise)
noiseWithNoiseSource_gradientColorsSelector = mkSelector "noiseWithNoiseSource:gradientColors:"

-- | @Selector@ for @initWithNoiseSource:@
initWithNoiseSourceSelector :: Selector '[Id GKNoiseSource] (Id GKNoise)
initWithNoiseSourceSelector = mkSelector "initWithNoiseSource:"

-- | @Selector@ for @initWithNoiseSource:gradientColors:@
initWithNoiseSource_gradientColorsSelector :: Selector '[Id GKNoiseSource, Id NSDictionary] (Id GKNoise)
initWithNoiseSource_gradientColorsSelector = mkSelector "initWithNoiseSource:gradientColors:"

-- | @Selector@ for @noiseWithComponentNoises:selectionNoise:@
noiseWithComponentNoises_selectionNoiseSelector :: Selector '[Id NSArray, Id GKNoise] (Id GKNoise)
noiseWithComponentNoises_selectionNoiseSelector = mkSelector "noiseWithComponentNoises:selectionNoise:"

-- | @Selector@ for @noiseWithComponentNoises:selectionNoise:componentBoundaries:boundaryBlendDistances:@
noiseWithComponentNoises_selectionNoise_componentBoundaries_boundaryBlendDistancesSelector :: Selector '[Id NSArray, Id GKNoise, Id NSArray, Id NSArray] (Id GKNoise)
noiseWithComponentNoises_selectionNoise_componentBoundaries_boundaryBlendDistancesSelector = mkSelector "noiseWithComponentNoises:selectionNoise:componentBoundaries:boundaryBlendDistances:"

-- | @Selector@ for @applyAbsoluteValue@
applyAbsoluteValueSelector :: Selector '[] ()
applyAbsoluteValueSelector = mkSelector "applyAbsoluteValue"

-- | @Selector@ for @clampWithLowerBound:upperBound:@
clampWithLowerBound_upperBoundSelector :: Selector '[CDouble, CDouble] ()
clampWithLowerBound_upperBoundSelector = mkSelector "clampWithLowerBound:upperBound:"

-- | @Selector@ for @raiseToPower:@
raiseToPowerSelector :: Selector '[CDouble] ()
raiseToPowerSelector = mkSelector "raiseToPower:"

-- | @Selector@ for @invert@
invertSelector :: Selector '[] ()
invertSelector = mkSelector "invert"

-- | @Selector@ for @applyTurbulenceWithFrequency:power:roughness:seed:@
applyTurbulenceWithFrequency_power_roughness_seedSelector :: Selector '[CDouble, CDouble, CInt, CInt] ()
applyTurbulenceWithFrequency_power_roughness_seedSelector = mkSelector "applyTurbulenceWithFrequency:power:roughness:seed:"

-- | @Selector@ for @remapValuesToCurveWithControlPoints:@
remapValuesToCurveWithControlPointsSelector :: Selector '[Id NSDictionary] ()
remapValuesToCurveWithControlPointsSelector = mkSelector "remapValuesToCurveWithControlPoints:"

-- | @Selector@ for @remapValuesToTerracesWithPeaks:terracesInverted:@
remapValuesToTerracesWithPeaks_terracesInvertedSelector :: Selector '[Id NSArray, Bool] ()
remapValuesToTerracesWithPeaks_terracesInvertedSelector = mkSelector "remapValuesToTerracesWithPeaks:terracesInverted:"

-- | @Selector@ for @addWithNoise:@
addWithNoiseSelector :: Selector '[Id GKNoise] ()
addWithNoiseSelector = mkSelector "addWithNoise:"

-- | @Selector@ for @multiplyWithNoise:@
multiplyWithNoiseSelector :: Selector '[Id GKNoise] ()
multiplyWithNoiseSelector = mkSelector "multiplyWithNoise:"

-- | @Selector@ for @minimumWithNoise:@
minimumWithNoiseSelector :: Selector '[Id GKNoise] ()
minimumWithNoiseSelector = mkSelector "minimumWithNoise:"

-- | @Selector@ for @maximumWithNoise:@
maximumWithNoiseSelector :: Selector '[Id GKNoise] ()
maximumWithNoiseSelector = mkSelector "maximumWithNoise:"

-- | @Selector@ for @raiseToPowerWithNoise:@
raiseToPowerWithNoiseSelector :: Selector '[Id GKNoise] ()
raiseToPowerWithNoiseSelector = mkSelector "raiseToPowerWithNoise:"

-- | @Selector@ for @displaceXWithNoise:yWithNoise:zWithNoise:@
displaceXWithNoise_yWithNoise_zWithNoiseSelector :: Selector '[Id GKNoise, Id GKNoise, Id GKNoise] ()
displaceXWithNoise_yWithNoise_zWithNoiseSelector = mkSelector "displaceXWithNoise:yWithNoise:zWithNoise:"

