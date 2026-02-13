{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MDLCamera@.
module ObjC.ModelIO.MDLCamera
  ( MDLCamera
  , IsMDLCamera(..)
  , projection
  , setProjection
  , nearVisibilityDistance
  , setNearVisibilityDistance
  , farVisibilityDistance
  , setFarVisibilityDistance
  , worldToMetersConversionScale
  , setWorldToMetersConversionScale
  , barrelDistortion
  , setBarrelDistortion
  , fisheyeDistortion
  , setFisheyeDistortion
  , opticalVignetting
  , setOpticalVignetting
  , chromaticAberration
  , setChromaticAberration
  , focalLength
  , setFocalLength
  , focusDistance
  , setFocusDistance
  , fieldOfView
  , setFieldOfView
  , fStop
  , setFStop
  , apertureBladeCount
  , setApertureBladeCount
  , maximumCircleOfConfusion
  , setMaximumCircleOfConfusion
  , shutterOpenInterval
  , setShutterOpenInterval
  , sensorVerticalAperture
  , setSensorVerticalAperture
  , sensorAspect
  , setSensorAspect
  , apertureBladeCountSelector
  , barrelDistortionSelector
  , chromaticAberrationSelector
  , fStopSelector
  , farVisibilityDistanceSelector
  , fieldOfViewSelector
  , fisheyeDistortionSelector
  , focalLengthSelector
  , focusDistanceSelector
  , maximumCircleOfConfusionSelector
  , nearVisibilityDistanceSelector
  , opticalVignettingSelector
  , projectionSelector
  , sensorAspectSelector
  , sensorVerticalApertureSelector
  , setApertureBladeCountSelector
  , setBarrelDistortionSelector
  , setChromaticAberrationSelector
  , setFStopSelector
  , setFarVisibilityDistanceSelector
  , setFieldOfViewSelector
  , setFisheyeDistortionSelector
  , setFocalLengthSelector
  , setFocusDistanceSelector
  , setMaximumCircleOfConfusionSelector
  , setNearVisibilityDistanceSelector
  , setOpticalVignettingSelector
  , setProjectionSelector
  , setSensorAspectSelector
  , setSensorVerticalApertureSelector
  , setShutterOpenIntervalSelector
  , setWorldToMetersConversionScaleSelector
  , shutterOpenIntervalSelector
  , worldToMetersConversionScaleSelector

  -- * Enum types
  , MDLCameraProjection(MDLCameraProjection)
  , pattern MDLCameraProjectionPerspective
  , pattern MDLCameraProjectionOrthographic

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ModelIO.Internal.Classes
import ObjC.ModelIO.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- projection@
projection :: IsMDLCamera mdlCamera => mdlCamera -> IO MDLCameraProjection
projection mdlCamera =
  sendMessage mdlCamera projectionSelector

-- | @- setProjection:@
setProjection :: IsMDLCamera mdlCamera => mdlCamera -> MDLCameraProjection -> IO ()
setProjection mdlCamera value =
  sendMessage mdlCamera setProjectionSelector value

-- | Bounding distance for visible objects
--
-- ObjC selector: @- nearVisibilityDistance@
nearVisibilityDistance :: IsMDLCamera mdlCamera => mdlCamera -> IO CFloat
nearVisibilityDistance mdlCamera =
  sendMessage mdlCamera nearVisibilityDistanceSelector

-- | Bounding distance for visible objects
--
-- ObjC selector: @- setNearVisibilityDistance:@
setNearVisibilityDistance :: IsMDLCamera mdlCamera => mdlCamera -> CFloat -> IO ()
setNearVisibilityDistance mdlCamera value =
  sendMessage mdlCamera setNearVisibilityDistanceSelector value

-- | @- farVisibilityDistance@
farVisibilityDistance :: IsMDLCamera mdlCamera => mdlCamera -> IO CFloat
farVisibilityDistance mdlCamera =
  sendMessage mdlCamera farVisibilityDistanceSelector

-- | @- setFarVisibilityDistance:@
setFarVisibilityDistance :: IsMDLCamera mdlCamera => mdlCamera -> CFloat -> IO ()
setFarVisibilityDistance mdlCamera value =
  sendMessage mdlCamera setFarVisibilityDistanceSelector value

-- | World to meters conversion scale. Required for certain calculations.
--
-- ObjC selector: @- worldToMetersConversionScale@
worldToMetersConversionScale :: IsMDLCamera mdlCamera => mdlCamera -> IO CFloat
worldToMetersConversionScale mdlCamera =
  sendMessage mdlCamera worldToMetersConversionScaleSelector

-- | World to meters conversion scale. Required for certain calculations.
--
-- ObjC selector: @- setWorldToMetersConversionScale:@
setWorldToMetersConversionScale :: IsMDLCamera mdlCamera => mdlCamera -> CFloat -> IO ()
setWorldToMetersConversionScale mdlCamera value =
  sendMessage mdlCamera setWorldToMetersConversionScaleSelector value

-- | Radial distortion of the lens, second order term
--
-- ObjC selector: @- barrelDistortion@
barrelDistortion :: IsMDLCamera mdlCamera => mdlCamera -> IO CFloat
barrelDistortion mdlCamera =
  sendMessage mdlCamera barrelDistortionSelector

-- | Radial distortion of the lens, second order term
--
-- ObjC selector: @- setBarrelDistortion:@
setBarrelDistortion :: IsMDLCamera mdlCamera => mdlCamera -> CFloat -> IO ()
setBarrelDistortion mdlCamera value =
  sendMessage mdlCamera setBarrelDistortionSelector value

-- | Radial distortion of the lens, fourth order term
--
-- ObjC selector: @- fisheyeDistortion@
fisheyeDistortion :: IsMDLCamera mdlCamera => mdlCamera -> IO CFloat
fisheyeDistortion mdlCamera =
  sendMessage mdlCamera fisheyeDistortionSelector

-- | Radial distortion of the lens, fourth order term
--
-- ObjC selector: @- setFisheyeDistortion:@
setFisheyeDistortion :: IsMDLCamera mdlCamera => mdlCamera -> CFloat -> IO ()
setFisheyeDistortion mdlCamera value =
  sendMessage mdlCamera setFisheyeDistortionSelector value

-- | Amount of optical vignetting, rom zero to one.
--
-- ObjC selector: @- opticalVignetting@
opticalVignetting :: IsMDLCamera mdlCamera => mdlCamera -> IO CFloat
opticalVignetting mdlCamera =
  sendMessage mdlCamera opticalVignettingSelector

-- | Amount of optical vignetting, rom zero to one.
--
-- ObjC selector: @- setOpticalVignetting:@
setOpticalVignetting :: IsMDLCamera mdlCamera => mdlCamera -> CFloat -> IO ()
setOpticalVignetting mdlCamera value =
  sendMessage mdlCamera setOpticalVignettingSelector value

-- | Amount of chromatic abberation, from zero to one.
--
-- ObjC selector: @- chromaticAberration@
chromaticAberration :: IsMDLCamera mdlCamera => mdlCamera -> IO CFloat
chromaticAberration mdlCamera =
  sendMessage mdlCamera chromaticAberrationSelector

-- | Amount of chromatic abberation, from zero to one.
--
-- ObjC selector: @- setChromaticAberration:@
setChromaticAberration :: IsMDLCamera mdlCamera => mdlCamera -> CFloat -> IO ()
setChromaticAberration mdlCamera value =
  sendMessage mdlCamera setChromaticAberrationSelector value

-- | Lens focal length in mm.
--
-- See: fieldOfView
--
-- ObjC selector: @- focalLength@
focalLength :: IsMDLCamera mdlCamera => mdlCamera -> IO CFloat
focalLength mdlCamera =
  sendMessage mdlCamera focalLengthSelector

-- | Lens focal length in mm.
--
-- See: fieldOfView
--
-- ObjC selector: @- setFocalLength:@
setFocalLength :: IsMDLCamera mdlCamera => mdlCamera -> CFloat -> IO ()
setFocalLength mdlCamera value =
  sendMessage mdlCamera setFocalLengthSelector value

-- | Focus distance
--
-- ObjC selector: @- focusDistance@
focusDistance :: IsMDLCamera mdlCamera => mdlCamera -> IO CFloat
focusDistance mdlCamera =
  sendMessage mdlCamera focusDistanceSelector

-- | Focus distance
--
-- ObjC selector: @- setFocusDistance:@
setFocusDistance :: IsMDLCamera mdlCamera => mdlCamera -> CFloat -> IO ()
setFocusDistance mdlCamera value =
  sendMessage mdlCamera setFocusDistanceSelector value

-- | The field of view, in degrees.
--
-- See: focalLength
--
-- ObjC selector: @- fieldOfView@
fieldOfView :: IsMDLCamera mdlCamera => mdlCamera -> IO CFloat
fieldOfView mdlCamera =
  sendMessage mdlCamera fieldOfViewSelector

-- | The field of view, in degrees.
--
-- See: focalLength
--
-- ObjC selector: @- setFieldOfView:@
setFieldOfView :: IsMDLCamera mdlCamera => mdlCamera -> CFloat -> IO ()
setFieldOfView mdlCamera value =
  sendMessage mdlCamera setFieldOfViewSelector value

-- | f-stop, default is 5.6
--
-- ObjC selector: @- fStop@
fStop :: IsMDLCamera mdlCamera => mdlCamera -> IO CFloat
fStop mdlCamera =
  sendMessage mdlCamera fStopSelector

-- | f-stop, default is 5.6
--
-- ObjC selector: @- setFStop:@
setFStop :: IsMDLCamera mdlCamera => mdlCamera -> CFloat -> IO ()
setFStop mdlCamera value =
  sendMessage mdlCamera setFStopSelector value

-- | Aperture blade count, where zero indicates a circle.
--
-- ObjC selector: @- apertureBladeCount@
apertureBladeCount :: IsMDLCamera mdlCamera => mdlCamera -> IO CULong
apertureBladeCount mdlCamera =
  sendMessage mdlCamera apertureBladeCountSelector

-- | Aperture blade count, where zero indicates a circle.
--
-- ObjC selector: @- setApertureBladeCount:@
setApertureBladeCount :: IsMDLCamera mdlCamera => mdlCamera -> CULong -> IO ()
setApertureBladeCount mdlCamera value =
  sendMessage mdlCamera setApertureBladeCountSelector value

-- | Maximum circle of confusion size in mm on the image plane
--
-- ObjC selector: @- maximumCircleOfConfusion@
maximumCircleOfConfusion :: IsMDLCamera mdlCamera => mdlCamera -> IO CFloat
maximumCircleOfConfusion mdlCamera =
  sendMessage mdlCamera maximumCircleOfConfusionSelector

-- | Maximum circle of confusion size in mm on the image plane
--
-- ObjC selector: @- setMaximumCircleOfConfusion:@
setMaximumCircleOfConfusion :: IsMDLCamera mdlCamera => mdlCamera -> CFloat -> IO ()
setMaximumCircleOfConfusion mdlCamera value =
  sendMessage mdlCamera setMaximumCircleOfConfusionSelector value

-- | Shutter open interval, in seconds
--
-- ObjC selector: @- shutterOpenInterval@
shutterOpenInterval :: IsMDLCamera mdlCamera => mdlCamera -> IO CDouble
shutterOpenInterval mdlCamera =
  sendMessage mdlCamera shutterOpenIntervalSelector

-- | Shutter open interval, in seconds
--
-- ObjC selector: @- setShutterOpenInterval:@
setShutterOpenInterval :: IsMDLCamera mdlCamera => mdlCamera -> CDouble -> IO ()
setShutterOpenInterval mdlCamera value =
  sendMessage mdlCamera setShutterOpenIntervalSelector value

-- | vertical aperture of the sensor or film gate, default is 24mm
--
-- See: sensorAspect
--
-- ObjC selector: @- sensorVerticalAperture@
sensorVerticalAperture :: IsMDLCamera mdlCamera => mdlCamera -> IO CFloat
sensorVerticalAperture mdlCamera =
  sendMessage mdlCamera sensorVerticalApertureSelector

-- | vertical aperture of the sensor or film gate, default is 24mm
--
-- See: sensorAspect
--
-- ObjC selector: @- setSensorVerticalAperture:@
setSensorVerticalAperture :: IsMDLCamera mdlCamera => mdlCamera -> CFloat -> IO ()
setSensorVerticalAperture mdlCamera value =
  sendMessage mdlCamera setSensorVerticalApertureSelector value

-- | Sensor or film gate aperture aspect ratio, default is 1.5
--
-- See: sensorVerticalAperture
--
-- ObjC selector: @- sensorAspect@
sensorAspect :: IsMDLCamera mdlCamera => mdlCamera -> IO CFloat
sensorAspect mdlCamera =
  sendMessage mdlCamera sensorAspectSelector

-- | Sensor or film gate aperture aspect ratio, default is 1.5
--
-- See: sensorVerticalAperture
--
-- ObjC selector: @- setSensorAspect:@
setSensorAspect :: IsMDLCamera mdlCamera => mdlCamera -> CFloat -> IO ()
setSensorAspect mdlCamera value =
  sendMessage mdlCamera setSensorAspectSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @projection@
projectionSelector :: Selector '[] MDLCameraProjection
projectionSelector = mkSelector "projection"

-- | @Selector@ for @setProjection:@
setProjectionSelector :: Selector '[MDLCameraProjection] ()
setProjectionSelector = mkSelector "setProjection:"

-- | @Selector@ for @nearVisibilityDistance@
nearVisibilityDistanceSelector :: Selector '[] CFloat
nearVisibilityDistanceSelector = mkSelector "nearVisibilityDistance"

-- | @Selector@ for @setNearVisibilityDistance:@
setNearVisibilityDistanceSelector :: Selector '[CFloat] ()
setNearVisibilityDistanceSelector = mkSelector "setNearVisibilityDistance:"

-- | @Selector@ for @farVisibilityDistance@
farVisibilityDistanceSelector :: Selector '[] CFloat
farVisibilityDistanceSelector = mkSelector "farVisibilityDistance"

-- | @Selector@ for @setFarVisibilityDistance:@
setFarVisibilityDistanceSelector :: Selector '[CFloat] ()
setFarVisibilityDistanceSelector = mkSelector "setFarVisibilityDistance:"

-- | @Selector@ for @worldToMetersConversionScale@
worldToMetersConversionScaleSelector :: Selector '[] CFloat
worldToMetersConversionScaleSelector = mkSelector "worldToMetersConversionScale"

-- | @Selector@ for @setWorldToMetersConversionScale:@
setWorldToMetersConversionScaleSelector :: Selector '[CFloat] ()
setWorldToMetersConversionScaleSelector = mkSelector "setWorldToMetersConversionScale:"

-- | @Selector@ for @barrelDistortion@
barrelDistortionSelector :: Selector '[] CFloat
barrelDistortionSelector = mkSelector "barrelDistortion"

-- | @Selector@ for @setBarrelDistortion:@
setBarrelDistortionSelector :: Selector '[CFloat] ()
setBarrelDistortionSelector = mkSelector "setBarrelDistortion:"

-- | @Selector@ for @fisheyeDistortion@
fisheyeDistortionSelector :: Selector '[] CFloat
fisheyeDistortionSelector = mkSelector "fisheyeDistortion"

-- | @Selector@ for @setFisheyeDistortion:@
setFisheyeDistortionSelector :: Selector '[CFloat] ()
setFisheyeDistortionSelector = mkSelector "setFisheyeDistortion:"

-- | @Selector@ for @opticalVignetting@
opticalVignettingSelector :: Selector '[] CFloat
opticalVignettingSelector = mkSelector "opticalVignetting"

-- | @Selector@ for @setOpticalVignetting:@
setOpticalVignettingSelector :: Selector '[CFloat] ()
setOpticalVignettingSelector = mkSelector "setOpticalVignetting:"

-- | @Selector@ for @chromaticAberration@
chromaticAberrationSelector :: Selector '[] CFloat
chromaticAberrationSelector = mkSelector "chromaticAberration"

-- | @Selector@ for @setChromaticAberration:@
setChromaticAberrationSelector :: Selector '[CFloat] ()
setChromaticAberrationSelector = mkSelector "setChromaticAberration:"

-- | @Selector@ for @focalLength@
focalLengthSelector :: Selector '[] CFloat
focalLengthSelector = mkSelector "focalLength"

-- | @Selector@ for @setFocalLength:@
setFocalLengthSelector :: Selector '[CFloat] ()
setFocalLengthSelector = mkSelector "setFocalLength:"

-- | @Selector@ for @focusDistance@
focusDistanceSelector :: Selector '[] CFloat
focusDistanceSelector = mkSelector "focusDistance"

-- | @Selector@ for @setFocusDistance:@
setFocusDistanceSelector :: Selector '[CFloat] ()
setFocusDistanceSelector = mkSelector "setFocusDistance:"

-- | @Selector@ for @fieldOfView@
fieldOfViewSelector :: Selector '[] CFloat
fieldOfViewSelector = mkSelector "fieldOfView"

-- | @Selector@ for @setFieldOfView:@
setFieldOfViewSelector :: Selector '[CFloat] ()
setFieldOfViewSelector = mkSelector "setFieldOfView:"

-- | @Selector@ for @fStop@
fStopSelector :: Selector '[] CFloat
fStopSelector = mkSelector "fStop"

-- | @Selector@ for @setFStop:@
setFStopSelector :: Selector '[CFloat] ()
setFStopSelector = mkSelector "setFStop:"

-- | @Selector@ for @apertureBladeCount@
apertureBladeCountSelector :: Selector '[] CULong
apertureBladeCountSelector = mkSelector "apertureBladeCount"

-- | @Selector@ for @setApertureBladeCount:@
setApertureBladeCountSelector :: Selector '[CULong] ()
setApertureBladeCountSelector = mkSelector "setApertureBladeCount:"

-- | @Selector@ for @maximumCircleOfConfusion@
maximumCircleOfConfusionSelector :: Selector '[] CFloat
maximumCircleOfConfusionSelector = mkSelector "maximumCircleOfConfusion"

-- | @Selector@ for @setMaximumCircleOfConfusion:@
setMaximumCircleOfConfusionSelector :: Selector '[CFloat] ()
setMaximumCircleOfConfusionSelector = mkSelector "setMaximumCircleOfConfusion:"

-- | @Selector@ for @shutterOpenInterval@
shutterOpenIntervalSelector :: Selector '[] CDouble
shutterOpenIntervalSelector = mkSelector "shutterOpenInterval"

-- | @Selector@ for @setShutterOpenInterval:@
setShutterOpenIntervalSelector :: Selector '[CDouble] ()
setShutterOpenIntervalSelector = mkSelector "setShutterOpenInterval:"

-- | @Selector@ for @sensorVerticalAperture@
sensorVerticalApertureSelector :: Selector '[] CFloat
sensorVerticalApertureSelector = mkSelector "sensorVerticalAperture"

-- | @Selector@ for @setSensorVerticalAperture:@
setSensorVerticalApertureSelector :: Selector '[CFloat] ()
setSensorVerticalApertureSelector = mkSelector "setSensorVerticalAperture:"

-- | @Selector@ for @sensorAspect@
sensorAspectSelector :: Selector '[] CFloat
sensorAspectSelector = mkSelector "sensorAspect"

-- | @Selector@ for @setSensorAspect:@
setSensorAspectSelector :: Selector '[CFloat] ()
setSensorAspectSelector = mkSelector "setSensorAspect:"

