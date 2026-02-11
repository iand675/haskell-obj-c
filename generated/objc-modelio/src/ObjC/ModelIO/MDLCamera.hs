{-# LANGUAGE PatternSynonyms #-}
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
  , projectionSelector
  , setProjectionSelector
  , nearVisibilityDistanceSelector
  , setNearVisibilityDistanceSelector
  , farVisibilityDistanceSelector
  , setFarVisibilityDistanceSelector
  , worldToMetersConversionScaleSelector
  , setWorldToMetersConversionScaleSelector
  , barrelDistortionSelector
  , setBarrelDistortionSelector
  , fisheyeDistortionSelector
  , setFisheyeDistortionSelector
  , opticalVignettingSelector
  , setOpticalVignettingSelector
  , chromaticAberrationSelector
  , setChromaticAberrationSelector
  , focalLengthSelector
  , setFocalLengthSelector
  , focusDistanceSelector
  , setFocusDistanceSelector
  , fieldOfViewSelector
  , setFieldOfViewSelector
  , fStopSelector
  , setFStopSelector
  , apertureBladeCountSelector
  , setApertureBladeCountSelector
  , maximumCircleOfConfusionSelector
  , setMaximumCircleOfConfusionSelector
  , shutterOpenIntervalSelector
  , setShutterOpenIntervalSelector
  , sensorVerticalApertureSelector
  , setSensorVerticalApertureSelector
  , sensorAspectSelector
  , setSensorAspectSelector

  -- * Enum types
  , MDLCameraProjection(MDLCameraProjection)
  , pattern MDLCameraProjectionPerspective
  , pattern MDLCameraProjectionOrthographic

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

import ObjC.ModelIO.Internal.Classes
import ObjC.ModelIO.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- projection@
projection :: IsMDLCamera mdlCamera => mdlCamera -> IO MDLCameraProjection
projection mdlCamera  =
  fmap (coerce :: CULong -> MDLCameraProjection) $ sendMsg mdlCamera (mkSelector "projection") retCULong []

-- | @- setProjection:@
setProjection :: IsMDLCamera mdlCamera => mdlCamera -> MDLCameraProjection -> IO ()
setProjection mdlCamera  value =
  sendMsg mdlCamera (mkSelector "setProjection:") retVoid [argCULong (coerce value)]

-- | Bounding distance for visible objects
--
-- ObjC selector: @- nearVisibilityDistance@
nearVisibilityDistance :: IsMDLCamera mdlCamera => mdlCamera -> IO CFloat
nearVisibilityDistance mdlCamera  =
  sendMsg mdlCamera (mkSelector "nearVisibilityDistance") retCFloat []

-- | Bounding distance for visible objects
--
-- ObjC selector: @- setNearVisibilityDistance:@
setNearVisibilityDistance :: IsMDLCamera mdlCamera => mdlCamera -> CFloat -> IO ()
setNearVisibilityDistance mdlCamera  value =
  sendMsg mdlCamera (mkSelector "setNearVisibilityDistance:") retVoid [argCFloat (fromIntegral value)]

-- | @- farVisibilityDistance@
farVisibilityDistance :: IsMDLCamera mdlCamera => mdlCamera -> IO CFloat
farVisibilityDistance mdlCamera  =
  sendMsg mdlCamera (mkSelector "farVisibilityDistance") retCFloat []

-- | @- setFarVisibilityDistance:@
setFarVisibilityDistance :: IsMDLCamera mdlCamera => mdlCamera -> CFloat -> IO ()
setFarVisibilityDistance mdlCamera  value =
  sendMsg mdlCamera (mkSelector "setFarVisibilityDistance:") retVoid [argCFloat (fromIntegral value)]

-- | World to meters conversion scale. Required for certain calculations.
--
-- ObjC selector: @- worldToMetersConversionScale@
worldToMetersConversionScale :: IsMDLCamera mdlCamera => mdlCamera -> IO CFloat
worldToMetersConversionScale mdlCamera  =
  sendMsg mdlCamera (mkSelector "worldToMetersConversionScale") retCFloat []

-- | World to meters conversion scale. Required for certain calculations.
--
-- ObjC selector: @- setWorldToMetersConversionScale:@
setWorldToMetersConversionScale :: IsMDLCamera mdlCamera => mdlCamera -> CFloat -> IO ()
setWorldToMetersConversionScale mdlCamera  value =
  sendMsg mdlCamera (mkSelector "setWorldToMetersConversionScale:") retVoid [argCFloat (fromIntegral value)]

-- | Radial distortion of the lens, second order term
--
-- ObjC selector: @- barrelDistortion@
barrelDistortion :: IsMDLCamera mdlCamera => mdlCamera -> IO CFloat
barrelDistortion mdlCamera  =
  sendMsg mdlCamera (mkSelector "barrelDistortion") retCFloat []

-- | Radial distortion of the lens, second order term
--
-- ObjC selector: @- setBarrelDistortion:@
setBarrelDistortion :: IsMDLCamera mdlCamera => mdlCamera -> CFloat -> IO ()
setBarrelDistortion mdlCamera  value =
  sendMsg mdlCamera (mkSelector "setBarrelDistortion:") retVoid [argCFloat (fromIntegral value)]

-- | Radial distortion of the lens, fourth order term
--
-- ObjC selector: @- fisheyeDistortion@
fisheyeDistortion :: IsMDLCamera mdlCamera => mdlCamera -> IO CFloat
fisheyeDistortion mdlCamera  =
  sendMsg mdlCamera (mkSelector "fisheyeDistortion") retCFloat []

-- | Radial distortion of the lens, fourth order term
--
-- ObjC selector: @- setFisheyeDistortion:@
setFisheyeDistortion :: IsMDLCamera mdlCamera => mdlCamera -> CFloat -> IO ()
setFisheyeDistortion mdlCamera  value =
  sendMsg mdlCamera (mkSelector "setFisheyeDistortion:") retVoid [argCFloat (fromIntegral value)]

-- | Amount of optical vignetting, rom zero to one.
--
-- ObjC selector: @- opticalVignetting@
opticalVignetting :: IsMDLCamera mdlCamera => mdlCamera -> IO CFloat
opticalVignetting mdlCamera  =
  sendMsg mdlCamera (mkSelector "opticalVignetting") retCFloat []

-- | Amount of optical vignetting, rom zero to one.
--
-- ObjC selector: @- setOpticalVignetting:@
setOpticalVignetting :: IsMDLCamera mdlCamera => mdlCamera -> CFloat -> IO ()
setOpticalVignetting mdlCamera  value =
  sendMsg mdlCamera (mkSelector "setOpticalVignetting:") retVoid [argCFloat (fromIntegral value)]

-- | Amount of chromatic abberation, from zero to one.
--
-- ObjC selector: @- chromaticAberration@
chromaticAberration :: IsMDLCamera mdlCamera => mdlCamera -> IO CFloat
chromaticAberration mdlCamera  =
  sendMsg mdlCamera (mkSelector "chromaticAberration") retCFloat []

-- | Amount of chromatic abberation, from zero to one.
--
-- ObjC selector: @- setChromaticAberration:@
setChromaticAberration :: IsMDLCamera mdlCamera => mdlCamera -> CFloat -> IO ()
setChromaticAberration mdlCamera  value =
  sendMsg mdlCamera (mkSelector "setChromaticAberration:") retVoid [argCFloat (fromIntegral value)]

-- | Lens focal length in mm.
--
-- See: fieldOfView
--
-- ObjC selector: @- focalLength@
focalLength :: IsMDLCamera mdlCamera => mdlCamera -> IO CFloat
focalLength mdlCamera  =
  sendMsg mdlCamera (mkSelector "focalLength") retCFloat []

-- | Lens focal length in mm.
--
-- See: fieldOfView
--
-- ObjC selector: @- setFocalLength:@
setFocalLength :: IsMDLCamera mdlCamera => mdlCamera -> CFloat -> IO ()
setFocalLength mdlCamera  value =
  sendMsg mdlCamera (mkSelector "setFocalLength:") retVoid [argCFloat (fromIntegral value)]

-- | Focus distance
--
-- ObjC selector: @- focusDistance@
focusDistance :: IsMDLCamera mdlCamera => mdlCamera -> IO CFloat
focusDistance mdlCamera  =
  sendMsg mdlCamera (mkSelector "focusDistance") retCFloat []

-- | Focus distance
--
-- ObjC selector: @- setFocusDistance:@
setFocusDistance :: IsMDLCamera mdlCamera => mdlCamera -> CFloat -> IO ()
setFocusDistance mdlCamera  value =
  sendMsg mdlCamera (mkSelector "setFocusDistance:") retVoid [argCFloat (fromIntegral value)]

-- | The field of view, in degrees.
--
-- See: focalLength
--
-- ObjC selector: @- fieldOfView@
fieldOfView :: IsMDLCamera mdlCamera => mdlCamera -> IO CFloat
fieldOfView mdlCamera  =
  sendMsg mdlCamera (mkSelector "fieldOfView") retCFloat []

-- | The field of view, in degrees.
--
-- See: focalLength
--
-- ObjC selector: @- setFieldOfView:@
setFieldOfView :: IsMDLCamera mdlCamera => mdlCamera -> CFloat -> IO ()
setFieldOfView mdlCamera  value =
  sendMsg mdlCamera (mkSelector "setFieldOfView:") retVoid [argCFloat (fromIntegral value)]

-- | f-stop, default is 5.6
--
-- ObjC selector: @- fStop@
fStop :: IsMDLCamera mdlCamera => mdlCamera -> IO CFloat
fStop mdlCamera  =
  sendMsg mdlCamera (mkSelector "fStop") retCFloat []

-- | f-stop, default is 5.6
--
-- ObjC selector: @- setFStop:@
setFStop :: IsMDLCamera mdlCamera => mdlCamera -> CFloat -> IO ()
setFStop mdlCamera  value =
  sendMsg mdlCamera (mkSelector "setFStop:") retVoid [argCFloat (fromIntegral value)]

-- | Aperture blade count, where zero indicates a circle.
--
-- ObjC selector: @- apertureBladeCount@
apertureBladeCount :: IsMDLCamera mdlCamera => mdlCamera -> IO CULong
apertureBladeCount mdlCamera  =
  sendMsg mdlCamera (mkSelector "apertureBladeCount") retCULong []

-- | Aperture blade count, where zero indicates a circle.
--
-- ObjC selector: @- setApertureBladeCount:@
setApertureBladeCount :: IsMDLCamera mdlCamera => mdlCamera -> CULong -> IO ()
setApertureBladeCount mdlCamera  value =
  sendMsg mdlCamera (mkSelector "setApertureBladeCount:") retVoid [argCULong (fromIntegral value)]

-- | Maximum circle of confusion size in mm on the image plane
--
-- ObjC selector: @- maximumCircleOfConfusion@
maximumCircleOfConfusion :: IsMDLCamera mdlCamera => mdlCamera -> IO CFloat
maximumCircleOfConfusion mdlCamera  =
  sendMsg mdlCamera (mkSelector "maximumCircleOfConfusion") retCFloat []

-- | Maximum circle of confusion size in mm on the image plane
--
-- ObjC selector: @- setMaximumCircleOfConfusion:@
setMaximumCircleOfConfusion :: IsMDLCamera mdlCamera => mdlCamera -> CFloat -> IO ()
setMaximumCircleOfConfusion mdlCamera  value =
  sendMsg mdlCamera (mkSelector "setMaximumCircleOfConfusion:") retVoid [argCFloat (fromIntegral value)]

-- | Shutter open interval, in seconds
--
-- ObjC selector: @- shutterOpenInterval@
shutterOpenInterval :: IsMDLCamera mdlCamera => mdlCamera -> IO CDouble
shutterOpenInterval mdlCamera  =
  sendMsg mdlCamera (mkSelector "shutterOpenInterval") retCDouble []

-- | Shutter open interval, in seconds
--
-- ObjC selector: @- setShutterOpenInterval:@
setShutterOpenInterval :: IsMDLCamera mdlCamera => mdlCamera -> CDouble -> IO ()
setShutterOpenInterval mdlCamera  value =
  sendMsg mdlCamera (mkSelector "setShutterOpenInterval:") retVoid [argCDouble (fromIntegral value)]

-- | vertical aperture of the sensor or film gate, default is 24mm
--
-- See: sensorAspect
--
-- ObjC selector: @- sensorVerticalAperture@
sensorVerticalAperture :: IsMDLCamera mdlCamera => mdlCamera -> IO CFloat
sensorVerticalAperture mdlCamera  =
  sendMsg mdlCamera (mkSelector "sensorVerticalAperture") retCFloat []

-- | vertical aperture of the sensor or film gate, default is 24mm
--
-- See: sensorAspect
--
-- ObjC selector: @- setSensorVerticalAperture:@
setSensorVerticalAperture :: IsMDLCamera mdlCamera => mdlCamera -> CFloat -> IO ()
setSensorVerticalAperture mdlCamera  value =
  sendMsg mdlCamera (mkSelector "setSensorVerticalAperture:") retVoid [argCFloat (fromIntegral value)]

-- | Sensor or film gate aperture aspect ratio, default is 1.5
--
-- See: sensorVerticalAperture
--
-- ObjC selector: @- sensorAspect@
sensorAspect :: IsMDLCamera mdlCamera => mdlCamera -> IO CFloat
sensorAspect mdlCamera  =
  sendMsg mdlCamera (mkSelector "sensorAspect") retCFloat []

-- | Sensor or film gate aperture aspect ratio, default is 1.5
--
-- See: sensorVerticalAperture
--
-- ObjC selector: @- setSensorAspect:@
setSensorAspect :: IsMDLCamera mdlCamera => mdlCamera -> CFloat -> IO ()
setSensorAspect mdlCamera  value =
  sendMsg mdlCamera (mkSelector "setSensorAspect:") retVoid [argCFloat (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @projection@
projectionSelector :: Selector
projectionSelector = mkSelector "projection"

-- | @Selector@ for @setProjection:@
setProjectionSelector :: Selector
setProjectionSelector = mkSelector "setProjection:"

-- | @Selector@ for @nearVisibilityDistance@
nearVisibilityDistanceSelector :: Selector
nearVisibilityDistanceSelector = mkSelector "nearVisibilityDistance"

-- | @Selector@ for @setNearVisibilityDistance:@
setNearVisibilityDistanceSelector :: Selector
setNearVisibilityDistanceSelector = mkSelector "setNearVisibilityDistance:"

-- | @Selector@ for @farVisibilityDistance@
farVisibilityDistanceSelector :: Selector
farVisibilityDistanceSelector = mkSelector "farVisibilityDistance"

-- | @Selector@ for @setFarVisibilityDistance:@
setFarVisibilityDistanceSelector :: Selector
setFarVisibilityDistanceSelector = mkSelector "setFarVisibilityDistance:"

-- | @Selector@ for @worldToMetersConversionScale@
worldToMetersConversionScaleSelector :: Selector
worldToMetersConversionScaleSelector = mkSelector "worldToMetersConversionScale"

-- | @Selector@ for @setWorldToMetersConversionScale:@
setWorldToMetersConversionScaleSelector :: Selector
setWorldToMetersConversionScaleSelector = mkSelector "setWorldToMetersConversionScale:"

-- | @Selector@ for @barrelDistortion@
barrelDistortionSelector :: Selector
barrelDistortionSelector = mkSelector "barrelDistortion"

-- | @Selector@ for @setBarrelDistortion:@
setBarrelDistortionSelector :: Selector
setBarrelDistortionSelector = mkSelector "setBarrelDistortion:"

-- | @Selector@ for @fisheyeDistortion@
fisheyeDistortionSelector :: Selector
fisheyeDistortionSelector = mkSelector "fisheyeDistortion"

-- | @Selector@ for @setFisheyeDistortion:@
setFisheyeDistortionSelector :: Selector
setFisheyeDistortionSelector = mkSelector "setFisheyeDistortion:"

-- | @Selector@ for @opticalVignetting@
opticalVignettingSelector :: Selector
opticalVignettingSelector = mkSelector "opticalVignetting"

-- | @Selector@ for @setOpticalVignetting:@
setOpticalVignettingSelector :: Selector
setOpticalVignettingSelector = mkSelector "setOpticalVignetting:"

-- | @Selector@ for @chromaticAberration@
chromaticAberrationSelector :: Selector
chromaticAberrationSelector = mkSelector "chromaticAberration"

-- | @Selector@ for @setChromaticAberration:@
setChromaticAberrationSelector :: Selector
setChromaticAberrationSelector = mkSelector "setChromaticAberration:"

-- | @Selector@ for @focalLength@
focalLengthSelector :: Selector
focalLengthSelector = mkSelector "focalLength"

-- | @Selector@ for @setFocalLength:@
setFocalLengthSelector :: Selector
setFocalLengthSelector = mkSelector "setFocalLength:"

-- | @Selector@ for @focusDistance@
focusDistanceSelector :: Selector
focusDistanceSelector = mkSelector "focusDistance"

-- | @Selector@ for @setFocusDistance:@
setFocusDistanceSelector :: Selector
setFocusDistanceSelector = mkSelector "setFocusDistance:"

-- | @Selector@ for @fieldOfView@
fieldOfViewSelector :: Selector
fieldOfViewSelector = mkSelector "fieldOfView"

-- | @Selector@ for @setFieldOfView:@
setFieldOfViewSelector :: Selector
setFieldOfViewSelector = mkSelector "setFieldOfView:"

-- | @Selector@ for @fStop@
fStopSelector :: Selector
fStopSelector = mkSelector "fStop"

-- | @Selector@ for @setFStop:@
setFStopSelector :: Selector
setFStopSelector = mkSelector "setFStop:"

-- | @Selector@ for @apertureBladeCount@
apertureBladeCountSelector :: Selector
apertureBladeCountSelector = mkSelector "apertureBladeCount"

-- | @Selector@ for @setApertureBladeCount:@
setApertureBladeCountSelector :: Selector
setApertureBladeCountSelector = mkSelector "setApertureBladeCount:"

-- | @Selector@ for @maximumCircleOfConfusion@
maximumCircleOfConfusionSelector :: Selector
maximumCircleOfConfusionSelector = mkSelector "maximumCircleOfConfusion"

-- | @Selector@ for @setMaximumCircleOfConfusion:@
setMaximumCircleOfConfusionSelector :: Selector
setMaximumCircleOfConfusionSelector = mkSelector "setMaximumCircleOfConfusion:"

-- | @Selector@ for @shutterOpenInterval@
shutterOpenIntervalSelector :: Selector
shutterOpenIntervalSelector = mkSelector "shutterOpenInterval"

-- | @Selector@ for @setShutterOpenInterval:@
setShutterOpenIntervalSelector :: Selector
setShutterOpenIntervalSelector = mkSelector "setShutterOpenInterval:"

-- | @Selector@ for @sensorVerticalAperture@
sensorVerticalApertureSelector :: Selector
sensorVerticalApertureSelector = mkSelector "sensorVerticalAperture"

-- | @Selector@ for @setSensorVerticalAperture:@
setSensorVerticalApertureSelector :: Selector
setSensorVerticalApertureSelector = mkSelector "setSensorVerticalAperture:"

-- | @Selector@ for @sensorAspect@
sensorAspectSelector :: Selector
sensorAspectSelector = mkSelector "sensorAspect"

-- | @Selector@ for @setSensorAspect:@
setSensorAspectSelector :: Selector
setSensorAspectSelector = mkSelector "setSensorAspect:"

