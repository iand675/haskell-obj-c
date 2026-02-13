{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MDLSkyCubeTexture  A physically realistic sky as a cube texture
--
-- sunElevation A value of zero is at the zenith, 0.5 is at the horizon,
--
-- 1.0 is at the nadir. Use in conjunction with turbidity to give a dawn,            dusk, or noon look.
--
-- turbidity A value of zero simulates the effect of a clear sky, the sun
--
-- will impart very little color to the sky. A value of one simulates a           great deal of dust and moisture in the sky, and will cause the sun's           color to spread across the atmosphere.
--
-- upperAtmosphereScattering A value of zero will give very dusky colors,
--
-- a value of one will give noon-ish saturated colors.
--
-- groundAlbedo controls the amount of light that bounces back up into
--
-- the sky from the ground. A value of zero will yield a clear sky, a           value of one will reduce the contrast of the sky, making it a bit foggy.
--
-- horizonElevation If the lower half of the environment is being replaced
--
-- by a color, horizonElevation is angle, in radians, below which the           replacement should occur. Negative values are below the horizon.
--
-- groundColor If this value is set, the environment will be replaced with
--
-- the color below the horizonElevation value blended with the w factor up to           Pi/2.0 past the horizon.           (e.g. w = 0.0 groundColor is applied immediatly on the horizon with no blend                 w = Pi/2 groundColor is linearly applied all the way to the south pole)           NOTE: To maintain default behavior a simple length(groundColor) != 0 is used to determine                 if we want to set the ground color (e.g. black and blended immediatly                 on the horizon use (0.0, 0.0, 0.0, 0.0000001))           4 component treats the first 3 components as color and w as blend factor           3 component treats the first 3 components as color and 0 as blend factor           2 component treats the first component as greyscale color and y as blend factor           1 component treats the scalar component as greyscale color and 0 as blend factor
--
-- gamma Modifies the amount of gamma correction applied during
--
-- tone mapping.
--
-- exposure Modifies the exposure applied during tone mapping.
--
-- brighness Modifies the brightness of the image during tone mapping.
--
-- contrast Modifies the contrast of the image during tone mapping.
--
-- saturation Modifes the saturation of the image during tone mapping.
--
-- highDynamicRangeCompression values below the x component of this value
--
-- are not compressed during tone mapping. Values between the x component           and y component are compressed to the maximum brightness value during           tone mapping. Values above the limit are clamped.
--
-- the texture will be created if data is referenced, otherwise, this object is merely a description. All parameters have legal values between zero and one.
--
-- Generated bindings for @MDLSkyCubeTexture@.
module ObjC.ModelIO.MDLSkyCubeTexture
  ( MDLSkyCubeTexture
  , IsMDLSkyCubeTexture(..)
  , updateTexture
  , turbidity
  , setTurbidity
  , sunElevation
  , setSunElevation
  , sunAzimuth
  , setSunAzimuth
  , upperAtmosphereScattering
  , setUpperAtmosphereScattering
  , groundAlbedo
  , setGroundAlbedo
  , horizonElevation
  , setHorizonElevation
  , groundColor
  , setGroundColor
  , gamma
  , setGamma
  , exposure
  , setExposure
  , brightness
  , setBrightness
  , contrast
  , setContrast
  , saturation
  , setSaturation
  , brightnessSelector
  , contrastSelector
  , exposureSelector
  , gammaSelector
  , groundAlbedoSelector
  , groundColorSelector
  , horizonElevationSelector
  , saturationSelector
  , setBrightnessSelector
  , setContrastSelector
  , setExposureSelector
  , setGammaSelector
  , setGroundAlbedoSelector
  , setGroundColorSelector
  , setHorizonElevationSelector
  , setSaturationSelector
  , setSunAzimuthSelector
  , setSunElevationSelector
  , setTurbiditySelector
  , setUpperAtmosphereScatteringSelector
  , sunAzimuthSelector
  , sunElevationSelector
  , turbiditySelector
  , updateTextureSelector
  , upperAtmosphereScatteringSelector

  -- * Enum types
  , MDLTextureChannelEncoding(MDLTextureChannelEncoding)
  , pattern MDLTextureChannelEncodingUInt8
  , pattern MDLTextureChannelEncodingUint8
  , pattern MDLTextureChannelEncodingUInt16
  , pattern MDLTextureChannelEncodingUint16
  , pattern MDLTextureChannelEncodingUInt24
  , pattern MDLTextureChannelEncodingUint24
  , pattern MDLTextureChannelEncodingUInt32
  , pattern MDLTextureChannelEncodingUint32
  , pattern MDLTextureChannelEncodingFloat16
  , pattern MDLTextureChannelEncodingFloat16SR
  , pattern MDLTextureChannelEncodingFloat32

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

-- | Call updateTexture if parameters have been changed and a new sky is required.
--
-- ObjC selector: @- updateTexture@
updateTexture :: IsMDLSkyCubeTexture mdlSkyCubeTexture => mdlSkyCubeTexture -> IO ()
updateTexture mdlSkyCubeTexture =
  sendMessage mdlSkyCubeTexture updateTextureSelector

-- | @- turbidity@
turbidity :: IsMDLSkyCubeTexture mdlSkyCubeTexture => mdlSkyCubeTexture -> IO CFloat
turbidity mdlSkyCubeTexture =
  sendMessage mdlSkyCubeTexture turbiditySelector

-- | @- setTurbidity:@
setTurbidity :: IsMDLSkyCubeTexture mdlSkyCubeTexture => mdlSkyCubeTexture -> CFloat -> IO ()
setTurbidity mdlSkyCubeTexture value =
  sendMessage mdlSkyCubeTexture setTurbiditySelector value

-- | @- sunElevation@
sunElevation :: IsMDLSkyCubeTexture mdlSkyCubeTexture => mdlSkyCubeTexture -> IO CFloat
sunElevation mdlSkyCubeTexture =
  sendMessage mdlSkyCubeTexture sunElevationSelector

-- | @- setSunElevation:@
setSunElevation :: IsMDLSkyCubeTexture mdlSkyCubeTexture => mdlSkyCubeTexture -> CFloat -> IO ()
setSunElevation mdlSkyCubeTexture value =
  sendMessage mdlSkyCubeTexture setSunElevationSelector value

-- | @- sunAzimuth@
sunAzimuth :: IsMDLSkyCubeTexture mdlSkyCubeTexture => mdlSkyCubeTexture -> IO CFloat
sunAzimuth mdlSkyCubeTexture =
  sendMessage mdlSkyCubeTexture sunAzimuthSelector

-- | @- setSunAzimuth:@
setSunAzimuth :: IsMDLSkyCubeTexture mdlSkyCubeTexture => mdlSkyCubeTexture -> CFloat -> IO ()
setSunAzimuth mdlSkyCubeTexture value =
  sendMessage mdlSkyCubeTexture setSunAzimuthSelector value

-- | @- upperAtmosphereScattering@
upperAtmosphereScattering :: IsMDLSkyCubeTexture mdlSkyCubeTexture => mdlSkyCubeTexture -> IO CFloat
upperAtmosphereScattering mdlSkyCubeTexture =
  sendMessage mdlSkyCubeTexture upperAtmosphereScatteringSelector

-- | @- setUpperAtmosphereScattering:@
setUpperAtmosphereScattering :: IsMDLSkyCubeTexture mdlSkyCubeTexture => mdlSkyCubeTexture -> CFloat -> IO ()
setUpperAtmosphereScattering mdlSkyCubeTexture value =
  sendMessage mdlSkyCubeTexture setUpperAtmosphereScatteringSelector value

-- | @- groundAlbedo@
groundAlbedo :: IsMDLSkyCubeTexture mdlSkyCubeTexture => mdlSkyCubeTexture -> IO CFloat
groundAlbedo mdlSkyCubeTexture =
  sendMessage mdlSkyCubeTexture groundAlbedoSelector

-- | @- setGroundAlbedo:@
setGroundAlbedo :: IsMDLSkyCubeTexture mdlSkyCubeTexture => mdlSkyCubeTexture -> CFloat -> IO ()
setGroundAlbedo mdlSkyCubeTexture value =
  sendMessage mdlSkyCubeTexture setGroundAlbedoSelector value

-- | @- horizonElevation@
horizonElevation :: IsMDLSkyCubeTexture mdlSkyCubeTexture => mdlSkyCubeTexture -> IO CFloat
horizonElevation mdlSkyCubeTexture =
  sendMessage mdlSkyCubeTexture horizonElevationSelector

-- | @- setHorizonElevation:@
setHorizonElevation :: IsMDLSkyCubeTexture mdlSkyCubeTexture => mdlSkyCubeTexture -> CFloat -> IO ()
setHorizonElevation mdlSkyCubeTexture value =
  sendMessage mdlSkyCubeTexture setHorizonElevationSelector value

-- | @- groundColor@
groundColor :: IsMDLSkyCubeTexture mdlSkyCubeTexture => mdlSkyCubeTexture -> IO (Ptr ())
groundColor mdlSkyCubeTexture =
  sendMessage mdlSkyCubeTexture groundColorSelector

-- | @- setGroundColor:@
setGroundColor :: IsMDLSkyCubeTexture mdlSkyCubeTexture => mdlSkyCubeTexture -> Ptr () -> IO ()
setGroundColor mdlSkyCubeTexture value =
  sendMessage mdlSkyCubeTexture setGroundColorSelector value

-- | @- gamma@
gamma :: IsMDLSkyCubeTexture mdlSkyCubeTexture => mdlSkyCubeTexture -> IO CFloat
gamma mdlSkyCubeTexture =
  sendMessage mdlSkyCubeTexture gammaSelector

-- | @- setGamma:@
setGamma :: IsMDLSkyCubeTexture mdlSkyCubeTexture => mdlSkyCubeTexture -> CFloat -> IO ()
setGamma mdlSkyCubeTexture value =
  sendMessage mdlSkyCubeTexture setGammaSelector value

-- | @- exposure@
exposure :: IsMDLSkyCubeTexture mdlSkyCubeTexture => mdlSkyCubeTexture -> IO CFloat
exposure mdlSkyCubeTexture =
  sendMessage mdlSkyCubeTexture exposureSelector

-- | @- setExposure:@
setExposure :: IsMDLSkyCubeTexture mdlSkyCubeTexture => mdlSkyCubeTexture -> CFloat -> IO ()
setExposure mdlSkyCubeTexture value =
  sendMessage mdlSkyCubeTexture setExposureSelector value

-- | @- brightness@
brightness :: IsMDLSkyCubeTexture mdlSkyCubeTexture => mdlSkyCubeTexture -> IO CFloat
brightness mdlSkyCubeTexture =
  sendMessage mdlSkyCubeTexture brightnessSelector

-- | @- setBrightness:@
setBrightness :: IsMDLSkyCubeTexture mdlSkyCubeTexture => mdlSkyCubeTexture -> CFloat -> IO ()
setBrightness mdlSkyCubeTexture value =
  sendMessage mdlSkyCubeTexture setBrightnessSelector value

-- | @- contrast@
contrast :: IsMDLSkyCubeTexture mdlSkyCubeTexture => mdlSkyCubeTexture -> IO CFloat
contrast mdlSkyCubeTexture =
  sendMessage mdlSkyCubeTexture contrastSelector

-- | @- setContrast:@
setContrast :: IsMDLSkyCubeTexture mdlSkyCubeTexture => mdlSkyCubeTexture -> CFloat -> IO ()
setContrast mdlSkyCubeTexture value =
  sendMessage mdlSkyCubeTexture setContrastSelector value

-- | @- saturation@
saturation :: IsMDLSkyCubeTexture mdlSkyCubeTexture => mdlSkyCubeTexture -> IO CFloat
saturation mdlSkyCubeTexture =
  sendMessage mdlSkyCubeTexture saturationSelector

-- | @- setSaturation:@
setSaturation :: IsMDLSkyCubeTexture mdlSkyCubeTexture => mdlSkyCubeTexture -> CFloat -> IO ()
setSaturation mdlSkyCubeTexture value =
  sendMessage mdlSkyCubeTexture setSaturationSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @updateTexture@
updateTextureSelector :: Selector '[] ()
updateTextureSelector = mkSelector "updateTexture"

-- | @Selector@ for @turbidity@
turbiditySelector :: Selector '[] CFloat
turbiditySelector = mkSelector "turbidity"

-- | @Selector@ for @setTurbidity:@
setTurbiditySelector :: Selector '[CFloat] ()
setTurbiditySelector = mkSelector "setTurbidity:"

-- | @Selector@ for @sunElevation@
sunElevationSelector :: Selector '[] CFloat
sunElevationSelector = mkSelector "sunElevation"

-- | @Selector@ for @setSunElevation:@
setSunElevationSelector :: Selector '[CFloat] ()
setSunElevationSelector = mkSelector "setSunElevation:"

-- | @Selector@ for @sunAzimuth@
sunAzimuthSelector :: Selector '[] CFloat
sunAzimuthSelector = mkSelector "sunAzimuth"

-- | @Selector@ for @setSunAzimuth:@
setSunAzimuthSelector :: Selector '[CFloat] ()
setSunAzimuthSelector = mkSelector "setSunAzimuth:"

-- | @Selector@ for @upperAtmosphereScattering@
upperAtmosphereScatteringSelector :: Selector '[] CFloat
upperAtmosphereScatteringSelector = mkSelector "upperAtmosphereScattering"

-- | @Selector@ for @setUpperAtmosphereScattering:@
setUpperAtmosphereScatteringSelector :: Selector '[CFloat] ()
setUpperAtmosphereScatteringSelector = mkSelector "setUpperAtmosphereScattering:"

-- | @Selector@ for @groundAlbedo@
groundAlbedoSelector :: Selector '[] CFloat
groundAlbedoSelector = mkSelector "groundAlbedo"

-- | @Selector@ for @setGroundAlbedo:@
setGroundAlbedoSelector :: Selector '[CFloat] ()
setGroundAlbedoSelector = mkSelector "setGroundAlbedo:"

-- | @Selector@ for @horizonElevation@
horizonElevationSelector :: Selector '[] CFloat
horizonElevationSelector = mkSelector "horizonElevation"

-- | @Selector@ for @setHorizonElevation:@
setHorizonElevationSelector :: Selector '[CFloat] ()
setHorizonElevationSelector = mkSelector "setHorizonElevation:"

-- | @Selector@ for @groundColor@
groundColorSelector :: Selector '[] (Ptr ())
groundColorSelector = mkSelector "groundColor"

-- | @Selector@ for @setGroundColor:@
setGroundColorSelector :: Selector '[Ptr ()] ()
setGroundColorSelector = mkSelector "setGroundColor:"

-- | @Selector@ for @gamma@
gammaSelector :: Selector '[] CFloat
gammaSelector = mkSelector "gamma"

-- | @Selector@ for @setGamma:@
setGammaSelector :: Selector '[CFloat] ()
setGammaSelector = mkSelector "setGamma:"

-- | @Selector@ for @exposure@
exposureSelector :: Selector '[] CFloat
exposureSelector = mkSelector "exposure"

-- | @Selector@ for @setExposure:@
setExposureSelector :: Selector '[CFloat] ()
setExposureSelector = mkSelector "setExposure:"

-- | @Selector@ for @brightness@
brightnessSelector :: Selector '[] CFloat
brightnessSelector = mkSelector "brightness"

-- | @Selector@ for @setBrightness:@
setBrightnessSelector :: Selector '[CFloat] ()
setBrightnessSelector = mkSelector "setBrightness:"

-- | @Selector@ for @contrast@
contrastSelector :: Selector '[] CFloat
contrastSelector = mkSelector "contrast"

-- | @Selector@ for @setContrast:@
setContrastSelector :: Selector '[CFloat] ()
setContrastSelector = mkSelector "setContrast:"

-- | @Selector@ for @saturation@
saturationSelector :: Selector '[] CFloat
saturationSelector = mkSelector "saturation"

-- | @Selector@ for @setSaturation:@
setSaturationSelector :: Selector '[CFloat] ()
setSaturationSelector = mkSelector "setSaturation:"

