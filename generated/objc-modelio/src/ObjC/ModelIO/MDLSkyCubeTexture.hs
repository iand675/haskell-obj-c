{-# LANGUAGE PatternSynonyms #-}
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
  , updateTextureSelector
  , turbiditySelector
  , setTurbiditySelector
  , sunElevationSelector
  , setSunElevationSelector
  , sunAzimuthSelector
  , setSunAzimuthSelector
  , upperAtmosphereScatteringSelector
  , setUpperAtmosphereScatteringSelector
  , groundAlbedoSelector
  , setGroundAlbedoSelector
  , horizonElevationSelector
  , setHorizonElevationSelector
  , groundColorSelector
  , setGroundColorSelector
  , gammaSelector
  , setGammaSelector
  , exposureSelector
  , setExposureSelector
  , brightnessSelector
  , setBrightnessSelector
  , contrastSelector
  , setContrastSelector
  , saturationSelector
  , setSaturationSelector

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

-- | Call updateTexture if parameters have been changed and a new sky is required.
--
-- ObjC selector: @- updateTexture@
updateTexture :: IsMDLSkyCubeTexture mdlSkyCubeTexture => mdlSkyCubeTexture -> IO ()
updateTexture mdlSkyCubeTexture  =
  sendMsg mdlSkyCubeTexture (mkSelector "updateTexture") retVoid []

-- | @- turbidity@
turbidity :: IsMDLSkyCubeTexture mdlSkyCubeTexture => mdlSkyCubeTexture -> IO CFloat
turbidity mdlSkyCubeTexture  =
  sendMsg mdlSkyCubeTexture (mkSelector "turbidity") retCFloat []

-- | @- setTurbidity:@
setTurbidity :: IsMDLSkyCubeTexture mdlSkyCubeTexture => mdlSkyCubeTexture -> CFloat -> IO ()
setTurbidity mdlSkyCubeTexture  value =
  sendMsg mdlSkyCubeTexture (mkSelector "setTurbidity:") retVoid [argCFloat (fromIntegral value)]

-- | @- sunElevation@
sunElevation :: IsMDLSkyCubeTexture mdlSkyCubeTexture => mdlSkyCubeTexture -> IO CFloat
sunElevation mdlSkyCubeTexture  =
  sendMsg mdlSkyCubeTexture (mkSelector "sunElevation") retCFloat []

-- | @- setSunElevation:@
setSunElevation :: IsMDLSkyCubeTexture mdlSkyCubeTexture => mdlSkyCubeTexture -> CFloat -> IO ()
setSunElevation mdlSkyCubeTexture  value =
  sendMsg mdlSkyCubeTexture (mkSelector "setSunElevation:") retVoid [argCFloat (fromIntegral value)]

-- | @- sunAzimuth@
sunAzimuth :: IsMDLSkyCubeTexture mdlSkyCubeTexture => mdlSkyCubeTexture -> IO CFloat
sunAzimuth mdlSkyCubeTexture  =
  sendMsg mdlSkyCubeTexture (mkSelector "sunAzimuth") retCFloat []

-- | @- setSunAzimuth:@
setSunAzimuth :: IsMDLSkyCubeTexture mdlSkyCubeTexture => mdlSkyCubeTexture -> CFloat -> IO ()
setSunAzimuth mdlSkyCubeTexture  value =
  sendMsg mdlSkyCubeTexture (mkSelector "setSunAzimuth:") retVoid [argCFloat (fromIntegral value)]

-- | @- upperAtmosphereScattering@
upperAtmosphereScattering :: IsMDLSkyCubeTexture mdlSkyCubeTexture => mdlSkyCubeTexture -> IO CFloat
upperAtmosphereScattering mdlSkyCubeTexture  =
  sendMsg mdlSkyCubeTexture (mkSelector "upperAtmosphereScattering") retCFloat []

-- | @- setUpperAtmosphereScattering:@
setUpperAtmosphereScattering :: IsMDLSkyCubeTexture mdlSkyCubeTexture => mdlSkyCubeTexture -> CFloat -> IO ()
setUpperAtmosphereScattering mdlSkyCubeTexture  value =
  sendMsg mdlSkyCubeTexture (mkSelector "setUpperAtmosphereScattering:") retVoid [argCFloat (fromIntegral value)]

-- | @- groundAlbedo@
groundAlbedo :: IsMDLSkyCubeTexture mdlSkyCubeTexture => mdlSkyCubeTexture -> IO CFloat
groundAlbedo mdlSkyCubeTexture  =
  sendMsg mdlSkyCubeTexture (mkSelector "groundAlbedo") retCFloat []

-- | @- setGroundAlbedo:@
setGroundAlbedo :: IsMDLSkyCubeTexture mdlSkyCubeTexture => mdlSkyCubeTexture -> CFloat -> IO ()
setGroundAlbedo mdlSkyCubeTexture  value =
  sendMsg mdlSkyCubeTexture (mkSelector "setGroundAlbedo:") retVoid [argCFloat (fromIntegral value)]

-- | @- horizonElevation@
horizonElevation :: IsMDLSkyCubeTexture mdlSkyCubeTexture => mdlSkyCubeTexture -> IO CFloat
horizonElevation mdlSkyCubeTexture  =
  sendMsg mdlSkyCubeTexture (mkSelector "horizonElevation") retCFloat []

-- | @- setHorizonElevation:@
setHorizonElevation :: IsMDLSkyCubeTexture mdlSkyCubeTexture => mdlSkyCubeTexture -> CFloat -> IO ()
setHorizonElevation mdlSkyCubeTexture  value =
  sendMsg mdlSkyCubeTexture (mkSelector "setHorizonElevation:") retVoid [argCFloat (fromIntegral value)]

-- | @- groundColor@
groundColor :: IsMDLSkyCubeTexture mdlSkyCubeTexture => mdlSkyCubeTexture -> IO (Ptr ())
groundColor mdlSkyCubeTexture  =
  fmap castPtr $ sendMsg mdlSkyCubeTexture (mkSelector "groundColor") (retPtr retVoid) []

-- | @- setGroundColor:@
setGroundColor :: IsMDLSkyCubeTexture mdlSkyCubeTexture => mdlSkyCubeTexture -> Ptr () -> IO ()
setGroundColor mdlSkyCubeTexture  value =
  sendMsg mdlSkyCubeTexture (mkSelector "setGroundColor:") retVoid [argPtr value]

-- | @- gamma@
gamma :: IsMDLSkyCubeTexture mdlSkyCubeTexture => mdlSkyCubeTexture -> IO CFloat
gamma mdlSkyCubeTexture  =
  sendMsg mdlSkyCubeTexture (mkSelector "gamma") retCFloat []

-- | @- setGamma:@
setGamma :: IsMDLSkyCubeTexture mdlSkyCubeTexture => mdlSkyCubeTexture -> CFloat -> IO ()
setGamma mdlSkyCubeTexture  value =
  sendMsg mdlSkyCubeTexture (mkSelector "setGamma:") retVoid [argCFloat (fromIntegral value)]

-- | @- exposure@
exposure :: IsMDLSkyCubeTexture mdlSkyCubeTexture => mdlSkyCubeTexture -> IO CFloat
exposure mdlSkyCubeTexture  =
  sendMsg mdlSkyCubeTexture (mkSelector "exposure") retCFloat []

-- | @- setExposure:@
setExposure :: IsMDLSkyCubeTexture mdlSkyCubeTexture => mdlSkyCubeTexture -> CFloat -> IO ()
setExposure mdlSkyCubeTexture  value =
  sendMsg mdlSkyCubeTexture (mkSelector "setExposure:") retVoid [argCFloat (fromIntegral value)]

-- | @- brightness@
brightness :: IsMDLSkyCubeTexture mdlSkyCubeTexture => mdlSkyCubeTexture -> IO CFloat
brightness mdlSkyCubeTexture  =
  sendMsg mdlSkyCubeTexture (mkSelector "brightness") retCFloat []

-- | @- setBrightness:@
setBrightness :: IsMDLSkyCubeTexture mdlSkyCubeTexture => mdlSkyCubeTexture -> CFloat -> IO ()
setBrightness mdlSkyCubeTexture  value =
  sendMsg mdlSkyCubeTexture (mkSelector "setBrightness:") retVoid [argCFloat (fromIntegral value)]

-- | @- contrast@
contrast :: IsMDLSkyCubeTexture mdlSkyCubeTexture => mdlSkyCubeTexture -> IO CFloat
contrast mdlSkyCubeTexture  =
  sendMsg mdlSkyCubeTexture (mkSelector "contrast") retCFloat []

-- | @- setContrast:@
setContrast :: IsMDLSkyCubeTexture mdlSkyCubeTexture => mdlSkyCubeTexture -> CFloat -> IO ()
setContrast mdlSkyCubeTexture  value =
  sendMsg mdlSkyCubeTexture (mkSelector "setContrast:") retVoid [argCFloat (fromIntegral value)]

-- | @- saturation@
saturation :: IsMDLSkyCubeTexture mdlSkyCubeTexture => mdlSkyCubeTexture -> IO CFloat
saturation mdlSkyCubeTexture  =
  sendMsg mdlSkyCubeTexture (mkSelector "saturation") retCFloat []

-- | @- setSaturation:@
setSaturation :: IsMDLSkyCubeTexture mdlSkyCubeTexture => mdlSkyCubeTexture -> CFloat -> IO ()
setSaturation mdlSkyCubeTexture  value =
  sendMsg mdlSkyCubeTexture (mkSelector "setSaturation:") retVoid [argCFloat (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @updateTexture@
updateTextureSelector :: Selector
updateTextureSelector = mkSelector "updateTexture"

-- | @Selector@ for @turbidity@
turbiditySelector :: Selector
turbiditySelector = mkSelector "turbidity"

-- | @Selector@ for @setTurbidity:@
setTurbiditySelector :: Selector
setTurbiditySelector = mkSelector "setTurbidity:"

-- | @Selector@ for @sunElevation@
sunElevationSelector :: Selector
sunElevationSelector = mkSelector "sunElevation"

-- | @Selector@ for @setSunElevation:@
setSunElevationSelector :: Selector
setSunElevationSelector = mkSelector "setSunElevation:"

-- | @Selector@ for @sunAzimuth@
sunAzimuthSelector :: Selector
sunAzimuthSelector = mkSelector "sunAzimuth"

-- | @Selector@ for @setSunAzimuth:@
setSunAzimuthSelector :: Selector
setSunAzimuthSelector = mkSelector "setSunAzimuth:"

-- | @Selector@ for @upperAtmosphereScattering@
upperAtmosphereScatteringSelector :: Selector
upperAtmosphereScatteringSelector = mkSelector "upperAtmosphereScattering"

-- | @Selector@ for @setUpperAtmosphereScattering:@
setUpperAtmosphereScatteringSelector :: Selector
setUpperAtmosphereScatteringSelector = mkSelector "setUpperAtmosphereScattering:"

-- | @Selector@ for @groundAlbedo@
groundAlbedoSelector :: Selector
groundAlbedoSelector = mkSelector "groundAlbedo"

-- | @Selector@ for @setGroundAlbedo:@
setGroundAlbedoSelector :: Selector
setGroundAlbedoSelector = mkSelector "setGroundAlbedo:"

-- | @Selector@ for @horizonElevation@
horizonElevationSelector :: Selector
horizonElevationSelector = mkSelector "horizonElevation"

-- | @Selector@ for @setHorizonElevation:@
setHorizonElevationSelector :: Selector
setHorizonElevationSelector = mkSelector "setHorizonElevation:"

-- | @Selector@ for @groundColor@
groundColorSelector :: Selector
groundColorSelector = mkSelector "groundColor"

-- | @Selector@ for @setGroundColor:@
setGroundColorSelector :: Selector
setGroundColorSelector = mkSelector "setGroundColor:"

-- | @Selector@ for @gamma@
gammaSelector :: Selector
gammaSelector = mkSelector "gamma"

-- | @Selector@ for @setGamma:@
setGammaSelector :: Selector
setGammaSelector = mkSelector "setGamma:"

-- | @Selector@ for @exposure@
exposureSelector :: Selector
exposureSelector = mkSelector "exposure"

-- | @Selector@ for @setExposure:@
setExposureSelector :: Selector
setExposureSelector = mkSelector "setExposure:"

-- | @Selector@ for @brightness@
brightnessSelector :: Selector
brightnessSelector = mkSelector "brightness"

-- | @Selector@ for @setBrightness:@
setBrightnessSelector :: Selector
setBrightnessSelector = mkSelector "setBrightness:"

-- | @Selector@ for @contrast@
contrastSelector :: Selector
contrastSelector = mkSelector "contrast"

-- | @Selector@ for @setContrast:@
setContrastSelector :: Selector
setContrastSelector = mkSelector "setContrast:"

-- | @Selector@ for @saturation@
saturationSelector :: Selector
saturationSelector = mkSelector "saturation"

-- | @Selector@ for @setSaturation:@
setSaturationSelector :: Selector
setSaturationSelector = mkSelector "setSaturation:"

