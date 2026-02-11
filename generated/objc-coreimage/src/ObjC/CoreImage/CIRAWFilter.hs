{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CIRAWFilter@.
module ObjC.CoreImage.CIRAWFilter
  ( CIRAWFilter
  , IsCIRAWFilter(..)
  , filterWithImageURL
  , filterWithImageData_identifierHint
  , filterWithCVPixelBuffer_properties
  , supportedCameraModels
  , supportedDecoderVersions
  , properties
  , orientation
  , setOrientation
  , draftModeEnabled
  , setDraftModeEnabled
  , decoderVersion
  , setDecoderVersion
  , scaleFactor
  , setScaleFactor
  , exposure
  , setExposure
  , baselineExposure
  , setBaselineExposure
  , shadowBias
  , setShadowBias
  , boostAmount
  , setBoostAmount
  , boostShadowAmount
  , setBoostShadowAmount
  , highlightRecoverySupported
  , highlightRecoveryEnabled
  , setHighlightRecoveryEnabled
  , gamutMappingEnabled
  , setGamutMappingEnabled
  , lensCorrectionSupported
  , lensCorrectionEnabled
  , setLensCorrectionEnabled
  , luminanceNoiseReductionSupported
  , luminanceNoiseReductionAmount
  , setLuminanceNoiseReductionAmount
  , colorNoiseReductionSupported
  , colorNoiseReductionAmount
  , setColorNoiseReductionAmount
  , sharpnessSupported
  , sharpnessAmount
  , setSharpnessAmount
  , contrastSupported
  , contrastAmount
  , setContrastAmount
  , detailSupported
  , detailAmount
  , setDetailAmount
  , moireReductionSupported
  , moireReductionAmount
  , setMoireReductionAmount
  , localToneMapSupported
  , localToneMapAmount
  , setLocalToneMapAmount
  , extendedDynamicRangeAmount
  , setExtendedDynamicRangeAmount
  , neutralTemperature
  , setNeutralTemperature
  , neutralTint
  , setNeutralTint
  , linearSpaceFilter
  , setLinearSpaceFilter
  , previewImage
  , portraitEffectsMatte
  , semanticSegmentationSkinMatte
  , semanticSegmentationHairMatte
  , semanticSegmentationGlassesMatte
  , semanticSegmentationSkyMatte
  , semanticSegmentationTeethMatte
  , filterWithImageURLSelector
  , filterWithImageData_identifierHintSelector
  , filterWithCVPixelBuffer_propertiesSelector
  , supportedCameraModelsSelector
  , supportedDecoderVersionsSelector
  , propertiesSelector
  , orientationSelector
  , setOrientationSelector
  , draftModeEnabledSelector
  , setDraftModeEnabledSelector
  , decoderVersionSelector
  , setDecoderVersionSelector
  , scaleFactorSelector
  , setScaleFactorSelector
  , exposureSelector
  , setExposureSelector
  , baselineExposureSelector
  , setBaselineExposureSelector
  , shadowBiasSelector
  , setShadowBiasSelector
  , boostAmountSelector
  , setBoostAmountSelector
  , boostShadowAmountSelector
  , setBoostShadowAmountSelector
  , highlightRecoverySupportedSelector
  , highlightRecoveryEnabledSelector
  , setHighlightRecoveryEnabledSelector
  , gamutMappingEnabledSelector
  , setGamutMappingEnabledSelector
  , lensCorrectionSupportedSelector
  , lensCorrectionEnabledSelector
  , setLensCorrectionEnabledSelector
  , luminanceNoiseReductionSupportedSelector
  , luminanceNoiseReductionAmountSelector
  , setLuminanceNoiseReductionAmountSelector
  , colorNoiseReductionSupportedSelector
  , colorNoiseReductionAmountSelector
  , setColorNoiseReductionAmountSelector
  , sharpnessSupportedSelector
  , sharpnessAmountSelector
  , setSharpnessAmountSelector
  , contrastSupportedSelector
  , contrastAmountSelector
  , setContrastAmountSelector
  , detailSupportedSelector
  , detailAmountSelector
  , setDetailAmountSelector
  , moireReductionSupportedSelector
  , moireReductionAmountSelector
  , setMoireReductionAmountSelector
  , localToneMapSupportedSelector
  , localToneMapAmountSelector
  , setLocalToneMapAmountSelector
  , extendedDynamicRangeAmountSelector
  , setExtendedDynamicRangeAmountSelector
  , neutralTemperatureSelector
  , setNeutralTemperatureSelector
  , neutralTintSelector
  , setNeutralTintSelector
  , linearSpaceFilterSelector
  , setLinearSpaceFilterSelector
  , previewImageSelector
  , portraitEffectsMatteSelector
  , semanticSegmentationSkinMatteSelector
  , semanticSegmentationHairMatteSelector
  , semanticSegmentationGlassesMatteSelector
  , semanticSegmentationSkyMatteSelector
  , semanticSegmentationTeethMatteSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ filterWithImageURL:@
filterWithImageURL :: IsNSURL url => url -> IO (Id CIRAWFilter)
filterWithImageURL url =
  do
    cls' <- getRequiredClass "CIRAWFilter"
    withObjCPtr url $ \raw_url ->
      sendClassMsg cls' (mkSelector "filterWithImageURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= retainedObject . castPtr

-- | @+ filterWithImageData:identifierHint:@
filterWithImageData_identifierHint :: (IsNSData data_, IsNSString identifierHint) => data_ -> identifierHint -> IO (Id CIRAWFilter)
filterWithImageData_identifierHint data_ identifierHint =
  do
    cls' <- getRequiredClass "CIRAWFilter"
    withObjCPtr data_ $ \raw_data_ ->
      withObjCPtr identifierHint $ \raw_identifierHint ->
        sendClassMsg cls' (mkSelector "filterWithImageData:identifierHint:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_identifierHint :: Ptr ())] >>= retainedObject . castPtr

-- | @+ filterWithCVPixelBuffer:properties:@
filterWithCVPixelBuffer_properties :: IsNSDictionary properties => Ptr () -> properties -> IO (Id CIRAWFilter)
filterWithCVPixelBuffer_properties buffer properties =
  do
    cls' <- getRequiredClass "CIRAWFilter"
    withObjCPtr properties $ \raw_properties ->
      sendClassMsg cls' (mkSelector "filterWithCVPixelBuffer:properties:") (retPtr retVoid) [argPtr buffer, argPtr (castPtr raw_properties :: Ptr ())] >>= retainedObject . castPtr

-- | @+ supportedCameraModels@
supportedCameraModels :: IO (Id NSArray)
supportedCameraModels  =
  do
    cls' <- getRequiredClass "CIRAWFilter"
    sendClassMsg cls' (mkSelector "supportedCameraModels") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- supportedDecoderVersions@
supportedDecoderVersions :: IsCIRAWFilter cirawFilter => cirawFilter -> IO (Id NSArray)
supportedDecoderVersions cirawFilter  =
  sendMsg cirawFilter (mkSelector "supportedDecoderVersions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- properties@
properties :: IsCIRAWFilter cirawFilter => cirawFilter -> IO (Id NSDictionary)
properties cirawFilter  =
  sendMsg cirawFilter (mkSelector "properties") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- orientation@
orientation :: IsCIRAWFilter cirawFilter => cirawFilter -> IO CInt
orientation cirawFilter  =
  sendMsg cirawFilter (mkSelector "orientation") retCInt []

-- | @- setOrientation:@
setOrientation :: IsCIRAWFilter cirawFilter => cirawFilter -> CInt -> IO ()
setOrientation cirawFilter  value =
  sendMsg cirawFilter (mkSelector "setOrientation:") retVoid [argCInt (fromIntegral value)]

-- | @- draftModeEnabled@
draftModeEnabled :: IsCIRAWFilter cirawFilter => cirawFilter -> IO Bool
draftModeEnabled cirawFilter  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cirawFilter (mkSelector "draftModeEnabled") retCULong []

-- | @- setDraftModeEnabled:@
setDraftModeEnabled :: IsCIRAWFilter cirawFilter => cirawFilter -> Bool -> IO ()
setDraftModeEnabled cirawFilter  value =
  sendMsg cirawFilter (mkSelector "setDraftModeEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- decoderVersion@
decoderVersion :: IsCIRAWFilter cirawFilter => cirawFilter -> IO (Id NSString)
decoderVersion cirawFilter  =
  sendMsg cirawFilter (mkSelector "decoderVersion") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDecoderVersion:@
setDecoderVersion :: (IsCIRAWFilter cirawFilter, IsNSString value) => cirawFilter -> value -> IO ()
setDecoderVersion cirawFilter  value =
withObjCPtr value $ \raw_value ->
    sendMsg cirawFilter (mkSelector "setDecoderVersion:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- scaleFactor@
scaleFactor :: IsCIRAWFilter cirawFilter => cirawFilter -> IO CFloat
scaleFactor cirawFilter  =
  sendMsg cirawFilter (mkSelector "scaleFactor") retCFloat []

-- | @- setScaleFactor:@
setScaleFactor :: IsCIRAWFilter cirawFilter => cirawFilter -> CFloat -> IO ()
setScaleFactor cirawFilter  value =
  sendMsg cirawFilter (mkSelector "setScaleFactor:") retVoid [argCFloat (fromIntegral value)]

-- | @- exposure@
exposure :: IsCIRAWFilter cirawFilter => cirawFilter -> IO CFloat
exposure cirawFilter  =
  sendMsg cirawFilter (mkSelector "exposure") retCFloat []

-- | @- setExposure:@
setExposure :: IsCIRAWFilter cirawFilter => cirawFilter -> CFloat -> IO ()
setExposure cirawFilter  value =
  sendMsg cirawFilter (mkSelector "setExposure:") retVoid [argCFloat (fromIntegral value)]

-- | @- baselineExposure@
baselineExposure :: IsCIRAWFilter cirawFilter => cirawFilter -> IO CFloat
baselineExposure cirawFilter  =
  sendMsg cirawFilter (mkSelector "baselineExposure") retCFloat []

-- | @- setBaselineExposure:@
setBaselineExposure :: IsCIRAWFilter cirawFilter => cirawFilter -> CFloat -> IO ()
setBaselineExposure cirawFilter  value =
  sendMsg cirawFilter (mkSelector "setBaselineExposure:") retVoid [argCFloat (fromIntegral value)]

-- | @- shadowBias@
shadowBias :: IsCIRAWFilter cirawFilter => cirawFilter -> IO CFloat
shadowBias cirawFilter  =
  sendMsg cirawFilter (mkSelector "shadowBias") retCFloat []

-- | @- setShadowBias:@
setShadowBias :: IsCIRAWFilter cirawFilter => cirawFilter -> CFloat -> IO ()
setShadowBias cirawFilter  value =
  sendMsg cirawFilter (mkSelector "setShadowBias:") retVoid [argCFloat (fromIntegral value)]

-- | @- boostAmount@
boostAmount :: IsCIRAWFilter cirawFilter => cirawFilter -> IO CFloat
boostAmount cirawFilter  =
  sendMsg cirawFilter (mkSelector "boostAmount") retCFloat []

-- | @- setBoostAmount:@
setBoostAmount :: IsCIRAWFilter cirawFilter => cirawFilter -> CFloat -> IO ()
setBoostAmount cirawFilter  value =
  sendMsg cirawFilter (mkSelector "setBoostAmount:") retVoid [argCFloat (fromIntegral value)]

-- | @- boostShadowAmount@
boostShadowAmount :: IsCIRAWFilter cirawFilter => cirawFilter -> IO CFloat
boostShadowAmount cirawFilter  =
  sendMsg cirawFilter (mkSelector "boostShadowAmount") retCFloat []

-- | @- setBoostShadowAmount:@
setBoostShadowAmount :: IsCIRAWFilter cirawFilter => cirawFilter -> CFloat -> IO ()
setBoostShadowAmount cirawFilter  value =
  sendMsg cirawFilter (mkSelector "setBoostShadowAmount:") retVoid [argCFloat (fromIntegral value)]

-- | @- highlightRecoverySupported@
highlightRecoverySupported :: IsCIRAWFilter cirawFilter => cirawFilter -> IO Bool
highlightRecoverySupported cirawFilter  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cirawFilter (mkSelector "highlightRecoverySupported") retCULong []

-- | @- highlightRecoveryEnabled@
highlightRecoveryEnabled :: IsCIRAWFilter cirawFilter => cirawFilter -> IO Bool
highlightRecoveryEnabled cirawFilter  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cirawFilter (mkSelector "highlightRecoveryEnabled") retCULong []

-- | @- setHighlightRecoveryEnabled:@
setHighlightRecoveryEnabled :: IsCIRAWFilter cirawFilter => cirawFilter -> Bool -> IO ()
setHighlightRecoveryEnabled cirawFilter  value =
  sendMsg cirawFilter (mkSelector "setHighlightRecoveryEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- gamutMappingEnabled@
gamutMappingEnabled :: IsCIRAWFilter cirawFilter => cirawFilter -> IO Bool
gamutMappingEnabled cirawFilter  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cirawFilter (mkSelector "gamutMappingEnabled") retCULong []

-- | @- setGamutMappingEnabled:@
setGamutMappingEnabled :: IsCIRAWFilter cirawFilter => cirawFilter -> Bool -> IO ()
setGamutMappingEnabled cirawFilter  value =
  sendMsg cirawFilter (mkSelector "setGamutMappingEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- lensCorrectionSupported@
lensCorrectionSupported :: IsCIRAWFilter cirawFilter => cirawFilter -> IO Bool
lensCorrectionSupported cirawFilter  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cirawFilter (mkSelector "lensCorrectionSupported") retCULong []

-- | @- lensCorrectionEnabled@
lensCorrectionEnabled :: IsCIRAWFilter cirawFilter => cirawFilter -> IO Bool
lensCorrectionEnabled cirawFilter  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cirawFilter (mkSelector "lensCorrectionEnabled") retCULong []

-- | @- setLensCorrectionEnabled:@
setLensCorrectionEnabled :: IsCIRAWFilter cirawFilter => cirawFilter -> Bool -> IO ()
setLensCorrectionEnabled cirawFilter  value =
  sendMsg cirawFilter (mkSelector "setLensCorrectionEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- luminanceNoiseReductionSupported@
luminanceNoiseReductionSupported :: IsCIRAWFilter cirawFilter => cirawFilter -> IO Bool
luminanceNoiseReductionSupported cirawFilter  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cirawFilter (mkSelector "luminanceNoiseReductionSupported") retCULong []

-- | @- luminanceNoiseReductionAmount@
luminanceNoiseReductionAmount :: IsCIRAWFilter cirawFilter => cirawFilter -> IO CFloat
luminanceNoiseReductionAmount cirawFilter  =
  sendMsg cirawFilter (mkSelector "luminanceNoiseReductionAmount") retCFloat []

-- | @- setLuminanceNoiseReductionAmount:@
setLuminanceNoiseReductionAmount :: IsCIRAWFilter cirawFilter => cirawFilter -> CFloat -> IO ()
setLuminanceNoiseReductionAmount cirawFilter  value =
  sendMsg cirawFilter (mkSelector "setLuminanceNoiseReductionAmount:") retVoid [argCFloat (fromIntegral value)]

-- | @- colorNoiseReductionSupported@
colorNoiseReductionSupported :: IsCIRAWFilter cirawFilter => cirawFilter -> IO Bool
colorNoiseReductionSupported cirawFilter  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cirawFilter (mkSelector "colorNoiseReductionSupported") retCULong []

-- | @- colorNoiseReductionAmount@
colorNoiseReductionAmount :: IsCIRAWFilter cirawFilter => cirawFilter -> IO CFloat
colorNoiseReductionAmount cirawFilter  =
  sendMsg cirawFilter (mkSelector "colorNoiseReductionAmount") retCFloat []

-- | @- setColorNoiseReductionAmount:@
setColorNoiseReductionAmount :: IsCIRAWFilter cirawFilter => cirawFilter -> CFloat -> IO ()
setColorNoiseReductionAmount cirawFilter  value =
  sendMsg cirawFilter (mkSelector "setColorNoiseReductionAmount:") retVoid [argCFloat (fromIntegral value)]

-- | @- sharpnessSupported@
sharpnessSupported :: IsCIRAWFilter cirawFilter => cirawFilter -> IO Bool
sharpnessSupported cirawFilter  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cirawFilter (mkSelector "sharpnessSupported") retCULong []

-- | @- sharpnessAmount@
sharpnessAmount :: IsCIRAWFilter cirawFilter => cirawFilter -> IO CFloat
sharpnessAmount cirawFilter  =
  sendMsg cirawFilter (mkSelector "sharpnessAmount") retCFloat []

-- | @- setSharpnessAmount:@
setSharpnessAmount :: IsCIRAWFilter cirawFilter => cirawFilter -> CFloat -> IO ()
setSharpnessAmount cirawFilter  value =
  sendMsg cirawFilter (mkSelector "setSharpnessAmount:") retVoid [argCFloat (fromIntegral value)]

-- | @- contrastSupported@
contrastSupported :: IsCIRAWFilter cirawFilter => cirawFilter -> IO Bool
contrastSupported cirawFilter  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cirawFilter (mkSelector "contrastSupported") retCULong []

-- | @- contrastAmount@
contrastAmount :: IsCIRAWFilter cirawFilter => cirawFilter -> IO CFloat
contrastAmount cirawFilter  =
  sendMsg cirawFilter (mkSelector "contrastAmount") retCFloat []

-- | @- setContrastAmount:@
setContrastAmount :: IsCIRAWFilter cirawFilter => cirawFilter -> CFloat -> IO ()
setContrastAmount cirawFilter  value =
  sendMsg cirawFilter (mkSelector "setContrastAmount:") retVoid [argCFloat (fromIntegral value)]

-- | @- detailSupported@
detailSupported :: IsCIRAWFilter cirawFilter => cirawFilter -> IO Bool
detailSupported cirawFilter  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cirawFilter (mkSelector "detailSupported") retCULong []

-- | @- detailAmount@
detailAmount :: IsCIRAWFilter cirawFilter => cirawFilter -> IO CFloat
detailAmount cirawFilter  =
  sendMsg cirawFilter (mkSelector "detailAmount") retCFloat []

-- | @- setDetailAmount:@
setDetailAmount :: IsCIRAWFilter cirawFilter => cirawFilter -> CFloat -> IO ()
setDetailAmount cirawFilter  value =
  sendMsg cirawFilter (mkSelector "setDetailAmount:") retVoid [argCFloat (fromIntegral value)]

-- | @- moireReductionSupported@
moireReductionSupported :: IsCIRAWFilter cirawFilter => cirawFilter -> IO Bool
moireReductionSupported cirawFilter  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cirawFilter (mkSelector "moireReductionSupported") retCULong []

-- | @- moireReductionAmount@
moireReductionAmount :: IsCIRAWFilter cirawFilter => cirawFilter -> IO CFloat
moireReductionAmount cirawFilter  =
  sendMsg cirawFilter (mkSelector "moireReductionAmount") retCFloat []

-- | @- setMoireReductionAmount:@
setMoireReductionAmount :: IsCIRAWFilter cirawFilter => cirawFilter -> CFloat -> IO ()
setMoireReductionAmount cirawFilter  value =
  sendMsg cirawFilter (mkSelector "setMoireReductionAmount:") retVoid [argCFloat (fromIntegral value)]

-- | @- localToneMapSupported@
localToneMapSupported :: IsCIRAWFilter cirawFilter => cirawFilter -> IO Bool
localToneMapSupported cirawFilter  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cirawFilter (mkSelector "localToneMapSupported") retCULong []

-- | @- localToneMapAmount@
localToneMapAmount :: IsCIRAWFilter cirawFilter => cirawFilter -> IO CFloat
localToneMapAmount cirawFilter  =
  sendMsg cirawFilter (mkSelector "localToneMapAmount") retCFloat []

-- | @- setLocalToneMapAmount:@
setLocalToneMapAmount :: IsCIRAWFilter cirawFilter => cirawFilter -> CFloat -> IO ()
setLocalToneMapAmount cirawFilter  value =
  sendMsg cirawFilter (mkSelector "setLocalToneMapAmount:") retVoid [argCFloat (fromIntegral value)]

-- | @- extendedDynamicRangeAmount@
extendedDynamicRangeAmount :: IsCIRAWFilter cirawFilter => cirawFilter -> IO CFloat
extendedDynamicRangeAmount cirawFilter  =
  sendMsg cirawFilter (mkSelector "extendedDynamicRangeAmount") retCFloat []

-- | @- setExtendedDynamicRangeAmount:@
setExtendedDynamicRangeAmount :: IsCIRAWFilter cirawFilter => cirawFilter -> CFloat -> IO ()
setExtendedDynamicRangeAmount cirawFilter  value =
  sendMsg cirawFilter (mkSelector "setExtendedDynamicRangeAmount:") retVoid [argCFloat (fromIntegral value)]

-- | @- neutralTemperature@
neutralTemperature :: IsCIRAWFilter cirawFilter => cirawFilter -> IO CFloat
neutralTemperature cirawFilter  =
  sendMsg cirawFilter (mkSelector "neutralTemperature") retCFloat []

-- | @- setNeutralTemperature:@
setNeutralTemperature :: IsCIRAWFilter cirawFilter => cirawFilter -> CFloat -> IO ()
setNeutralTemperature cirawFilter  value =
  sendMsg cirawFilter (mkSelector "setNeutralTemperature:") retVoid [argCFloat (fromIntegral value)]

-- | @- neutralTint@
neutralTint :: IsCIRAWFilter cirawFilter => cirawFilter -> IO CFloat
neutralTint cirawFilter  =
  sendMsg cirawFilter (mkSelector "neutralTint") retCFloat []

-- | @- setNeutralTint:@
setNeutralTint :: IsCIRAWFilter cirawFilter => cirawFilter -> CFloat -> IO ()
setNeutralTint cirawFilter  value =
  sendMsg cirawFilter (mkSelector "setNeutralTint:") retVoid [argCFloat (fromIntegral value)]

-- | @- linearSpaceFilter@
linearSpaceFilter :: IsCIRAWFilter cirawFilter => cirawFilter -> IO (Id CIFilter)
linearSpaceFilter cirawFilter  =
  sendMsg cirawFilter (mkSelector "linearSpaceFilter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLinearSpaceFilter:@
setLinearSpaceFilter :: (IsCIRAWFilter cirawFilter, IsCIFilter value) => cirawFilter -> value -> IO ()
setLinearSpaceFilter cirawFilter  value =
withObjCPtr value $ \raw_value ->
    sendMsg cirawFilter (mkSelector "setLinearSpaceFilter:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- previewImage@
previewImage :: IsCIRAWFilter cirawFilter => cirawFilter -> IO (Id CIImage)
previewImage cirawFilter  =
  sendMsg cirawFilter (mkSelector "previewImage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- portraitEffectsMatte@
portraitEffectsMatte :: IsCIRAWFilter cirawFilter => cirawFilter -> IO (Id CIImage)
portraitEffectsMatte cirawFilter  =
  sendMsg cirawFilter (mkSelector "portraitEffectsMatte") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- semanticSegmentationSkinMatte@
semanticSegmentationSkinMatte :: IsCIRAWFilter cirawFilter => cirawFilter -> IO (Id CIImage)
semanticSegmentationSkinMatte cirawFilter  =
  sendMsg cirawFilter (mkSelector "semanticSegmentationSkinMatte") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- semanticSegmentationHairMatte@
semanticSegmentationHairMatte :: IsCIRAWFilter cirawFilter => cirawFilter -> IO (Id CIImage)
semanticSegmentationHairMatte cirawFilter  =
  sendMsg cirawFilter (mkSelector "semanticSegmentationHairMatte") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- semanticSegmentationGlassesMatte@
semanticSegmentationGlassesMatte :: IsCIRAWFilter cirawFilter => cirawFilter -> IO (Id CIImage)
semanticSegmentationGlassesMatte cirawFilter  =
  sendMsg cirawFilter (mkSelector "semanticSegmentationGlassesMatte") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- semanticSegmentationSkyMatte@
semanticSegmentationSkyMatte :: IsCIRAWFilter cirawFilter => cirawFilter -> IO (Id CIImage)
semanticSegmentationSkyMatte cirawFilter  =
  sendMsg cirawFilter (mkSelector "semanticSegmentationSkyMatte") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- semanticSegmentationTeethMatte@
semanticSegmentationTeethMatte :: IsCIRAWFilter cirawFilter => cirawFilter -> IO (Id CIImage)
semanticSegmentationTeethMatte cirawFilter  =
  sendMsg cirawFilter (mkSelector "semanticSegmentationTeethMatte") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @filterWithImageURL:@
filterWithImageURLSelector :: Selector
filterWithImageURLSelector = mkSelector "filterWithImageURL:"

-- | @Selector@ for @filterWithImageData:identifierHint:@
filterWithImageData_identifierHintSelector :: Selector
filterWithImageData_identifierHintSelector = mkSelector "filterWithImageData:identifierHint:"

-- | @Selector@ for @filterWithCVPixelBuffer:properties:@
filterWithCVPixelBuffer_propertiesSelector :: Selector
filterWithCVPixelBuffer_propertiesSelector = mkSelector "filterWithCVPixelBuffer:properties:"

-- | @Selector@ for @supportedCameraModels@
supportedCameraModelsSelector :: Selector
supportedCameraModelsSelector = mkSelector "supportedCameraModels"

-- | @Selector@ for @supportedDecoderVersions@
supportedDecoderVersionsSelector :: Selector
supportedDecoderVersionsSelector = mkSelector "supportedDecoderVersions"

-- | @Selector@ for @properties@
propertiesSelector :: Selector
propertiesSelector = mkSelector "properties"

-- | @Selector@ for @orientation@
orientationSelector :: Selector
orientationSelector = mkSelector "orientation"

-- | @Selector@ for @setOrientation:@
setOrientationSelector :: Selector
setOrientationSelector = mkSelector "setOrientation:"

-- | @Selector@ for @draftModeEnabled@
draftModeEnabledSelector :: Selector
draftModeEnabledSelector = mkSelector "draftModeEnabled"

-- | @Selector@ for @setDraftModeEnabled:@
setDraftModeEnabledSelector :: Selector
setDraftModeEnabledSelector = mkSelector "setDraftModeEnabled:"

-- | @Selector@ for @decoderVersion@
decoderVersionSelector :: Selector
decoderVersionSelector = mkSelector "decoderVersion"

-- | @Selector@ for @setDecoderVersion:@
setDecoderVersionSelector :: Selector
setDecoderVersionSelector = mkSelector "setDecoderVersion:"

-- | @Selector@ for @scaleFactor@
scaleFactorSelector :: Selector
scaleFactorSelector = mkSelector "scaleFactor"

-- | @Selector@ for @setScaleFactor:@
setScaleFactorSelector :: Selector
setScaleFactorSelector = mkSelector "setScaleFactor:"

-- | @Selector@ for @exposure@
exposureSelector :: Selector
exposureSelector = mkSelector "exposure"

-- | @Selector@ for @setExposure:@
setExposureSelector :: Selector
setExposureSelector = mkSelector "setExposure:"

-- | @Selector@ for @baselineExposure@
baselineExposureSelector :: Selector
baselineExposureSelector = mkSelector "baselineExposure"

-- | @Selector@ for @setBaselineExposure:@
setBaselineExposureSelector :: Selector
setBaselineExposureSelector = mkSelector "setBaselineExposure:"

-- | @Selector@ for @shadowBias@
shadowBiasSelector :: Selector
shadowBiasSelector = mkSelector "shadowBias"

-- | @Selector@ for @setShadowBias:@
setShadowBiasSelector :: Selector
setShadowBiasSelector = mkSelector "setShadowBias:"

-- | @Selector@ for @boostAmount@
boostAmountSelector :: Selector
boostAmountSelector = mkSelector "boostAmount"

-- | @Selector@ for @setBoostAmount:@
setBoostAmountSelector :: Selector
setBoostAmountSelector = mkSelector "setBoostAmount:"

-- | @Selector@ for @boostShadowAmount@
boostShadowAmountSelector :: Selector
boostShadowAmountSelector = mkSelector "boostShadowAmount"

-- | @Selector@ for @setBoostShadowAmount:@
setBoostShadowAmountSelector :: Selector
setBoostShadowAmountSelector = mkSelector "setBoostShadowAmount:"

-- | @Selector@ for @highlightRecoverySupported@
highlightRecoverySupportedSelector :: Selector
highlightRecoverySupportedSelector = mkSelector "highlightRecoverySupported"

-- | @Selector@ for @highlightRecoveryEnabled@
highlightRecoveryEnabledSelector :: Selector
highlightRecoveryEnabledSelector = mkSelector "highlightRecoveryEnabled"

-- | @Selector@ for @setHighlightRecoveryEnabled:@
setHighlightRecoveryEnabledSelector :: Selector
setHighlightRecoveryEnabledSelector = mkSelector "setHighlightRecoveryEnabled:"

-- | @Selector@ for @gamutMappingEnabled@
gamutMappingEnabledSelector :: Selector
gamutMappingEnabledSelector = mkSelector "gamutMappingEnabled"

-- | @Selector@ for @setGamutMappingEnabled:@
setGamutMappingEnabledSelector :: Selector
setGamutMappingEnabledSelector = mkSelector "setGamutMappingEnabled:"

-- | @Selector@ for @lensCorrectionSupported@
lensCorrectionSupportedSelector :: Selector
lensCorrectionSupportedSelector = mkSelector "lensCorrectionSupported"

-- | @Selector@ for @lensCorrectionEnabled@
lensCorrectionEnabledSelector :: Selector
lensCorrectionEnabledSelector = mkSelector "lensCorrectionEnabled"

-- | @Selector@ for @setLensCorrectionEnabled:@
setLensCorrectionEnabledSelector :: Selector
setLensCorrectionEnabledSelector = mkSelector "setLensCorrectionEnabled:"

-- | @Selector@ for @luminanceNoiseReductionSupported@
luminanceNoiseReductionSupportedSelector :: Selector
luminanceNoiseReductionSupportedSelector = mkSelector "luminanceNoiseReductionSupported"

-- | @Selector@ for @luminanceNoiseReductionAmount@
luminanceNoiseReductionAmountSelector :: Selector
luminanceNoiseReductionAmountSelector = mkSelector "luminanceNoiseReductionAmount"

-- | @Selector@ for @setLuminanceNoiseReductionAmount:@
setLuminanceNoiseReductionAmountSelector :: Selector
setLuminanceNoiseReductionAmountSelector = mkSelector "setLuminanceNoiseReductionAmount:"

-- | @Selector@ for @colorNoiseReductionSupported@
colorNoiseReductionSupportedSelector :: Selector
colorNoiseReductionSupportedSelector = mkSelector "colorNoiseReductionSupported"

-- | @Selector@ for @colorNoiseReductionAmount@
colorNoiseReductionAmountSelector :: Selector
colorNoiseReductionAmountSelector = mkSelector "colorNoiseReductionAmount"

-- | @Selector@ for @setColorNoiseReductionAmount:@
setColorNoiseReductionAmountSelector :: Selector
setColorNoiseReductionAmountSelector = mkSelector "setColorNoiseReductionAmount:"

-- | @Selector@ for @sharpnessSupported@
sharpnessSupportedSelector :: Selector
sharpnessSupportedSelector = mkSelector "sharpnessSupported"

-- | @Selector@ for @sharpnessAmount@
sharpnessAmountSelector :: Selector
sharpnessAmountSelector = mkSelector "sharpnessAmount"

-- | @Selector@ for @setSharpnessAmount:@
setSharpnessAmountSelector :: Selector
setSharpnessAmountSelector = mkSelector "setSharpnessAmount:"

-- | @Selector@ for @contrastSupported@
contrastSupportedSelector :: Selector
contrastSupportedSelector = mkSelector "contrastSupported"

-- | @Selector@ for @contrastAmount@
contrastAmountSelector :: Selector
contrastAmountSelector = mkSelector "contrastAmount"

-- | @Selector@ for @setContrastAmount:@
setContrastAmountSelector :: Selector
setContrastAmountSelector = mkSelector "setContrastAmount:"

-- | @Selector@ for @detailSupported@
detailSupportedSelector :: Selector
detailSupportedSelector = mkSelector "detailSupported"

-- | @Selector@ for @detailAmount@
detailAmountSelector :: Selector
detailAmountSelector = mkSelector "detailAmount"

-- | @Selector@ for @setDetailAmount:@
setDetailAmountSelector :: Selector
setDetailAmountSelector = mkSelector "setDetailAmount:"

-- | @Selector@ for @moireReductionSupported@
moireReductionSupportedSelector :: Selector
moireReductionSupportedSelector = mkSelector "moireReductionSupported"

-- | @Selector@ for @moireReductionAmount@
moireReductionAmountSelector :: Selector
moireReductionAmountSelector = mkSelector "moireReductionAmount"

-- | @Selector@ for @setMoireReductionAmount:@
setMoireReductionAmountSelector :: Selector
setMoireReductionAmountSelector = mkSelector "setMoireReductionAmount:"

-- | @Selector@ for @localToneMapSupported@
localToneMapSupportedSelector :: Selector
localToneMapSupportedSelector = mkSelector "localToneMapSupported"

-- | @Selector@ for @localToneMapAmount@
localToneMapAmountSelector :: Selector
localToneMapAmountSelector = mkSelector "localToneMapAmount"

-- | @Selector@ for @setLocalToneMapAmount:@
setLocalToneMapAmountSelector :: Selector
setLocalToneMapAmountSelector = mkSelector "setLocalToneMapAmount:"

-- | @Selector@ for @extendedDynamicRangeAmount@
extendedDynamicRangeAmountSelector :: Selector
extendedDynamicRangeAmountSelector = mkSelector "extendedDynamicRangeAmount"

-- | @Selector@ for @setExtendedDynamicRangeAmount:@
setExtendedDynamicRangeAmountSelector :: Selector
setExtendedDynamicRangeAmountSelector = mkSelector "setExtendedDynamicRangeAmount:"

-- | @Selector@ for @neutralTemperature@
neutralTemperatureSelector :: Selector
neutralTemperatureSelector = mkSelector "neutralTemperature"

-- | @Selector@ for @setNeutralTemperature:@
setNeutralTemperatureSelector :: Selector
setNeutralTemperatureSelector = mkSelector "setNeutralTemperature:"

-- | @Selector@ for @neutralTint@
neutralTintSelector :: Selector
neutralTintSelector = mkSelector "neutralTint"

-- | @Selector@ for @setNeutralTint:@
setNeutralTintSelector :: Selector
setNeutralTintSelector = mkSelector "setNeutralTint:"

-- | @Selector@ for @linearSpaceFilter@
linearSpaceFilterSelector :: Selector
linearSpaceFilterSelector = mkSelector "linearSpaceFilter"

-- | @Selector@ for @setLinearSpaceFilter:@
setLinearSpaceFilterSelector :: Selector
setLinearSpaceFilterSelector = mkSelector "setLinearSpaceFilter:"

-- | @Selector@ for @previewImage@
previewImageSelector :: Selector
previewImageSelector = mkSelector "previewImage"

-- | @Selector@ for @portraitEffectsMatte@
portraitEffectsMatteSelector :: Selector
portraitEffectsMatteSelector = mkSelector "portraitEffectsMatte"

-- | @Selector@ for @semanticSegmentationSkinMatte@
semanticSegmentationSkinMatteSelector :: Selector
semanticSegmentationSkinMatteSelector = mkSelector "semanticSegmentationSkinMatte"

-- | @Selector@ for @semanticSegmentationHairMatte@
semanticSegmentationHairMatteSelector :: Selector
semanticSegmentationHairMatteSelector = mkSelector "semanticSegmentationHairMatte"

-- | @Selector@ for @semanticSegmentationGlassesMatte@
semanticSegmentationGlassesMatteSelector :: Selector
semanticSegmentationGlassesMatteSelector = mkSelector "semanticSegmentationGlassesMatte"

-- | @Selector@ for @semanticSegmentationSkyMatte@
semanticSegmentationSkyMatteSelector :: Selector
semanticSegmentationSkyMatteSelector = mkSelector "semanticSegmentationSkyMatte"

-- | @Selector@ for @semanticSegmentationTeethMatte@
semanticSegmentationTeethMatteSelector :: Selector
semanticSegmentationTeethMatteSelector = mkSelector "semanticSegmentationTeethMatte"

