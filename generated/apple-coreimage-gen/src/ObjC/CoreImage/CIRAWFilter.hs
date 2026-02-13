{-# LANGUAGE DataKinds #-}
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
  , baselineExposureSelector
  , boostAmountSelector
  , boostShadowAmountSelector
  , colorNoiseReductionAmountSelector
  , colorNoiseReductionSupportedSelector
  , contrastAmountSelector
  , contrastSupportedSelector
  , decoderVersionSelector
  , detailAmountSelector
  , detailSupportedSelector
  , draftModeEnabledSelector
  , exposureSelector
  , extendedDynamicRangeAmountSelector
  , filterWithCVPixelBuffer_propertiesSelector
  , filterWithImageData_identifierHintSelector
  , filterWithImageURLSelector
  , gamutMappingEnabledSelector
  , highlightRecoveryEnabledSelector
  , highlightRecoverySupportedSelector
  , lensCorrectionEnabledSelector
  , lensCorrectionSupportedSelector
  , linearSpaceFilterSelector
  , localToneMapAmountSelector
  , localToneMapSupportedSelector
  , luminanceNoiseReductionAmountSelector
  , luminanceNoiseReductionSupportedSelector
  , moireReductionAmountSelector
  , moireReductionSupportedSelector
  , neutralTemperatureSelector
  , neutralTintSelector
  , orientationSelector
  , portraitEffectsMatteSelector
  , previewImageSelector
  , propertiesSelector
  , scaleFactorSelector
  , semanticSegmentationGlassesMatteSelector
  , semanticSegmentationHairMatteSelector
  , semanticSegmentationSkinMatteSelector
  , semanticSegmentationSkyMatteSelector
  , semanticSegmentationTeethMatteSelector
  , setBaselineExposureSelector
  , setBoostAmountSelector
  , setBoostShadowAmountSelector
  , setColorNoiseReductionAmountSelector
  , setContrastAmountSelector
  , setDecoderVersionSelector
  , setDetailAmountSelector
  , setDraftModeEnabledSelector
  , setExposureSelector
  , setExtendedDynamicRangeAmountSelector
  , setGamutMappingEnabledSelector
  , setHighlightRecoveryEnabledSelector
  , setLensCorrectionEnabledSelector
  , setLinearSpaceFilterSelector
  , setLocalToneMapAmountSelector
  , setLuminanceNoiseReductionAmountSelector
  , setMoireReductionAmountSelector
  , setNeutralTemperatureSelector
  , setNeutralTintSelector
  , setOrientationSelector
  , setScaleFactorSelector
  , setShadowBiasSelector
  , setSharpnessAmountSelector
  , shadowBiasSelector
  , sharpnessAmountSelector
  , sharpnessSupportedSelector
  , supportedCameraModelsSelector
  , supportedDecoderVersionsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ filterWithImageURL:@
filterWithImageURL :: IsNSURL url => url -> IO (Id CIRAWFilter)
filterWithImageURL url =
  do
    cls' <- getRequiredClass "CIRAWFilter"
    sendClassMessage cls' filterWithImageURLSelector (toNSURL url)

-- | @+ filterWithImageData:identifierHint:@
filterWithImageData_identifierHint :: (IsNSData data_, IsNSString identifierHint) => data_ -> identifierHint -> IO (Id CIRAWFilter)
filterWithImageData_identifierHint data_ identifierHint =
  do
    cls' <- getRequiredClass "CIRAWFilter"
    sendClassMessage cls' filterWithImageData_identifierHintSelector (toNSData data_) (toNSString identifierHint)

-- | @+ filterWithCVPixelBuffer:properties:@
filterWithCVPixelBuffer_properties :: IsNSDictionary properties => Ptr () -> properties -> IO (Id CIRAWFilter)
filterWithCVPixelBuffer_properties buffer properties =
  do
    cls' <- getRequiredClass "CIRAWFilter"
    sendClassMessage cls' filterWithCVPixelBuffer_propertiesSelector buffer (toNSDictionary properties)

-- | @+ supportedCameraModels@
supportedCameraModels :: IO (Id NSArray)
supportedCameraModels  =
  do
    cls' <- getRequiredClass "CIRAWFilter"
    sendClassMessage cls' supportedCameraModelsSelector

-- | @- supportedDecoderVersions@
supportedDecoderVersions :: IsCIRAWFilter cirawFilter => cirawFilter -> IO (Id NSArray)
supportedDecoderVersions cirawFilter =
  sendMessage cirawFilter supportedDecoderVersionsSelector

-- | @- properties@
properties :: IsCIRAWFilter cirawFilter => cirawFilter -> IO (Id NSDictionary)
properties cirawFilter =
  sendMessage cirawFilter propertiesSelector

-- | @- orientation@
orientation :: IsCIRAWFilter cirawFilter => cirawFilter -> IO CInt
orientation cirawFilter =
  sendMessage cirawFilter orientationSelector

-- | @- setOrientation:@
setOrientation :: IsCIRAWFilter cirawFilter => cirawFilter -> CInt -> IO ()
setOrientation cirawFilter value =
  sendMessage cirawFilter setOrientationSelector value

-- | @- draftModeEnabled@
draftModeEnabled :: IsCIRAWFilter cirawFilter => cirawFilter -> IO Bool
draftModeEnabled cirawFilter =
  sendMessage cirawFilter draftModeEnabledSelector

-- | @- setDraftModeEnabled:@
setDraftModeEnabled :: IsCIRAWFilter cirawFilter => cirawFilter -> Bool -> IO ()
setDraftModeEnabled cirawFilter value =
  sendMessage cirawFilter setDraftModeEnabledSelector value

-- | @- decoderVersion@
decoderVersion :: IsCIRAWFilter cirawFilter => cirawFilter -> IO (Id NSString)
decoderVersion cirawFilter =
  sendMessage cirawFilter decoderVersionSelector

-- | @- setDecoderVersion:@
setDecoderVersion :: (IsCIRAWFilter cirawFilter, IsNSString value) => cirawFilter -> value -> IO ()
setDecoderVersion cirawFilter value =
  sendMessage cirawFilter setDecoderVersionSelector (toNSString value)

-- | @- scaleFactor@
scaleFactor :: IsCIRAWFilter cirawFilter => cirawFilter -> IO CFloat
scaleFactor cirawFilter =
  sendMessage cirawFilter scaleFactorSelector

-- | @- setScaleFactor:@
setScaleFactor :: IsCIRAWFilter cirawFilter => cirawFilter -> CFloat -> IO ()
setScaleFactor cirawFilter value =
  sendMessage cirawFilter setScaleFactorSelector value

-- | @- exposure@
exposure :: IsCIRAWFilter cirawFilter => cirawFilter -> IO CFloat
exposure cirawFilter =
  sendMessage cirawFilter exposureSelector

-- | @- setExposure:@
setExposure :: IsCIRAWFilter cirawFilter => cirawFilter -> CFloat -> IO ()
setExposure cirawFilter value =
  sendMessage cirawFilter setExposureSelector value

-- | @- baselineExposure@
baselineExposure :: IsCIRAWFilter cirawFilter => cirawFilter -> IO CFloat
baselineExposure cirawFilter =
  sendMessage cirawFilter baselineExposureSelector

-- | @- setBaselineExposure:@
setBaselineExposure :: IsCIRAWFilter cirawFilter => cirawFilter -> CFloat -> IO ()
setBaselineExposure cirawFilter value =
  sendMessage cirawFilter setBaselineExposureSelector value

-- | @- shadowBias@
shadowBias :: IsCIRAWFilter cirawFilter => cirawFilter -> IO CFloat
shadowBias cirawFilter =
  sendMessage cirawFilter shadowBiasSelector

-- | @- setShadowBias:@
setShadowBias :: IsCIRAWFilter cirawFilter => cirawFilter -> CFloat -> IO ()
setShadowBias cirawFilter value =
  sendMessage cirawFilter setShadowBiasSelector value

-- | @- boostAmount@
boostAmount :: IsCIRAWFilter cirawFilter => cirawFilter -> IO CFloat
boostAmount cirawFilter =
  sendMessage cirawFilter boostAmountSelector

-- | @- setBoostAmount:@
setBoostAmount :: IsCIRAWFilter cirawFilter => cirawFilter -> CFloat -> IO ()
setBoostAmount cirawFilter value =
  sendMessage cirawFilter setBoostAmountSelector value

-- | @- boostShadowAmount@
boostShadowAmount :: IsCIRAWFilter cirawFilter => cirawFilter -> IO CFloat
boostShadowAmount cirawFilter =
  sendMessage cirawFilter boostShadowAmountSelector

-- | @- setBoostShadowAmount:@
setBoostShadowAmount :: IsCIRAWFilter cirawFilter => cirawFilter -> CFloat -> IO ()
setBoostShadowAmount cirawFilter value =
  sendMessage cirawFilter setBoostShadowAmountSelector value

-- | @- highlightRecoverySupported@
highlightRecoverySupported :: IsCIRAWFilter cirawFilter => cirawFilter -> IO Bool
highlightRecoverySupported cirawFilter =
  sendMessage cirawFilter highlightRecoverySupportedSelector

-- | @- highlightRecoveryEnabled@
highlightRecoveryEnabled :: IsCIRAWFilter cirawFilter => cirawFilter -> IO Bool
highlightRecoveryEnabled cirawFilter =
  sendMessage cirawFilter highlightRecoveryEnabledSelector

-- | @- setHighlightRecoveryEnabled:@
setHighlightRecoveryEnabled :: IsCIRAWFilter cirawFilter => cirawFilter -> Bool -> IO ()
setHighlightRecoveryEnabled cirawFilter value =
  sendMessage cirawFilter setHighlightRecoveryEnabledSelector value

-- | @- gamutMappingEnabled@
gamutMappingEnabled :: IsCIRAWFilter cirawFilter => cirawFilter -> IO Bool
gamutMappingEnabled cirawFilter =
  sendMessage cirawFilter gamutMappingEnabledSelector

-- | @- setGamutMappingEnabled:@
setGamutMappingEnabled :: IsCIRAWFilter cirawFilter => cirawFilter -> Bool -> IO ()
setGamutMappingEnabled cirawFilter value =
  sendMessage cirawFilter setGamutMappingEnabledSelector value

-- | @- lensCorrectionSupported@
lensCorrectionSupported :: IsCIRAWFilter cirawFilter => cirawFilter -> IO Bool
lensCorrectionSupported cirawFilter =
  sendMessage cirawFilter lensCorrectionSupportedSelector

-- | @- lensCorrectionEnabled@
lensCorrectionEnabled :: IsCIRAWFilter cirawFilter => cirawFilter -> IO Bool
lensCorrectionEnabled cirawFilter =
  sendMessage cirawFilter lensCorrectionEnabledSelector

-- | @- setLensCorrectionEnabled:@
setLensCorrectionEnabled :: IsCIRAWFilter cirawFilter => cirawFilter -> Bool -> IO ()
setLensCorrectionEnabled cirawFilter value =
  sendMessage cirawFilter setLensCorrectionEnabledSelector value

-- | @- luminanceNoiseReductionSupported@
luminanceNoiseReductionSupported :: IsCIRAWFilter cirawFilter => cirawFilter -> IO Bool
luminanceNoiseReductionSupported cirawFilter =
  sendMessage cirawFilter luminanceNoiseReductionSupportedSelector

-- | @- luminanceNoiseReductionAmount@
luminanceNoiseReductionAmount :: IsCIRAWFilter cirawFilter => cirawFilter -> IO CFloat
luminanceNoiseReductionAmount cirawFilter =
  sendMessage cirawFilter luminanceNoiseReductionAmountSelector

-- | @- setLuminanceNoiseReductionAmount:@
setLuminanceNoiseReductionAmount :: IsCIRAWFilter cirawFilter => cirawFilter -> CFloat -> IO ()
setLuminanceNoiseReductionAmount cirawFilter value =
  sendMessage cirawFilter setLuminanceNoiseReductionAmountSelector value

-- | @- colorNoiseReductionSupported@
colorNoiseReductionSupported :: IsCIRAWFilter cirawFilter => cirawFilter -> IO Bool
colorNoiseReductionSupported cirawFilter =
  sendMessage cirawFilter colorNoiseReductionSupportedSelector

-- | @- colorNoiseReductionAmount@
colorNoiseReductionAmount :: IsCIRAWFilter cirawFilter => cirawFilter -> IO CFloat
colorNoiseReductionAmount cirawFilter =
  sendMessage cirawFilter colorNoiseReductionAmountSelector

-- | @- setColorNoiseReductionAmount:@
setColorNoiseReductionAmount :: IsCIRAWFilter cirawFilter => cirawFilter -> CFloat -> IO ()
setColorNoiseReductionAmount cirawFilter value =
  sendMessage cirawFilter setColorNoiseReductionAmountSelector value

-- | @- sharpnessSupported@
sharpnessSupported :: IsCIRAWFilter cirawFilter => cirawFilter -> IO Bool
sharpnessSupported cirawFilter =
  sendMessage cirawFilter sharpnessSupportedSelector

-- | @- sharpnessAmount@
sharpnessAmount :: IsCIRAWFilter cirawFilter => cirawFilter -> IO CFloat
sharpnessAmount cirawFilter =
  sendMessage cirawFilter sharpnessAmountSelector

-- | @- setSharpnessAmount:@
setSharpnessAmount :: IsCIRAWFilter cirawFilter => cirawFilter -> CFloat -> IO ()
setSharpnessAmount cirawFilter value =
  sendMessage cirawFilter setSharpnessAmountSelector value

-- | @- contrastSupported@
contrastSupported :: IsCIRAWFilter cirawFilter => cirawFilter -> IO Bool
contrastSupported cirawFilter =
  sendMessage cirawFilter contrastSupportedSelector

-- | @- contrastAmount@
contrastAmount :: IsCIRAWFilter cirawFilter => cirawFilter -> IO CFloat
contrastAmount cirawFilter =
  sendMessage cirawFilter contrastAmountSelector

-- | @- setContrastAmount:@
setContrastAmount :: IsCIRAWFilter cirawFilter => cirawFilter -> CFloat -> IO ()
setContrastAmount cirawFilter value =
  sendMessage cirawFilter setContrastAmountSelector value

-- | @- detailSupported@
detailSupported :: IsCIRAWFilter cirawFilter => cirawFilter -> IO Bool
detailSupported cirawFilter =
  sendMessage cirawFilter detailSupportedSelector

-- | @- detailAmount@
detailAmount :: IsCIRAWFilter cirawFilter => cirawFilter -> IO CFloat
detailAmount cirawFilter =
  sendMessage cirawFilter detailAmountSelector

-- | @- setDetailAmount:@
setDetailAmount :: IsCIRAWFilter cirawFilter => cirawFilter -> CFloat -> IO ()
setDetailAmount cirawFilter value =
  sendMessage cirawFilter setDetailAmountSelector value

-- | @- moireReductionSupported@
moireReductionSupported :: IsCIRAWFilter cirawFilter => cirawFilter -> IO Bool
moireReductionSupported cirawFilter =
  sendMessage cirawFilter moireReductionSupportedSelector

-- | @- moireReductionAmount@
moireReductionAmount :: IsCIRAWFilter cirawFilter => cirawFilter -> IO CFloat
moireReductionAmount cirawFilter =
  sendMessage cirawFilter moireReductionAmountSelector

-- | @- setMoireReductionAmount:@
setMoireReductionAmount :: IsCIRAWFilter cirawFilter => cirawFilter -> CFloat -> IO ()
setMoireReductionAmount cirawFilter value =
  sendMessage cirawFilter setMoireReductionAmountSelector value

-- | @- localToneMapSupported@
localToneMapSupported :: IsCIRAWFilter cirawFilter => cirawFilter -> IO Bool
localToneMapSupported cirawFilter =
  sendMessage cirawFilter localToneMapSupportedSelector

-- | @- localToneMapAmount@
localToneMapAmount :: IsCIRAWFilter cirawFilter => cirawFilter -> IO CFloat
localToneMapAmount cirawFilter =
  sendMessage cirawFilter localToneMapAmountSelector

-- | @- setLocalToneMapAmount:@
setLocalToneMapAmount :: IsCIRAWFilter cirawFilter => cirawFilter -> CFloat -> IO ()
setLocalToneMapAmount cirawFilter value =
  sendMessage cirawFilter setLocalToneMapAmountSelector value

-- | @- extendedDynamicRangeAmount@
extendedDynamicRangeAmount :: IsCIRAWFilter cirawFilter => cirawFilter -> IO CFloat
extendedDynamicRangeAmount cirawFilter =
  sendMessage cirawFilter extendedDynamicRangeAmountSelector

-- | @- setExtendedDynamicRangeAmount:@
setExtendedDynamicRangeAmount :: IsCIRAWFilter cirawFilter => cirawFilter -> CFloat -> IO ()
setExtendedDynamicRangeAmount cirawFilter value =
  sendMessage cirawFilter setExtendedDynamicRangeAmountSelector value

-- | @- neutralTemperature@
neutralTemperature :: IsCIRAWFilter cirawFilter => cirawFilter -> IO CFloat
neutralTemperature cirawFilter =
  sendMessage cirawFilter neutralTemperatureSelector

-- | @- setNeutralTemperature:@
setNeutralTemperature :: IsCIRAWFilter cirawFilter => cirawFilter -> CFloat -> IO ()
setNeutralTemperature cirawFilter value =
  sendMessage cirawFilter setNeutralTemperatureSelector value

-- | @- neutralTint@
neutralTint :: IsCIRAWFilter cirawFilter => cirawFilter -> IO CFloat
neutralTint cirawFilter =
  sendMessage cirawFilter neutralTintSelector

-- | @- setNeutralTint:@
setNeutralTint :: IsCIRAWFilter cirawFilter => cirawFilter -> CFloat -> IO ()
setNeutralTint cirawFilter value =
  sendMessage cirawFilter setNeutralTintSelector value

-- | @- linearSpaceFilter@
linearSpaceFilter :: IsCIRAWFilter cirawFilter => cirawFilter -> IO (Id CIFilter)
linearSpaceFilter cirawFilter =
  sendMessage cirawFilter linearSpaceFilterSelector

-- | @- setLinearSpaceFilter:@
setLinearSpaceFilter :: (IsCIRAWFilter cirawFilter, IsCIFilter value) => cirawFilter -> value -> IO ()
setLinearSpaceFilter cirawFilter value =
  sendMessage cirawFilter setLinearSpaceFilterSelector (toCIFilter value)

-- | @- previewImage@
previewImage :: IsCIRAWFilter cirawFilter => cirawFilter -> IO (Id CIImage)
previewImage cirawFilter =
  sendMessage cirawFilter previewImageSelector

-- | @- portraitEffectsMatte@
portraitEffectsMatte :: IsCIRAWFilter cirawFilter => cirawFilter -> IO (Id CIImage)
portraitEffectsMatte cirawFilter =
  sendMessage cirawFilter portraitEffectsMatteSelector

-- | @- semanticSegmentationSkinMatte@
semanticSegmentationSkinMatte :: IsCIRAWFilter cirawFilter => cirawFilter -> IO (Id CIImage)
semanticSegmentationSkinMatte cirawFilter =
  sendMessage cirawFilter semanticSegmentationSkinMatteSelector

-- | @- semanticSegmentationHairMatte@
semanticSegmentationHairMatte :: IsCIRAWFilter cirawFilter => cirawFilter -> IO (Id CIImage)
semanticSegmentationHairMatte cirawFilter =
  sendMessage cirawFilter semanticSegmentationHairMatteSelector

-- | @- semanticSegmentationGlassesMatte@
semanticSegmentationGlassesMatte :: IsCIRAWFilter cirawFilter => cirawFilter -> IO (Id CIImage)
semanticSegmentationGlassesMatte cirawFilter =
  sendMessage cirawFilter semanticSegmentationGlassesMatteSelector

-- | @- semanticSegmentationSkyMatte@
semanticSegmentationSkyMatte :: IsCIRAWFilter cirawFilter => cirawFilter -> IO (Id CIImage)
semanticSegmentationSkyMatte cirawFilter =
  sendMessage cirawFilter semanticSegmentationSkyMatteSelector

-- | @- semanticSegmentationTeethMatte@
semanticSegmentationTeethMatte :: IsCIRAWFilter cirawFilter => cirawFilter -> IO (Id CIImage)
semanticSegmentationTeethMatte cirawFilter =
  sendMessage cirawFilter semanticSegmentationTeethMatteSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @filterWithImageURL:@
filterWithImageURLSelector :: Selector '[Id NSURL] (Id CIRAWFilter)
filterWithImageURLSelector = mkSelector "filterWithImageURL:"

-- | @Selector@ for @filterWithImageData:identifierHint:@
filterWithImageData_identifierHintSelector :: Selector '[Id NSData, Id NSString] (Id CIRAWFilter)
filterWithImageData_identifierHintSelector = mkSelector "filterWithImageData:identifierHint:"

-- | @Selector@ for @filterWithCVPixelBuffer:properties:@
filterWithCVPixelBuffer_propertiesSelector :: Selector '[Ptr (), Id NSDictionary] (Id CIRAWFilter)
filterWithCVPixelBuffer_propertiesSelector = mkSelector "filterWithCVPixelBuffer:properties:"

-- | @Selector@ for @supportedCameraModels@
supportedCameraModelsSelector :: Selector '[] (Id NSArray)
supportedCameraModelsSelector = mkSelector "supportedCameraModels"

-- | @Selector@ for @supportedDecoderVersions@
supportedDecoderVersionsSelector :: Selector '[] (Id NSArray)
supportedDecoderVersionsSelector = mkSelector "supportedDecoderVersions"

-- | @Selector@ for @properties@
propertiesSelector :: Selector '[] (Id NSDictionary)
propertiesSelector = mkSelector "properties"

-- | @Selector@ for @orientation@
orientationSelector :: Selector '[] CInt
orientationSelector = mkSelector "orientation"

-- | @Selector@ for @setOrientation:@
setOrientationSelector :: Selector '[CInt] ()
setOrientationSelector = mkSelector "setOrientation:"

-- | @Selector@ for @draftModeEnabled@
draftModeEnabledSelector :: Selector '[] Bool
draftModeEnabledSelector = mkSelector "draftModeEnabled"

-- | @Selector@ for @setDraftModeEnabled:@
setDraftModeEnabledSelector :: Selector '[Bool] ()
setDraftModeEnabledSelector = mkSelector "setDraftModeEnabled:"

-- | @Selector@ for @decoderVersion@
decoderVersionSelector :: Selector '[] (Id NSString)
decoderVersionSelector = mkSelector "decoderVersion"

-- | @Selector@ for @setDecoderVersion:@
setDecoderVersionSelector :: Selector '[Id NSString] ()
setDecoderVersionSelector = mkSelector "setDecoderVersion:"

-- | @Selector@ for @scaleFactor@
scaleFactorSelector :: Selector '[] CFloat
scaleFactorSelector = mkSelector "scaleFactor"

-- | @Selector@ for @setScaleFactor:@
setScaleFactorSelector :: Selector '[CFloat] ()
setScaleFactorSelector = mkSelector "setScaleFactor:"

-- | @Selector@ for @exposure@
exposureSelector :: Selector '[] CFloat
exposureSelector = mkSelector "exposure"

-- | @Selector@ for @setExposure:@
setExposureSelector :: Selector '[CFloat] ()
setExposureSelector = mkSelector "setExposure:"

-- | @Selector@ for @baselineExposure@
baselineExposureSelector :: Selector '[] CFloat
baselineExposureSelector = mkSelector "baselineExposure"

-- | @Selector@ for @setBaselineExposure:@
setBaselineExposureSelector :: Selector '[CFloat] ()
setBaselineExposureSelector = mkSelector "setBaselineExposure:"

-- | @Selector@ for @shadowBias@
shadowBiasSelector :: Selector '[] CFloat
shadowBiasSelector = mkSelector "shadowBias"

-- | @Selector@ for @setShadowBias:@
setShadowBiasSelector :: Selector '[CFloat] ()
setShadowBiasSelector = mkSelector "setShadowBias:"

-- | @Selector@ for @boostAmount@
boostAmountSelector :: Selector '[] CFloat
boostAmountSelector = mkSelector "boostAmount"

-- | @Selector@ for @setBoostAmount:@
setBoostAmountSelector :: Selector '[CFloat] ()
setBoostAmountSelector = mkSelector "setBoostAmount:"

-- | @Selector@ for @boostShadowAmount@
boostShadowAmountSelector :: Selector '[] CFloat
boostShadowAmountSelector = mkSelector "boostShadowAmount"

-- | @Selector@ for @setBoostShadowAmount:@
setBoostShadowAmountSelector :: Selector '[CFloat] ()
setBoostShadowAmountSelector = mkSelector "setBoostShadowAmount:"

-- | @Selector@ for @highlightRecoverySupported@
highlightRecoverySupportedSelector :: Selector '[] Bool
highlightRecoverySupportedSelector = mkSelector "highlightRecoverySupported"

-- | @Selector@ for @highlightRecoveryEnabled@
highlightRecoveryEnabledSelector :: Selector '[] Bool
highlightRecoveryEnabledSelector = mkSelector "highlightRecoveryEnabled"

-- | @Selector@ for @setHighlightRecoveryEnabled:@
setHighlightRecoveryEnabledSelector :: Selector '[Bool] ()
setHighlightRecoveryEnabledSelector = mkSelector "setHighlightRecoveryEnabled:"

-- | @Selector@ for @gamutMappingEnabled@
gamutMappingEnabledSelector :: Selector '[] Bool
gamutMappingEnabledSelector = mkSelector "gamutMappingEnabled"

-- | @Selector@ for @setGamutMappingEnabled:@
setGamutMappingEnabledSelector :: Selector '[Bool] ()
setGamutMappingEnabledSelector = mkSelector "setGamutMappingEnabled:"

-- | @Selector@ for @lensCorrectionSupported@
lensCorrectionSupportedSelector :: Selector '[] Bool
lensCorrectionSupportedSelector = mkSelector "lensCorrectionSupported"

-- | @Selector@ for @lensCorrectionEnabled@
lensCorrectionEnabledSelector :: Selector '[] Bool
lensCorrectionEnabledSelector = mkSelector "lensCorrectionEnabled"

-- | @Selector@ for @setLensCorrectionEnabled:@
setLensCorrectionEnabledSelector :: Selector '[Bool] ()
setLensCorrectionEnabledSelector = mkSelector "setLensCorrectionEnabled:"

-- | @Selector@ for @luminanceNoiseReductionSupported@
luminanceNoiseReductionSupportedSelector :: Selector '[] Bool
luminanceNoiseReductionSupportedSelector = mkSelector "luminanceNoiseReductionSupported"

-- | @Selector@ for @luminanceNoiseReductionAmount@
luminanceNoiseReductionAmountSelector :: Selector '[] CFloat
luminanceNoiseReductionAmountSelector = mkSelector "luminanceNoiseReductionAmount"

-- | @Selector@ for @setLuminanceNoiseReductionAmount:@
setLuminanceNoiseReductionAmountSelector :: Selector '[CFloat] ()
setLuminanceNoiseReductionAmountSelector = mkSelector "setLuminanceNoiseReductionAmount:"

-- | @Selector@ for @colorNoiseReductionSupported@
colorNoiseReductionSupportedSelector :: Selector '[] Bool
colorNoiseReductionSupportedSelector = mkSelector "colorNoiseReductionSupported"

-- | @Selector@ for @colorNoiseReductionAmount@
colorNoiseReductionAmountSelector :: Selector '[] CFloat
colorNoiseReductionAmountSelector = mkSelector "colorNoiseReductionAmount"

-- | @Selector@ for @setColorNoiseReductionAmount:@
setColorNoiseReductionAmountSelector :: Selector '[CFloat] ()
setColorNoiseReductionAmountSelector = mkSelector "setColorNoiseReductionAmount:"

-- | @Selector@ for @sharpnessSupported@
sharpnessSupportedSelector :: Selector '[] Bool
sharpnessSupportedSelector = mkSelector "sharpnessSupported"

-- | @Selector@ for @sharpnessAmount@
sharpnessAmountSelector :: Selector '[] CFloat
sharpnessAmountSelector = mkSelector "sharpnessAmount"

-- | @Selector@ for @setSharpnessAmount:@
setSharpnessAmountSelector :: Selector '[CFloat] ()
setSharpnessAmountSelector = mkSelector "setSharpnessAmount:"

-- | @Selector@ for @contrastSupported@
contrastSupportedSelector :: Selector '[] Bool
contrastSupportedSelector = mkSelector "contrastSupported"

-- | @Selector@ for @contrastAmount@
contrastAmountSelector :: Selector '[] CFloat
contrastAmountSelector = mkSelector "contrastAmount"

-- | @Selector@ for @setContrastAmount:@
setContrastAmountSelector :: Selector '[CFloat] ()
setContrastAmountSelector = mkSelector "setContrastAmount:"

-- | @Selector@ for @detailSupported@
detailSupportedSelector :: Selector '[] Bool
detailSupportedSelector = mkSelector "detailSupported"

-- | @Selector@ for @detailAmount@
detailAmountSelector :: Selector '[] CFloat
detailAmountSelector = mkSelector "detailAmount"

-- | @Selector@ for @setDetailAmount:@
setDetailAmountSelector :: Selector '[CFloat] ()
setDetailAmountSelector = mkSelector "setDetailAmount:"

-- | @Selector@ for @moireReductionSupported@
moireReductionSupportedSelector :: Selector '[] Bool
moireReductionSupportedSelector = mkSelector "moireReductionSupported"

-- | @Selector@ for @moireReductionAmount@
moireReductionAmountSelector :: Selector '[] CFloat
moireReductionAmountSelector = mkSelector "moireReductionAmount"

-- | @Selector@ for @setMoireReductionAmount:@
setMoireReductionAmountSelector :: Selector '[CFloat] ()
setMoireReductionAmountSelector = mkSelector "setMoireReductionAmount:"

-- | @Selector@ for @localToneMapSupported@
localToneMapSupportedSelector :: Selector '[] Bool
localToneMapSupportedSelector = mkSelector "localToneMapSupported"

-- | @Selector@ for @localToneMapAmount@
localToneMapAmountSelector :: Selector '[] CFloat
localToneMapAmountSelector = mkSelector "localToneMapAmount"

-- | @Selector@ for @setLocalToneMapAmount:@
setLocalToneMapAmountSelector :: Selector '[CFloat] ()
setLocalToneMapAmountSelector = mkSelector "setLocalToneMapAmount:"

-- | @Selector@ for @extendedDynamicRangeAmount@
extendedDynamicRangeAmountSelector :: Selector '[] CFloat
extendedDynamicRangeAmountSelector = mkSelector "extendedDynamicRangeAmount"

-- | @Selector@ for @setExtendedDynamicRangeAmount:@
setExtendedDynamicRangeAmountSelector :: Selector '[CFloat] ()
setExtendedDynamicRangeAmountSelector = mkSelector "setExtendedDynamicRangeAmount:"

-- | @Selector@ for @neutralTemperature@
neutralTemperatureSelector :: Selector '[] CFloat
neutralTemperatureSelector = mkSelector "neutralTemperature"

-- | @Selector@ for @setNeutralTemperature:@
setNeutralTemperatureSelector :: Selector '[CFloat] ()
setNeutralTemperatureSelector = mkSelector "setNeutralTemperature:"

-- | @Selector@ for @neutralTint@
neutralTintSelector :: Selector '[] CFloat
neutralTintSelector = mkSelector "neutralTint"

-- | @Selector@ for @setNeutralTint:@
setNeutralTintSelector :: Selector '[CFloat] ()
setNeutralTintSelector = mkSelector "setNeutralTint:"

-- | @Selector@ for @linearSpaceFilter@
linearSpaceFilterSelector :: Selector '[] (Id CIFilter)
linearSpaceFilterSelector = mkSelector "linearSpaceFilter"

-- | @Selector@ for @setLinearSpaceFilter:@
setLinearSpaceFilterSelector :: Selector '[Id CIFilter] ()
setLinearSpaceFilterSelector = mkSelector "setLinearSpaceFilter:"

-- | @Selector@ for @previewImage@
previewImageSelector :: Selector '[] (Id CIImage)
previewImageSelector = mkSelector "previewImage"

-- | @Selector@ for @portraitEffectsMatte@
portraitEffectsMatteSelector :: Selector '[] (Id CIImage)
portraitEffectsMatteSelector = mkSelector "portraitEffectsMatte"

-- | @Selector@ for @semanticSegmentationSkinMatte@
semanticSegmentationSkinMatteSelector :: Selector '[] (Id CIImage)
semanticSegmentationSkinMatteSelector = mkSelector "semanticSegmentationSkinMatte"

-- | @Selector@ for @semanticSegmentationHairMatte@
semanticSegmentationHairMatteSelector :: Selector '[] (Id CIImage)
semanticSegmentationHairMatteSelector = mkSelector "semanticSegmentationHairMatte"

-- | @Selector@ for @semanticSegmentationGlassesMatte@
semanticSegmentationGlassesMatteSelector :: Selector '[] (Id CIImage)
semanticSegmentationGlassesMatteSelector = mkSelector "semanticSegmentationGlassesMatte"

-- | @Selector@ for @semanticSegmentationSkyMatte@
semanticSegmentationSkyMatteSelector :: Selector '[] (Id CIImage)
semanticSegmentationSkyMatteSelector = mkSelector "semanticSegmentationSkyMatte"

-- | @Selector@ for @semanticSegmentationTeethMatte@
semanticSegmentationTeethMatteSelector :: Selector '[] (Id CIImage)
semanticSegmentationTeethMatteSelector = mkSelector "semanticSegmentationTeethMatte"

