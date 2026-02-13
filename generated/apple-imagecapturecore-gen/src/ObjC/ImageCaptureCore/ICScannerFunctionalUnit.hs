{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | ICScannerFunctionalUnit
--
-- ICScannerFunctionalUnit is an abstract class that represents a scanner functiona unit. ImageCaptureCore defines three concrete subclasses of ICScannerFunctionalUnit: ICScannerFunctionalUnitFlatbed, ICScannerFunctionalUnitPositiveTransparency, ICScannerFunctionalUnitNegativeTransparency and ICScannerFunctionalUnitDocumentFeeder. ICScannerDevice creates instances of these concrete subclasses.
--
-- Generated bindings for @ICScannerFunctionalUnit@.
module ObjC.ImageCaptureCore.ICScannerFunctionalUnit
  ( ICScannerFunctionalUnit
  , IsICScannerFunctionalUnit(..)
  , type_
  , pixelDataType
  , setPixelDataType
  , supportedBitDepths
  , bitDepth
  , setBitDepth
  , supportedMeasurementUnits
  , measurementUnit
  , setMeasurementUnit
  , supportedResolutions
  , preferredResolutions
  , resolution
  , setResolution
  , nativeXResolution
  , nativeYResolution
  , supportedScaleFactors
  , preferredScaleFactors
  , scaleFactor
  , setScaleFactor
  , templates
  , vendorFeatures
  , physicalSize
  , scanArea
  , setScanArea
  , scanAreaOrientation
  , setScanAreaOrientation
  , acceptsThresholdForBlackAndWhiteScanning
  , usesThresholdForBlackAndWhiteScanning
  , setUsesThresholdForBlackAndWhiteScanning
  , defaultThresholdForBlackAndWhiteScanning
  , thresholdForBlackAndWhiteScanning
  , setThresholdForBlackAndWhiteScanning
  , state
  , scanInProgress
  , scanProgressPercentDone
  , canPerformOverviewScan
  , overviewScanInProgress
  , overviewImage
  , overviewResolution
  , setOverviewResolution
  , acceptsThresholdForBlackAndWhiteScanningSelector
  , bitDepthSelector
  , canPerformOverviewScanSelector
  , defaultThresholdForBlackAndWhiteScanningSelector
  , measurementUnitSelector
  , nativeXResolutionSelector
  , nativeYResolutionSelector
  , overviewImageSelector
  , overviewResolutionSelector
  , overviewScanInProgressSelector
  , physicalSizeSelector
  , pixelDataTypeSelector
  , preferredResolutionsSelector
  , preferredScaleFactorsSelector
  , resolutionSelector
  , scaleFactorSelector
  , scanAreaOrientationSelector
  , scanAreaSelector
  , scanInProgressSelector
  , scanProgressPercentDoneSelector
  , setBitDepthSelector
  , setMeasurementUnitSelector
  , setOverviewResolutionSelector
  , setPixelDataTypeSelector
  , setResolutionSelector
  , setScaleFactorSelector
  , setScanAreaOrientationSelector
  , setScanAreaSelector
  , setThresholdForBlackAndWhiteScanningSelector
  , setUsesThresholdForBlackAndWhiteScanningSelector
  , stateSelector
  , supportedBitDepthsSelector
  , supportedMeasurementUnitsSelector
  , supportedResolutionsSelector
  , supportedScaleFactorsSelector
  , templatesSelector
  , thresholdForBlackAndWhiteScanningSelector
  , typeSelector
  , usesThresholdForBlackAndWhiteScanningSelector
  , vendorFeaturesSelector

  -- * Enum types
  , ICEXIFOrientationType(ICEXIFOrientationType)
  , pattern ICEXIFOrientation1
  , pattern ICEXIFOrientation2
  , pattern ICEXIFOrientation3
  , pattern ICEXIFOrientation4
  , pattern ICEXIFOrientation5
  , pattern ICEXIFOrientation6
  , pattern ICEXIFOrientation7
  , pattern ICEXIFOrientation8
  , ICScannerBitDepth(ICScannerBitDepth)
  , pattern ICScannerBitDepth1Bit
  , pattern ICScannerBitDepth8Bits
  , pattern ICScannerBitDepth16Bits
  , ICScannerFunctionalUnitState(ICScannerFunctionalUnitState)
  , pattern ICScannerFunctionalUnitStateReady
  , pattern ICScannerFunctionalUnitStateScanInProgress
  , pattern ICScannerFunctionalUnitStateOverviewScanInProgress
  , ICScannerFunctionalUnitType(ICScannerFunctionalUnitType)
  , pattern ICScannerFunctionalUnitTypeFlatbed
  , pattern ICScannerFunctionalUnitTypePositiveTransparency
  , pattern ICScannerFunctionalUnitTypeNegativeTransparency
  , pattern ICScannerFunctionalUnitTypeDocumentFeeder
  , ICScannerMeasurementUnit(ICScannerMeasurementUnit)
  , pattern ICScannerMeasurementUnitInches
  , pattern ICScannerMeasurementUnitCentimeters
  , pattern ICScannerMeasurementUnitPicas
  , pattern ICScannerMeasurementUnitPoints
  , pattern ICScannerMeasurementUnitTwips
  , pattern ICScannerMeasurementUnitPixels
  , ICScannerPixelDataType(ICScannerPixelDataType)
  , pattern ICScannerPixelDataTypeBW
  , pattern ICScannerPixelDataTypeGray
  , pattern ICScannerPixelDataTypeRGB
  , pattern ICScannerPixelDataTypePalette
  , pattern ICScannerPixelDataTypeCMY
  , pattern ICScannerPixelDataTypeCMYK
  , pattern ICScannerPixelDataTypeYUV
  , pattern ICScannerPixelDataTypeYUVK
  , pattern ICScannerPixelDataTypeCIEXYZ

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ImageCaptureCore.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.ImageCaptureCore.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | type
--
-- ￼Functional unit type.
--
-- ObjC selector: @- type@
type_ :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> IO ICScannerFunctionalUnitType
type_ icScannerFunctionalUnit =
  sendMessage icScannerFunctionalUnit typeSelector

-- | pixelDataType
--
-- ￼The pixel data type.
--
-- ObjC selector: @- pixelDataType@
pixelDataType :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> IO ICScannerPixelDataType
pixelDataType icScannerFunctionalUnit =
  sendMessage icScannerFunctionalUnit pixelDataTypeSelector

-- | pixelDataType
--
-- ￼The pixel data type.
--
-- ObjC selector: @- setPixelDataType:@
setPixelDataType :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> ICScannerPixelDataType -> IO ()
setPixelDataType icScannerFunctionalUnit value =
  sendMessage icScannerFunctionalUnit setPixelDataTypeSelector value

-- | supportedBitDepths
--
-- ￼Supported bit depths. The values in this set are valid values defined by ICScannerBitDepth.
--
-- ObjC selector: @- supportedBitDepths@
supportedBitDepths :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> IO (Id NSIndexSet)
supportedBitDepths icScannerFunctionalUnit =
  sendMessage icScannerFunctionalUnit supportedBitDepthsSelector

-- | bitDepth
--
-- ￼The bit depth to use when performing the final scan. This will always be one of the supported bit depths.
--
-- ObjC selector: @- bitDepth@
bitDepth :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> IO ICScannerBitDepth
bitDepth icScannerFunctionalUnit =
  sendMessage icScannerFunctionalUnit bitDepthSelector

-- | bitDepth
--
-- ￼The bit depth to use when performing the final scan. This will always be one of the supported bit depths.
--
-- ObjC selector: @- setBitDepth:@
setBitDepth :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> ICScannerBitDepth -> IO ()
setBitDepth icScannerFunctionalUnit value =
  sendMessage icScannerFunctionalUnit setBitDepthSelector value

-- | supportedMeasurementUnits
--
-- ￼Supported measurement units. The values in this set are valid values defined by ICScannerMeasurementUnit.
--
-- ObjC selector: @- supportedMeasurementUnits@
supportedMeasurementUnits :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> IO (Id NSIndexSet)
supportedMeasurementUnits icScannerFunctionalUnit =
  sendMessage icScannerFunctionalUnit supportedMeasurementUnitsSelector

-- | measurementUnit
--
-- ￼Current measurement unit. This will always be one of the supported measurement units.
--
-- ObjC selector: @- measurementUnit@
measurementUnit :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> IO ICScannerMeasurementUnit
measurementUnit icScannerFunctionalUnit =
  sendMessage icScannerFunctionalUnit measurementUnitSelector

-- | measurementUnit
--
-- ￼Current measurement unit. This will always be one of the supported measurement units.
--
-- ObjC selector: @- setMeasurementUnit:@
setMeasurementUnit :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> ICScannerMeasurementUnit -> IO ()
setMeasurementUnit icScannerFunctionalUnit value =
  sendMessage icScannerFunctionalUnit setMeasurementUnitSelector value

-- | supportedResolutions
--
-- ￼Supported scan resolutions in DPI.
--
-- ObjC selector: @- supportedResolutions@
supportedResolutions :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> IO (Id NSIndexSet)
supportedResolutions icScannerFunctionalUnit =
  sendMessage icScannerFunctionalUnit supportedResolutionsSelector

-- | preferredResolutions
--
-- ￼Preferred scan resolutions in DPI.
--
-- ObjC selector: @- preferredResolutions@
preferredResolutions :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> IO (Id NSIndexSet)
preferredResolutions icScannerFunctionalUnit =
  sendMessage icScannerFunctionalUnit preferredResolutionsSelector

-- | resolution
--
-- ￼Current scan resolution. This will always be one of the supported resolution values.
--
-- ObjC selector: @- resolution@
resolution :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> IO CULong
resolution icScannerFunctionalUnit =
  sendMessage icScannerFunctionalUnit resolutionSelector

-- | resolution
--
-- ￼Current scan resolution. This will always be one of the supported resolution values.
--
-- ObjC selector: @- setResolution:@
setResolution :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> CULong -> IO ()
setResolution icScannerFunctionalUnit value =
  sendMessage icScannerFunctionalUnit setResolutionSelector value

-- | nativeXResolution
--
-- ￼Optical resolution along the X axis.
--
-- ObjC selector: @- nativeXResolution@
nativeXResolution :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> IO CULong
nativeXResolution icScannerFunctionalUnit =
  sendMessage icScannerFunctionalUnit nativeXResolutionSelector

-- | nativeYResolution
--
-- ￼Optical resolution along the Y axis.
--
-- ObjC selector: @- nativeYResolution@
nativeYResolution :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> IO CULong
nativeYResolution icScannerFunctionalUnit =
  sendMessage icScannerFunctionalUnit nativeYResolutionSelector

-- | supportedScaleFactors
--
-- ￼Supported scale factors in percentage.
--
-- ObjC selector: @- supportedScaleFactors@
supportedScaleFactors :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> IO (Id NSIndexSet)
supportedScaleFactors icScannerFunctionalUnit =
  sendMessage icScannerFunctionalUnit supportedScaleFactorsSelector

-- | preferredScaleFactors
--
-- ￼Preferred scale factors in percentage.
--
-- ObjC selector: @- preferredScaleFactors@
preferredScaleFactors :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> IO (Id NSIndexSet)
preferredScaleFactors icScannerFunctionalUnit =
  sendMessage icScannerFunctionalUnit preferredScaleFactorsSelector

-- | scaleFactor
--
-- ￼Current scale factor. This will always be one of the supported scale factor values.
--
-- ObjC selector: @- scaleFactor@
scaleFactor :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> IO CULong
scaleFactor icScannerFunctionalUnit =
  sendMessage icScannerFunctionalUnit scaleFactorSelector

-- | scaleFactor
--
-- ￼Current scale factor. This will always be one of the supported scale factor values.
--
-- ObjC selector: @- setScaleFactor:@
setScaleFactor :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> CULong -> IO ()
setScaleFactor icScannerFunctionalUnit value =
  sendMessage icScannerFunctionalUnit setScaleFactorSelector value

-- | templates
--
-- An array of objects of type ICScannerFeatureTemplate.
--
-- ObjC selector: @- templates@
templates :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> IO (Id NSArray)
templates icScannerFunctionalUnit =
  sendMessage icScannerFunctionalUnit templatesSelector

-- | vendorFeatures
--
-- An array of objects of type ICScannerFeature.
--
-- ObjC selector: @- vendorFeatures@
vendorFeatures :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> IO (Id NSArray)
vendorFeatures icScannerFunctionalUnit =
  sendMessage icScannerFunctionalUnit vendorFeaturesSelector

-- | physicalSize
--
-- ￼Physical size of the scan area in current measurement unit.
--
-- ObjC selector: @- physicalSize@
physicalSize :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> IO NSSize
physicalSize icScannerFunctionalUnit =
  sendMessage icScannerFunctionalUnit physicalSizeSelector

-- | scanArea
--
-- ￼This property along with scanAreaOrientation describes the area to be scanned.
--
-- ObjC selector: @- scanArea@
scanArea :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> IO NSRect
scanArea icScannerFunctionalUnit =
  sendMessage icScannerFunctionalUnit scanAreaSelector

-- | scanArea
--
-- ￼This property along with scanAreaOrientation describes the area to be scanned.
--
-- ObjC selector: @- setScanArea:@
setScanArea :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> NSRect -> IO ()
setScanArea icScannerFunctionalUnit value =
  sendMessage icScannerFunctionalUnit setScanAreaSelector value

-- | scanAreaOrientation
--
-- ￼Desired orientation of the scan area. This property along with scanArea describes the area to be scanned.
--
-- This property is set to ICEXIFOrientation1 initially. This property is not used by the ICScannerFunctionalUnitDocumentFeeder subclass.
--
-- ObjC selector: @- scanAreaOrientation@
scanAreaOrientation :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> IO ICEXIFOrientationType
scanAreaOrientation icScannerFunctionalUnit =
  sendMessage icScannerFunctionalUnit scanAreaOrientationSelector

-- | scanAreaOrientation
--
-- ￼Desired orientation of the scan area. This property along with scanArea describes the area to be scanned.
--
-- This property is set to ICEXIFOrientation1 initially. This property is not used by the ICScannerFunctionalUnitDocumentFeeder subclass.
--
-- ObjC selector: @- setScanAreaOrientation:@
setScanAreaOrientation :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> ICEXIFOrientationType -> IO ()
setScanAreaOrientation icScannerFunctionalUnit value =
  sendMessage icScannerFunctionalUnit setScanAreaOrientationSelector value

-- | acceptsThresholdForBlackAndWhiteScanning
--
-- ￼Indicates if this functional unit accepts threshold value to be used when performing a scan in black & white.
--
-- ObjC selector: @- acceptsThresholdForBlackAndWhiteScanning@
acceptsThresholdForBlackAndWhiteScanning :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> IO Bool
acceptsThresholdForBlackAndWhiteScanning icScannerFunctionalUnit =
  sendMessage icScannerFunctionalUnit acceptsThresholdForBlackAndWhiteScanningSelector

-- | usesThresholdForBlackAndWhiteScanning
--
-- ￼Indicates if this functional unit uses threshold value to be used when performing a scan in black & white.
--
-- ObjC selector: @- usesThresholdForBlackAndWhiteScanning@
usesThresholdForBlackAndWhiteScanning :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> IO Bool
usesThresholdForBlackAndWhiteScanning icScannerFunctionalUnit =
  sendMessage icScannerFunctionalUnit usesThresholdForBlackAndWhiteScanningSelector

-- | usesThresholdForBlackAndWhiteScanning
--
-- ￼Indicates if this functional unit uses threshold value to be used when performing a scan in black & white.
--
-- ObjC selector: @- setUsesThresholdForBlackAndWhiteScanning:@
setUsesThresholdForBlackAndWhiteScanning :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> Bool -> IO ()
setUsesThresholdForBlackAndWhiteScanning icScannerFunctionalUnit value =
  sendMessage icScannerFunctionalUnit setUsesThresholdForBlackAndWhiteScanningSelector value

-- | defaultThresholdForBlackAndWhiteScanning
--
-- ￼Default threshold value used when performing a scan in black & white. This value is from 0 to 255.
--
-- ObjC selector: @- defaultThresholdForBlackAndWhiteScanning@
defaultThresholdForBlackAndWhiteScanning :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> IO CUChar
defaultThresholdForBlackAndWhiteScanning icScannerFunctionalUnit =
  sendMessage icScannerFunctionalUnit defaultThresholdForBlackAndWhiteScanningSelector

-- | thresholdForBlackAndWhiteScanning
--
-- ￼Threshold value to be used when performing a scan in black & white. This value should be from 0 to 255.
--
-- ObjC selector: @- thresholdForBlackAndWhiteScanning@
thresholdForBlackAndWhiteScanning :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> IO CUChar
thresholdForBlackAndWhiteScanning icScannerFunctionalUnit =
  sendMessage icScannerFunctionalUnit thresholdForBlackAndWhiteScanningSelector

-- | thresholdForBlackAndWhiteScanning
--
-- ￼Threshold value to be used when performing a scan in black & white. This value should be from 0 to 255.
--
-- ObjC selector: @- setThresholdForBlackAndWhiteScanning:@
setThresholdForBlackAndWhiteScanning :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> CUChar -> IO ()
setThresholdForBlackAndWhiteScanning icScannerFunctionalUnit value =
  sendMessage icScannerFunctionalUnit setThresholdForBlackAndWhiteScanningSelector value

-- | state
--
-- ￼Indicates the current state of the functional unit.
--
-- ObjC selector: @- state@
state :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> IO ICScannerFunctionalUnitState
state icScannerFunctionalUnit =
  sendMessage icScannerFunctionalUnit stateSelector

-- | scanInProgress
--
-- ￼Indicates if a scan is in progress.
--
-- ObjC selector: @- scanInProgress@
scanInProgress :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> IO Bool
scanInProgress icScannerFunctionalUnit =
  sendMessage icScannerFunctionalUnit scanInProgressSelector

-- | scanProgressPercentDone
--
-- ￼Indicates percentage of scan completed.
--
-- ObjC selector: @- scanProgressPercentDone@
scanProgressPercentDone :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> IO CDouble
scanProgressPercentDone icScannerFunctionalUnit =
  sendMessage icScannerFunctionalUnit scanProgressPercentDoneSelector

-- | canPerformOverviewScan
--
-- ￼Indicates if this functional unit can perfrom an overview scan. Not all functional units can perform an overview scan. For example, a document feeder or a sheet feeder unit cannot perform an overview scan.
--
-- ObjC selector: @- canPerformOverviewScan@
canPerformOverviewScan :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> IO Bool
canPerformOverviewScan icScannerFunctionalUnit =
  sendMessage icScannerFunctionalUnit canPerformOverviewScanSelector

-- | overviewScanInProgress
--
-- ￼Indicates if an overview scan is in progress.
--
-- ObjC selector: @- overviewScanInProgress@
overviewScanInProgress :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> IO Bool
overviewScanInProgress icScannerFunctionalUnit =
  sendMessage icScannerFunctionalUnit overviewScanInProgressSelector

-- | overviewImage
--
-- ￼Overview scan image. This property will be NULL for functional units that do not support overview scans.
--
-- ObjC selector: @- overviewImage@
overviewImage :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> IO (Ptr ())
overviewImage icScannerFunctionalUnit =
  sendMessage icScannerFunctionalUnit overviewImageSelector

-- | overviewResolution
--
-- ￼Overview image resolution. Value assigned to this will be contrained by resolutions allowed by the device.
--
-- ObjC selector: @- overviewResolution@
overviewResolution :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> IO CULong
overviewResolution icScannerFunctionalUnit =
  sendMessage icScannerFunctionalUnit overviewResolutionSelector

-- | overviewResolution
--
-- ￼Overview image resolution. Value assigned to this will be contrained by resolutions allowed by the device.
--
-- ObjC selector: @- setOverviewResolution:@
setOverviewResolution :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> CULong -> IO ()
setOverviewResolution icScannerFunctionalUnit value =
  sendMessage icScannerFunctionalUnit setOverviewResolutionSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @type@
typeSelector :: Selector '[] ICScannerFunctionalUnitType
typeSelector = mkSelector "type"

-- | @Selector@ for @pixelDataType@
pixelDataTypeSelector :: Selector '[] ICScannerPixelDataType
pixelDataTypeSelector = mkSelector "pixelDataType"

-- | @Selector@ for @setPixelDataType:@
setPixelDataTypeSelector :: Selector '[ICScannerPixelDataType] ()
setPixelDataTypeSelector = mkSelector "setPixelDataType:"

-- | @Selector@ for @supportedBitDepths@
supportedBitDepthsSelector :: Selector '[] (Id NSIndexSet)
supportedBitDepthsSelector = mkSelector "supportedBitDepths"

-- | @Selector@ for @bitDepth@
bitDepthSelector :: Selector '[] ICScannerBitDepth
bitDepthSelector = mkSelector "bitDepth"

-- | @Selector@ for @setBitDepth:@
setBitDepthSelector :: Selector '[ICScannerBitDepth] ()
setBitDepthSelector = mkSelector "setBitDepth:"

-- | @Selector@ for @supportedMeasurementUnits@
supportedMeasurementUnitsSelector :: Selector '[] (Id NSIndexSet)
supportedMeasurementUnitsSelector = mkSelector "supportedMeasurementUnits"

-- | @Selector@ for @measurementUnit@
measurementUnitSelector :: Selector '[] ICScannerMeasurementUnit
measurementUnitSelector = mkSelector "measurementUnit"

-- | @Selector@ for @setMeasurementUnit:@
setMeasurementUnitSelector :: Selector '[ICScannerMeasurementUnit] ()
setMeasurementUnitSelector = mkSelector "setMeasurementUnit:"

-- | @Selector@ for @supportedResolutions@
supportedResolutionsSelector :: Selector '[] (Id NSIndexSet)
supportedResolutionsSelector = mkSelector "supportedResolutions"

-- | @Selector@ for @preferredResolutions@
preferredResolutionsSelector :: Selector '[] (Id NSIndexSet)
preferredResolutionsSelector = mkSelector "preferredResolutions"

-- | @Selector@ for @resolution@
resolutionSelector :: Selector '[] CULong
resolutionSelector = mkSelector "resolution"

-- | @Selector@ for @setResolution:@
setResolutionSelector :: Selector '[CULong] ()
setResolutionSelector = mkSelector "setResolution:"

-- | @Selector@ for @nativeXResolution@
nativeXResolutionSelector :: Selector '[] CULong
nativeXResolutionSelector = mkSelector "nativeXResolution"

-- | @Selector@ for @nativeYResolution@
nativeYResolutionSelector :: Selector '[] CULong
nativeYResolutionSelector = mkSelector "nativeYResolution"

-- | @Selector@ for @supportedScaleFactors@
supportedScaleFactorsSelector :: Selector '[] (Id NSIndexSet)
supportedScaleFactorsSelector = mkSelector "supportedScaleFactors"

-- | @Selector@ for @preferredScaleFactors@
preferredScaleFactorsSelector :: Selector '[] (Id NSIndexSet)
preferredScaleFactorsSelector = mkSelector "preferredScaleFactors"

-- | @Selector@ for @scaleFactor@
scaleFactorSelector :: Selector '[] CULong
scaleFactorSelector = mkSelector "scaleFactor"

-- | @Selector@ for @setScaleFactor:@
setScaleFactorSelector :: Selector '[CULong] ()
setScaleFactorSelector = mkSelector "setScaleFactor:"

-- | @Selector@ for @templates@
templatesSelector :: Selector '[] (Id NSArray)
templatesSelector = mkSelector "templates"

-- | @Selector@ for @vendorFeatures@
vendorFeaturesSelector :: Selector '[] (Id NSArray)
vendorFeaturesSelector = mkSelector "vendorFeatures"

-- | @Selector@ for @physicalSize@
physicalSizeSelector :: Selector '[] NSSize
physicalSizeSelector = mkSelector "physicalSize"

-- | @Selector@ for @scanArea@
scanAreaSelector :: Selector '[] NSRect
scanAreaSelector = mkSelector "scanArea"

-- | @Selector@ for @setScanArea:@
setScanAreaSelector :: Selector '[NSRect] ()
setScanAreaSelector = mkSelector "setScanArea:"

-- | @Selector@ for @scanAreaOrientation@
scanAreaOrientationSelector :: Selector '[] ICEXIFOrientationType
scanAreaOrientationSelector = mkSelector "scanAreaOrientation"

-- | @Selector@ for @setScanAreaOrientation:@
setScanAreaOrientationSelector :: Selector '[ICEXIFOrientationType] ()
setScanAreaOrientationSelector = mkSelector "setScanAreaOrientation:"

-- | @Selector@ for @acceptsThresholdForBlackAndWhiteScanning@
acceptsThresholdForBlackAndWhiteScanningSelector :: Selector '[] Bool
acceptsThresholdForBlackAndWhiteScanningSelector = mkSelector "acceptsThresholdForBlackAndWhiteScanning"

-- | @Selector@ for @usesThresholdForBlackAndWhiteScanning@
usesThresholdForBlackAndWhiteScanningSelector :: Selector '[] Bool
usesThresholdForBlackAndWhiteScanningSelector = mkSelector "usesThresholdForBlackAndWhiteScanning"

-- | @Selector@ for @setUsesThresholdForBlackAndWhiteScanning:@
setUsesThresholdForBlackAndWhiteScanningSelector :: Selector '[Bool] ()
setUsesThresholdForBlackAndWhiteScanningSelector = mkSelector "setUsesThresholdForBlackAndWhiteScanning:"

-- | @Selector@ for @defaultThresholdForBlackAndWhiteScanning@
defaultThresholdForBlackAndWhiteScanningSelector :: Selector '[] CUChar
defaultThresholdForBlackAndWhiteScanningSelector = mkSelector "defaultThresholdForBlackAndWhiteScanning"

-- | @Selector@ for @thresholdForBlackAndWhiteScanning@
thresholdForBlackAndWhiteScanningSelector :: Selector '[] CUChar
thresholdForBlackAndWhiteScanningSelector = mkSelector "thresholdForBlackAndWhiteScanning"

-- | @Selector@ for @setThresholdForBlackAndWhiteScanning:@
setThresholdForBlackAndWhiteScanningSelector :: Selector '[CUChar] ()
setThresholdForBlackAndWhiteScanningSelector = mkSelector "setThresholdForBlackAndWhiteScanning:"

-- | @Selector@ for @state@
stateSelector :: Selector '[] ICScannerFunctionalUnitState
stateSelector = mkSelector "state"

-- | @Selector@ for @scanInProgress@
scanInProgressSelector :: Selector '[] Bool
scanInProgressSelector = mkSelector "scanInProgress"

-- | @Selector@ for @scanProgressPercentDone@
scanProgressPercentDoneSelector :: Selector '[] CDouble
scanProgressPercentDoneSelector = mkSelector "scanProgressPercentDone"

-- | @Selector@ for @canPerformOverviewScan@
canPerformOverviewScanSelector :: Selector '[] Bool
canPerformOverviewScanSelector = mkSelector "canPerformOverviewScan"

-- | @Selector@ for @overviewScanInProgress@
overviewScanInProgressSelector :: Selector '[] Bool
overviewScanInProgressSelector = mkSelector "overviewScanInProgress"

-- | @Selector@ for @overviewImage@
overviewImageSelector :: Selector '[] (Ptr ())
overviewImageSelector = mkSelector "overviewImage"

-- | @Selector@ for @overviewResolution@
overviewResolutionSelector :: Selector '[] CULong
overviewResolutionSelector = mkSelector "overviewResolution"

-- | @Selector@ for @setOverviewResolution:@
setOverviewResolutionSelector :: Selector '[CULong] ()
setOverviewResolutionSelector = mkSelector "setOverviewResolution:"

