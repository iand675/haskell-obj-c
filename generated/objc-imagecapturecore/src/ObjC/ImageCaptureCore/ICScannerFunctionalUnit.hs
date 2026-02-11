{-# LANGUAGE PatternSynonyms #-}
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
  , typeSelector
  , pixelDataTypeSelector
  , setPixelDataTypeSelector
  , supportedBitDepthsSelector
  , bitDepthSelector
  , setBitDepthSelector
  , supportedMeasurementUnitsSelector
  , measurementUnitSelector
  , setMeasurementUnitSelector
  , supportedResolutionsSelector
  , preferredResolutionsSelector
  , resolutionSelector
  , setResolutionSelector
  , nativeXResolutionSelector
  , nativeYResolutionSelector
  , supportedScaleFactorsSelector
  , preferredScaleFactorsSelector
  , scaleFactorSelector
  , setScaleFactorSelector
  , templatesSelector
  , vendorFeaturesSelector
  , physicalSizeSelector
  , scanAreaSelector
  , setScanAreaSelector
  , scanAreaOrientationSelector
  , setScanAreaOrientationSelector
  , acceptsThresholdForBlackAndWhiteScanningSelector
  , usesThresholdForBlackAndWhiteScanningSelector
  , setUsesThresholdForBlackAndWhiteScanningSelector
  , defaultThresholdForBlackAndWhiteScanningSelector
  , thresholdForBlackAndWhiteScanningSelector
  , setThresholdForBlackAndWhiteScanningSelector
  , stateSelector
  , scanInProgressSelector
  , scanProgressPercentDoneSelector
  , canPerformOverviewScanSelector
  , overviewScanInProgressSelector
  , overviewImageSelector
  , overviewResolutionSelector
  , setOverviewResolutionSelector

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
type_ icScannerFunctionalUnit  =
  fmap (coerce :: CULong -> ICScannerFunctionalUnitType) $ sendMsg icScannerFunctionalUnit (mkSelector "type") retCULong []

-- | pixelDataType
--
-- ￼The pixel data type.
--
-- ObjC selector: @- pixelDataType@
pixelDataType :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> IO ICScannerPixelDataType
pixelDataType icScannerFunctionalUnit  =
  fmap (coerce :: CULong -> ICScannerPixelDataType) $ sendMsg icScannerFunctionalUnit (mkSelector "pixelDataType") retCULong []

-- | pixelDataType
--
-- ￼The pixel data type.
--
-- ObjC selector: @- setPixelDataType:@
setPixelDataType :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> ICScannerPixelDataType -> IO ()
setPixelDataType icScannerFunctionalUnit  value =
  sendMsg icScannerFunctionalUnit (mkSelector "setPixelDataType:") retVoid [argCULong (coerce value)]

-- | supportedBitDepths
--
-- ￼Supported bit depths. The values in this set are valid values defined by ICScannerBitDepth.
--
-- ObjC selector: @- supportedBitDepths@
supportedBitDepths :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> IO (Id NSIndexSet)
supportedBitDepths icScannerFunctionalUnit  =
  sendMsg icScannerFunctionalUnit (mkSelector "supportedBitDepths") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | bitDepth
--
-- ￼The bit depth to use when performing the final scan. This will always be one of the supported bit depths.
--
-- ObjC selector: @- bitDepth@
bitDepth :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> IO ICScannerBitDepth
bitDepth icScannerFunctionalUnit  =
  fmap (coerce :: CULong -> ICScannerBitDepth) $ sendMsg icScannerFunctionalUnit (mkSelector "bitDepth") retCULong []

-- | bitDepth
--
-- ￼The bit depth to use when performing the final scan. This will always be one of the supported bit depths.
--
-- ObjC selector: @- setBitDepth:@
setBitDepth :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> ICScannerBitDepth -> IO ()
setBitDepth icScannerFunctionalUnit  value =
  sendMsg icScannerFunctionalUnit (mkSelector "setBitDepth:") retVoid [argCULong (coerce value)]

-- | supportedMeasurementUnits
--
-- ￼Supported measurement units. The values in this set are valid values defined by ICScannerMeasurementUnit.
--
-- ObjC selector: @- supportedMeasurementUnits@
supportedMeasurementUnits :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> IO (Id NSIndexSet)
supportedMeasurementUnits icScannerFunctionalUnit  =
  sendMsg icScannerFunctionalUnit (mkSelector "supportedMeasurementUnits") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | measurementUnit
--
-- ￼Current measurement unit. This will always be one of the supported measurement units.
--
-- ObjC selector: @- measurementUnit@
measurementUnit :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> IO ICScannerMeasurementUnit
measurementUnit icScannerFunctionalUnit  =
  fmap (coerce :: CULong -> ICScannerMeasurementUnit) $ sendMsg icScannerFunctionalUnit (mkSelector "measurementUnit") retCULong []

-- | measurementUnit
--
-- ￼Current measurement unit. This will always be one of the supported measurement units.
--
-- ObjC selector: @- setMeasurementUnit:@
setMeasurementUnit :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> ICScannerMeasurementUnit -> IO ()
setMeasurementUnit icScannerFunctionalUnit  value =
  sendMsg icScannerFunctionalUnit (mkSelector "setMeasurementUnit:") retVoid [argCULong (coerce value)]

-- | supportedResolutions
--
-- ￼Supported scan resolutions in DPI.
--
-- ObjC selector: @- supportedResolutions@
supportedResolutions :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> IO (Id NSIndexSet)
supportedResolutions icScannerFunctionalUnit  =
  sendMsg icScannerFunctionalUnit (mkSelector "supportedResolutions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | preferredResolutions
--
-- ￼Preferred scan resolutions in DPI.
--
-- ObjC selector: @- preferredResolutions@
preferredResolutions :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> IO (Id NSIndexSet)
preferredResolutions icScannerFunctionalUnit  =
  sendMsg icScannerFunctionalUnit (mkSelector "preferredResolutions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | resolution
--
-- ￼Current scan resolution. This will always be one of the supported resolution values.
--
-- ObjC selector: @- resolution@
resolution :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> IO CULong
resolution icScannerFunctionalUnit  =
  sendMsg icScannerFunctionalUnit (mkSelector "resolution") retCULong []

-- | resolution
--
-- ￼Current scan resolution. This will always be one of the supported resolution values.
--
-- ObjC selector: @- setResolution:@
setResolution :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> CULong -> IO ()
setResolution icScannerFunctionalUnit  value =
  sendMsg icScannerFunctionalUnit (mkSelector "setResolution:") retVoid [argCULong (fromIntegral value)]

-- | nativeXResolution
--
-- ￼Optical resolution along the X axis.
--
-- ObjC selector: @- nativeXResolution@
nativeXResolution :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> IO CULong
nativeXResolution icScannerFunctionalUnit  =
  sendMsg icScannerFunctionalUnit (mkSelector "nativeXResolution") retCULong []

-- | nativeYResolution
--
-- ￼Optical resolution along the Y axis.
--
-- ObjC selector: @- nativeYResolution@
nativeYResolution :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> IO CULong
nativeYResolution icScannerFunctionalUnit  =
  sendMsg icScannerFunctionalUnit (mkSelector "nativeYResolution") retCULong []

-- | supportedScaleFactors
--
-- ￼Supported scale factors in percentage.
--
-- ObjC selector: @- supportedScaleFactors@
supportedScaleFactors :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> IO (Id NSIndexSet)
supportedScaleFactors icScannerFunctionalUnit  =
  sendMsg icScannerFunctionalUnit (mkSelector "supportedScaleFactors") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | preferredScaleFactors
--
-- ￼Preferred scale factors in percentage.
--
-- ObjC selector: @- preferredScaleFactors@
preferredScaleFactors :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> IO (Id NSIndexSet)
preferredScaleFactors icScannerFunctionalUnit  =
  sendMsg icScannerFunctionalUnit (mkSelector "preferredScaleFactors") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | scaleFactor
--
-- ￼Current scale factor. This will always be one of the supported scale factor values.
--
-- ObjC selector: @- scaleFactor@
scaleFactor :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> IO CULong
scaleFactor icScannerFunctionalUnit  =
  sendMsg icScannerFunctionalUnit (mkSelector "scaleFactor") retCULong []

-- | scaleFactor
--
-- ￼Current scale factor. This will always be one of the supported scale factor values.
--
-- ObjC selector: @- setScaleFactor:@
setScaleFactor :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> CULong -> IO ()
setScaleFactor icScannerFunctionalUnit  value =
  sendMsg icScannerFunctionalUnit (mkSelector "setScaleFactor:") retVoid [argCULong (fromIntegral value)]

-- | templates
--
-- An array of objects of type ICScannerFeatureTemplate.
--
-- ObjC selector: @- templates@
templates :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> IO (Id NSArray)
templates icScannerFunctionalUnit  =
  sendMsg icScannerFunctionalUnit (mkSelector "templates") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | vendorFeatures
--
-- An array of objects of type ICScannerFeature.
--
-- ObjC selector: @- vendorFeatures@
vendorFeatures :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> IO (Id NSArray)
vendorFeatures icScannerFunctionalUnit  =
  sendMsg icScannerFunctionalUnit (mkSelector "vendorFeatures") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | physicalSize
--
-- ￼Physical size of the scan area in current measurement unit.
--
-- ObjC selector: @- physicalSize@
physicalSize :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> IO NSSize
physicalSize icScannerFunctionalUnit  =
  sendMsgStret icScannerFunctionalUnit (mkSelector "physicalSize") retNSSize []

-- | scanArea
--
-- ￼This property along with scanAreaOrientation describes the area to be scanned.
--
-- ObjC selector: @- scanArea@
scanArea :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> IO NSRect
scanArea icScannerFunctionalUnit  =
  sendMsgStret icScannerFunctionalUnit (mkSelector "scanArea") retNSRect []

-- | scanArea
--
-- ￼This property along with scanAreaOrientation describes the area to be scanned.
--
-- ObjC selector: @- setScanArea:@
setScanArea :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> NSRect -> IO ()
setScanArea icScannerFunctionalUnit  value =
  sendMsg icScannerFunctionalUnit (mkSelector "setScanArea:") retVoid [argNSRect value]

-- | scanAreaOrientation
--
-- ￼Desired orientation of the scan area. This property along with scanArea describes the area to be scanned.
--
-- This property is set to ICEXIFOrientation1 initially. This property is not used by the ICScannerFunctionalUnitDocumentFeeder subclass.
--
-- ObjC selector: @- scanAreaOrientation@
scanAreaOrientation :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> IO ICEXIFOrientationType
scanAreaOrientation icScannerFunctionalUnit  =
  fmap (coerce :: CULong -> ICEXIFOrientationType) $ sendMsg icScannerFunctionalUnit (mkSelector "scanAreaOrientation") retCULong []

-- | scanAreaOrientation
--
-- ￼Desired orientation of the scan area. This property along with scanArea describes the area to be scanned.
--
-- This property is set to ICEXIFOrientation1 initially. This property is not used by the ICScannerFunctionalUnitDocumentFeeder subclass.
--
-- ObjC selector: @- setScanAreaOrientation:@
setScanAreaOrientation :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> ICEXIFOrientationType -> IO ()
setScanAreaOrientation icScannerFunctionalUnit  value =
  sendMsg icScannerFunctionalUnit (mkSelector "setScanAreaOrientation:") retVoid [argCULong (coerce value)]

-- | acceptsThresholdForBlackAndWhiteScanning
--
-- ￼Indicates if this functional unit accepts threshold value to be used when performing a scan in black & white.
--
-- ObjC selector: @- acceptsThresholdForBlackAndWhiteScanning@
acceptsThresholdForBlackAndWhiteScanning :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> IO Bool
acceptsThresholdForBlackAndWhiteScanning icScannerFunctionalUnit  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg icScannerFunctionalUnit (mkSelector "acceptsThresholdForBlackAndWhiteScanning") retCULong []

-- | usesThresholdForBlackAndWhiteScanning
--
-- ￼Indicates if this functional unit uses threshold value to be used when performing a scan in black & white.
--
-- ObjC selector: @- usesThresholdForBlackAndWhiteScanning@
usesThresholdForBlackAndWhiteScanning :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> IO Bool
usesThresholdForBlackAndWhiteScanning icScannerFunctionalUnit  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg icScannerFunctionalUnit (mkSelector "usesThresholdForBlackAndWhiteScanning") retCULong []

-- | usesThresholdForBlackAndWhiteScanning
--
-- ￼Indicates if this functional unit uses threshold value to be used when performing a scan in black & white.
--
-- ObjC selector: @- setUsesThresholdForBlackAndWhiteScanning:@
setUsesThresholdForBlackAndWhiteScanning :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> Bool -> IO ()
setUsesThresholdForBlackAndWhiteScanning icScannerFunctionalUnit  value =
  sendMsg icScannerFunctionalUnit (mkSelector "setUsesThresholdForBlackAndWhiteScanning:") retVoid [argCULong (if value then 1 else 0)]

-- | defaultThresholdForBlackAndWhiteScanning
--
-- ￼Default threshold value used when performing a scan in black & white. This value is from 0 to 255.
--
-- ObjC selector: @- defaultThresholdForBlackAndWhiteScanning@
defaultThresholdForBlackAndWhiteScanning :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> IO CUChar
defaultThresholdForBlackAndWhiteScanning icScannerFunctionalUnit  =
  sendMsg icScannerFunctionalUnit (mkSelector "defaultThresholdForBlackAndWhiteScanning") retCUChar []

-- | thresholdForBlackAndWhiteScanning
--
-- ￼Threshold value to be used when performing a scan in black & white. This value should be from 0 to 255.
--
-- ObjC selector: @- thresholdForBlackAndWhiteScanning@
thresholdForBlackAndWhiteScanning :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> IO CUChar
thresholdForBlackAndWhiteScanning icScannerFunctionalUnit  =
  sendMsg icScannerFunctionalUnit (mkSelector "thresholdForBlackAndWhiteScanning") retCUChar []

-- | thresholdForBlackAndWhiteScanning
--
-- ￼Threshold value to be used when performing a scan in black & white. This value should be from 0 to 255.
--
-- ObjC selector: @- setThresholdForBlackAndWhiteScanning:@
setThresholdForBlackAndWhiteScanning :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> CUChar -> IO ()
setThresholdForBlackAndWhiteScanning icScannerFunctionalUnit  value =
  sendMsg icScannerFunctionalUnit (mkSelector "setThresholdForBlackAndWhiteScanning:") retVoid [argCUChar (fromIntegral value)]

-- | state
--
-- ￼Indicates the current state of the functional unit.
--
-- ObjC selector: @- state@
state :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> IO ICScannerFunctionalUnitState
state icScannerFunctionalUnit  =
  fmap (coerce :: CULong -> ICScannerFunctionalUnitState) $ sendMsg icScannerFunctionalUnit (mkSelector "state") retCULong []

-- | scanInProgress
--
-- ￼Indicates if a scan is in progress.
--
-- ObjC selector: @- scanInProgress@
scanInProgress :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> IO Bool
scanInProgress icScannerFunctionalUnit  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg icScannerFunctionalUnit (mkSelector "scanInProgress") retCULong []

-- | scanProgressPercentDone
--
-- ￼Indicates percentage of scan completed.
--
-- ObjC selector: @- scanProgressPercentDone@
scanProgressPercentDone :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> IO CDouble
scanProgressPercentDone icScannerFunctionalUnit  =
  sendMsg icScannerFunctionalUnit (mkSelector "scanProgressPercentDone") retCDouble []

-- | canPerformOverviewScan
--
-- ￼Indicates if this functional unit can perfrom an overview scan. Not all functional units can perform an overview scan. For example, a document feeder or a sheet feeder unit cannot perform an overview scan.
--
-- ObjC selector: @- canPerformOverviewScan@
canPerformOverviewScan :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> IO Bool
canPerformOverviewScan icScannerFunctionalUnit  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg icScannerFunctionalUnit (mkSelector "canPerformOverviewScan") retCULong []

-- | overviewScanInProgress
--
-- ￼Indicates if an overview scan is in progress.
--
-- ObjC selector: @- overviewScanInProgress@
overviewScanInProgress :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> IO Bool
overviewScanInProgress icScannerFunctionalUnit  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg icScannerFunctionalUnit (mkSelector "overviewScanInProgress") retCULong []

-- | overviewImage
--
-- ￼Overview scan image. This property will be NULL for functional units that do not support overview scans.
--
-- ObjC selector: @- overviewImage@
overviewImage :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> IO (Ptr ())
overviewImage icScannerFunctionalUnit  =
  fmap castPtr $ sendMsg icScannerFunctionalUnit (mkSelector "overviewImage") (retPtr retVoid) []

-- | overviewResolution
--
-- ￼Overview image resolution. Value assigned to this will be contrained by resolutions allowed by the device.
--
-- ObjC selector: @- overviewResolution@
overviewResolution :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> IO CULong
overviewResolution icScannerFunctionalUnit  =
  sendMsg icScannerFunctionalUnit (mkSelector "overviewResolution") retCULong []

-- | overviewResolution
--
-- ￼Overview image resolution. Value assigned to this will be contrained by resolutions allowed by the device.
--
-- ObjC selector: @- setOverviewResolution:@
setOverviewResolution :: IsICScannerFunctionalUnit icScannerFunctionalUnit => icScannerFunctionalUnit -> CULong -> IO ()
setOverviewResolution icScannerFunctionalUnit  value =
  sendMsg icScannerFunctionalUnit (mkSelector "setOverviewResolution:") retVoid [argCULong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @pixelDataType@
pixelDataTypeSelector :: Selector
pixelDataTypeSelector = mkSelector "pixelDataType"

-- | @Selector@ for @setPixelDataType:@
setPixelDataTypeSelector :: Selector
setPixelDataTypeSelector = mkSelector "setPixelDataType:"

-- | @Selector@ for @supportedBitDepths@
supportedBitDepthsSelector :: Selector
supportedBitDepthsSelector = mkSelector "supportedBitDepths"

-- | @Selector@ for @bitDepth@
bitDepthSelector :: Selector
bitDepthSelector = mkSelector "bitDepth"

-- | @Selector@ for @setBitDepth:@
setBitDepthSelector :: Selector
setBitDepthSelector = mkSelector "setBitDepth:"

-- | @Selector@ for @supportedMeasurementUnits@
supportedMeasurementUnitsSelector :: Selector
supportedMeasurementUnitsSelector = mkSelector "supportedMeasurementUnits"

-- | @Selector@ for @measurementUnit@
measurementUnitSelector :: Selector
measurementUnitSelector = mkSelector "measurementUnit"

-- | @Selector@ for @setMeasurementUnit:@
setMeasurementUnitSelector :: Selector
setMeasurementUnitSelector = mkSelector "setMeasurementUnit:"

-- | @Selector@ for @supportedResolutions@
supportedResolutionsSelector :: Selector
supportedResolutionsSelector = mkSelector "supportedResolutions"

-- | @Selector@ for @preferredResolutions@
preferredResolutionsSelector :: Selector
preferredResolutionsSelector = mkSelector "preferredResolutions"

-- | @Selector@ for @resolution@
resolutionSelector :: Selector
resolutionSelector = mkSelector "resolution"

-- | @Selector@ for @setResolution:@
setResolutionSelector :: Selector
setResolutionSelector = mkSelector "setResolution:"

-- | @Selector@ for @nativeXResolution@
nativeXResolutionSelector :: Selector
nativeXResolutionSelector = mkSelector "nativeXResolution"

-- | @Selector@ for @nativeYResolution@
nativeYResolutionSelector :: Selector
nativeYResolutionSelector = mkSelector "nativeYResolution"

-- | @Selector@ for @supportedScaleFactors@
supportedScaleFactorsSelector :: Selector
supportedScaleFactorsSelector = mkSelector "supportedScaleFactors"

-- | @Selector@ for @preferredScaleFactors@
preferredScaleFactorsSelector :: Selector
preferredScaleFactorsSelector = mkSelector "preferredScaleFactors"

-- | @Selector@ for @scaleFactor@
scaleFactorSelector :: Selector
scaleFactorSelector = mkSelector "scaleFactor"

-- | @Selector@ for @setScaleFactor:@
setScaleFactorSelector :: Selector
setScaleFactorSelector = mkSelector "setScaleFactor:"

-- | @Selector@ for @templates@
templatesSelector :: Selector
templatesSelector = mkSelector "templates"

-- | @Selector@ for @vendorFeatures@
vendorFeaturesSelector :: Selector
vendorFeaturesSelector = mkSelector "vendorFeatures"

-- | @Selector@ for @physicalSize@
physicalSizeSelector :: Selector
physicalSizeSelector = mkSelector "physicalSize"

-- | @Selector@ for @scanArea@
scanAreaSelector :: Selector
scanAreaSelector = mkSelector "scanArea"

-- | @Selector@ for @setScanArea:@
setScanAreaSelector :: Selector
setScanAreaSelector = mkSelector "setScanArea:"

-- | @Selector@ for @scanAreaOrientation@
scanAreaOrientationSelector :: Selector
scanAreaOrientationSelector = mkSelector "scanAreaOrientation"

-- | @Selector@ for @setScanAreaOrientation:@
setScanAreaOrientationSelector :: Selector
setScanAreaOrientationSelector = mkSelector "setScanAreaOrientation:"

-- | @Selector@ for @acceptsThresholdForBlackAndWhiteScanning@
acceptsThresholdForBlackAndWhiteScanningSelector :: Selector
acceptsThresholdForBlackAndWhiteScanningSelector = mkSelector "acceptsThresholdForBlackAndWhiteScanning"

-- | @Selector@ for @usesThresholdForBlackAndWhiteScanning@
usesThresholdForBlackAndWhiteScanningSelector :: Selector
usesThresholdForBlackAndWhiteScanningSelector = mkSelector "usesThresholdForBlackAndWhiteScanning"

-- | @Selector@ for @setUsesThresholdForBlackAndWhiteScanning:@
setUsesThresholdForBlackAndWhiteScanningSelector :: Selector
setUsesThresholdForBlackAndWhiteScanningSelector = mkSelector "setUsesThresholdForBlackAndWhiteScanning:"

-- | @Selector@ for @defaultThresholdForBlackAndWhiteScanning@
defaultThresholdForBlackAndWhiteScanningSelector :: Selector
defaultThresholdForBlackAndWhiteScanningSelector = mkSelector "defaultThresholdForBlackAndWhiteScanning"

-- | @Selector@ for @thresholdForBlackAndWhiteScanning@
thresholdForBlackAndWhiteScanningSelector :: Selector
thresholdForBlackAndWhiteScanningSelector = mkSelector "thresholdForBlackAndWhiteScanning"

-- | @Selector@ for @setThresholdForBlackAndWhiteScanning:@
setThresholdForBlackAndWhiteScanningSelector :: Selector
setThresholdForBlackAndWhiteScanningSelector = mkSelector "setThresholdForBlackAndWhiteScanning:"

-- | @Selector@ for @state@
stateSelector :: Selector
stateSelector = mkSelector "state"

-- | @Selector@ for @scanInProgress@
scanInProgressSelector :: Selector
scanInProgressSelector = mkSelector "scanInProgress"

-- | @Selector@ for @scanProgressPercentDone@
scanProgressPercentDoneSelector :: Selector
scanProgressPercentDoneSelector = mkSelector "scanProgressPercentDone"

-- | @Selector@ for @canPerformOverviewScan@
canPerformOverviewScanSelector :: Selector
canPerformOverviewScanSelector = mkSelector "canPerformOverviewScan"

-- | @Selector@ for @overviewScanInProgress@
overviewScanInProgressSelector :: Selector
overviewScanInProgressSelector = mkSelector "overviewScanInProgress"

-- | @Selector@ for @overviewImage@
overviewImageSelector :: Selector
overviewImageSelector = mkSelector "overviewImage"

-- | @Selector@ for @overviewResolution@
overviewResolutionSelector :: Selector
overviewResolutionSelector = mkSelector "overviewResolution"

-- | @Selector@ for @setOverviewResolution:@
setOverviewResolutionSelector :: Selector
setOverviewResolutionSelector = mkSelector "setOverviewResolution:"

