{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An AVSpatialVideoConfiguration specifies spatial video properties.
--
-- Generated bindings for @AVSpatialVideoConfiguration@.
module ObjC.AVFoundation.AVSpatialVideoConfiguration
  ( AVSpatialVideoConfiguration
  , IsAVSpatialVideoConfiguration(..)
  , init_
  , initWithFormatDescription
  , cameraCalibrationDataLensCollection
  , setCameraCalibrationDataLensCollection
  , horizontalFieldOfView
  , setHorizontalFieldOfView
  , cameraSystemBaseline
  , setCameraSystemBaseline
  , disparityAdjustment
  , setDisparityAdjustment
  , initSelector
  , initWithFormatDescriptionSelector
  , cameraCalibrationDataLensCollectionSelector
  , setCameraCalibrationDataLensCollectionSelector
  , horizontalFieldOfViewSelector
  , setHorizontalFieldOfViewSelector
  , cameraSystemBaselineSelector
  , setCameraSystemBaselineSelector
  , disparityAdjustmentSelector
  , setDisparityAdjustmentSelector


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

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVSpatialVideoConfiguration avSpatialVideoConfiguration => avSpatialVideoConfiguration -> IO (Id AVSpatialVideoConfiguration)
init_ avSpatialVideoConfiguration  =
  sendMsg avSpatialVideoConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Initializes an AVSpatialVideoConfiguration with a format description.
--
-- The format description is not stored. - Parameter formatDescription: Format description to use to initialize the AVSpatialVideoConfiguration.
--
-- - Returns: An instance of AVSpatialVideoConfiguration
--
-- ObjC selector: @- initWithFormatDescription:@
initWithFormatDescription :: IsAVSpatialVideoConfiguration avSpatialVideoConfiguration => avSpatialVideoConfiguration -> RawId -> IO (Id AVSpatialVideoConfiguration)
initWithFormatDescription avSpatialVideoConfiguration  formatDescription =
  sendMsg avSpatialVideoConfiguration (mkSelector "initWithFormatDescription:") (retPtr retVoid) [argPtr (castPtr (unRawId formatDescription) :: Ptr ())] >>= ownedObject . castPtr

-- | Specifies intrinsic and extrinsic parameters for single or multiple lenses.
--
-- The property value is an array of dictionaries describing the camera calibration data for each lens. The camera calibration data includes intrinsics and extrinics with other parameters.  This property is only applicable when the projection kind is kCMTagProjectionTypeParametricImmersive.  Can be nil if the value is unknown.
--
-- ObjC selector: @- cameraCalibrationDataLensCollection@
cameraCalibrationDataLensCollection :: IsAVSpatialVideoConfiguration avSpatialVideoConfiguration => avSpatialVideoConfiguration -> IO (Id NSArray)
cameraCalibrationDataLensCollection avSpatialVideoConfiguration  =
  sendMsg avSpatialVideoConfiguration (mkSelector "cameraCalibrationDataLensCollection") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Specifies intrinsic and extrinsic parameters for single or multiple lenses.
--
-- The property value is an array of dictionaries describing the camera calibration data for each lens. The camera calibration data includes intrinsics and extrinics with other parameters.  This property is only applicable when the projection kind is kCMTagProjectionTypeParametricImmersive.  Can be nil if the value is unknown.
--
-- ObjC selector: @- setCameraCalibrationDataLensCollection:@
setCameraCalibrationDataLensCollection :: (IsAVSpatialVideoConfiguration avSpatialVideoConfiguration, IsNSArray value) => avSpatialVideoConfiguration -> value -> IO ()
setCameraCalibrationDataLensCollection avSpatialVideoConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg avSpatialVideoConfiguration (mkSelector "setCameraCalibrationDataLensCollection:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Specifies horizontal field of view in thousandths of a degree. Can be nil if the value is unknown.
--
-- ObjC selector: @- horizontalFieldOfView@
horizontalFieldOfView :: IsAVSpatialVideoConfiguration avSpatialVideoConfiguration => avSpatialVideoConfiguration -> IO (Id NSNumber)
horizontalFieldOfView avSpatialVideoConfiguration  =
  sendMsg avSpatialVideoConfiguration (mkSelector "horizontalFieldOfView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Specifies horizontal field of view in thousandths of a degree. Can be nil if the value is unknown.
--
-- ObjC selector: @- setHorizontalFieldOfView:@
setHorizontalFieldOfView :: (IsAVSpatialVideoConfiguration avSpatialVideoConfiguration, IsNSNumber value) => avSpatialVideoConfiguration -> value -> IO ()
setHorizontalFieldOfView avSpatialVideoConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg avSpatialVideoConfiguration (mkSelector "setHorizontalFieldOfView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Specifies the distance between centers of the lenses of the camera system that created the video.
--
-- The distance is in micrometers or thousandths of a millimeter. Can be nil if the value is unknown.
--
-- ObjC selector: @- cameraSystemBaseline@
cameraSystemBaseline :: IsAVSpatialVideoConfiguration avSpatialVideoConfiguration => avSpatialVideoConfiguration -> IO (Id NSNumber)
cameraSystemBaseline avSpatialVideoConfiguration  =
  sendMsg avSpatialVideoConfiguration (mkSelector "cameraSystemBaseline") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Specifies the distance between centers of the lenses of the camera system that created the video.
--
-- The distance is in micrometers or thousandths of a millimeter. Can be nil if the value is unknown.
--
-- ObjC selector: @- setCameraSystemBaseline:@
setCameraSystemBaseline :: (IsAVSpatialVideoConfiguration avSpatialVideoConfiguration, IsNSNumber value) => avSpatialVideoConfiguration -> value -> IO ()
setCameraSystemBaseline avSpatialVideoConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg avSpatialVideoConfiguration (mkSelector "setCameraSystemBaseline:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Specifies a relative shift of the left and right images, which changes the zero parallax plane.
--
-- The value is in normalized image space and measured over the range of -10000 to 10000 mapping to the uniform range [-1.0...1.0]. The interval of 0.0 to 1.0 or 0 to 10000 maps onto the stereo eye view image width. The negative interval 0.0 to -1.0 or 0 to -10000 similarly map onto the stereo eye view image width. Can be nil if the value is unknown.
--
-- ObjC selector: @- disparityAdjustment@
disparityAdjustment :: IsAVSpatialVideoConfiguration avSpatialVideoConfiguration => avSpatialVideoConfiguration -> IO (Id NSNumber)
disparityAdjustment avSpatialVideoConfiguration  =
  sendMsg avSpatialVideoConfiguration (mkSelector "disparityAdjustment") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Specifies a relative shift of the left and right images, which changes the zero parallax plane.
--
-- The value is in normalized image space and measured over the range of -10000 to 10000 mapping to the uniform range [-1.0...1.0]. The interval of 0.0 to 1.0 or 0 to 10000 maps onto the stereo eye view image width. The negative interval 0.0 to -1.0 or 0 to -10000 similarly map onto the stereo eye view image width. Can be nil if the value is unknown.
--
-- ObjC selector: @- setDisparityAdjustment:@
setDisparityAdjustment :: (IsAVSpatialVideoConfiguration avSpatialVideoConfiguration, IsNSNumber value) => avSpatialVideoConfiguration -> value -> IO ()
setDisparityAdjustment avSpatialVideoConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg avSpatialVideoConfiguration (mkSelector "setDisparityAdjustment:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithFormatDescription:@
initWithFormatDescriptionSelector :: Selector
initWithFormatDescriptionSelector = mkSelector "initWithFormatDescription:"

-- | @Selector@ for @cameraCalibrationDataLensCollection@
cameraCalibrationDataLensCollectionSelector :: Selector
cameraCalibrationDataLensCollectionSelector = mkSelector "cameraCalibrationDataLensCollection"

-- | @Selector@ for @setCameraCalibrationDataLensCollection:@
setCameraCalibrationDataLensCollectionSelector :: Selector
setCameraCalibrationDataLensCollectionSelector = mkSelector "setCameraCalibrationDataLensCollection:"

-- | @Selector@ for @horizontalFieldOfView@
horizontalFieldOfViewSelector :: Selector
horizontalFieldOfViewSelector = mkSelector "horizontalFieldOfView"

-- | @Selector@ for @setHorizontalFieldOfView:@
setHorizontalFieldOfViewSelector :: Selector
setHorizontalFieldOfViewSelector = mkSelector "setHorizontalFieldOfView:"

-- | @Selector@ for @cameraSystemBaseline@
cameraSystemBaselineSelector :: Selector
cameraSystemBaselineSelector = mkSelector "cameraSystemBaseline"

-- | @Selector@ for @setCameraSystemBaseline:@
setCameraSystemBaselineSelector :: Selector
setCameraSystemBaselineSelector = mkSelector "setCameraSystemBaseline:"

-- | @Selector@ for @disparityAdjustment@
disparityAdjustmentSelector :: Selector
disparityAdjustmentSelector = mkSelector "disparityAdjustment"

-- | @Selector@ for @setDisparityAdjustment:@
setDisparityAdjustmentSelector :: Selector
setDisparityAdjustmentSelector = mkSelector "setDisparityAdjustment:"

