{-# LANGUAGE DataKinds #-}
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
  , cameraCalibrationDataLensCollectionSelector
  , cameraSystemBaselineSelector
  , disparityAdjustmentSelector
  , horizontalFieldOfViewSelector
  , initSelector
  , initWithFormatDescriptionSelector
  , setCameraCalibrationDataLensCollectionSelector
  , setCameraSystemBaselineSelector
  , setDisparityAdjustmentSelector
  , setHorizontalFieldOfViewSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVSpatialVideoConfiguration avSpatialVideoConfiguration => avSpatialVideoConfiguration -> IO (Id AVSpatialVideoConfiguration)
init_ avSpatialVideoConfiguration =
  sendOwnedMessage avSpatialVideoConfiguration initSelector

-- | Initializes an AVSpatialVideoConfiguration with a format description.
--
-- The format description is not stored. - Parameter formatDescription: Format description to use to initialize the AVSpatialVideoConfiguration.
--
-- - Returns: An instance of AVSpatialVideoConfiguration
--
-- ObjC selector: @- initWithFormatDescription:@
initWithFormatDescription :: IsAVSpatialVideoConfiguration avSpatialVideoConfiguration => avSpatialVideoConfiguration -> RawId -> IO (Id AVSpatialVideoConfiguration)
initWithFormatDescription avSpatialVideoConfiguration formatDescription =
  sendOwnedMessage avSpatialVideoConfiguration initWithFormatDescriptionSelector formatDescription

-- | Specifies intrinsic and extrinsic parameters for single or multiple lenses.
--
-- The property value is an array of dictionaries describing the camera calibration data for each lens. The camera calibration data includes intrinsics and extrinics with other parameters.  This property is only applicable when the projection kind is kCMTagProjectionTypeParametricImmersive.  Can be nil if the value is unknown.
--
-- ObjC selector: @- cameraCalibrationDataLensCollection@
cameraCalibrationDataLensCollection :: IsAVSpatialVideoConfiguration avSpatialVideoConfiguration => avSpatialVideoConfiguration -> IO (Id NSArray)
cameraCalibrationDataLensCollection avSpatialVideoConfiguration =
  sendMessage avSpatialVideoConfiguration cameraCalibrationDataLensCollectionSelector

-- | Specifies intrinsic and extrinsic parameters for single or multiple lenses.
--
-- The property value is an array of dictionaries describing the camera calibration data for each lens. The camera calibration data includes intrinsics and extrinics with other parameters.  This property is only applicable when the projection kind is kCMTagProjectionTypeParametricImmersive.  Can be nil if the value is unknown.
--
-- ObjC selector: @- setCameraCalibrationDataLensCollection:@
setCameraCalibrationDataLensCollection :: (IsAVSpatialVideoConfiguration avSpatialVideoConfiguration, IsNSArray value) => avSpatialVideoConfiguration -> value -> IO ()
setCameraCalibrationDataLensCollection avSpatialVideoConfiguration value =
  sendMessage avSpatialVideoConfiguration setCameraCalibrationDataLensCollectionSelector (toNSArray value)

-- | Specifies horizontal field of view in thousandths of a degree. Can be nil if the value is unknown.
--
-- ObjC selector: @- horizontalFieldOfView@
horizontalFieldOfView :: IsAVSpatialVideoConfiguration avSpatialVideoConfiguration => avSpatialVideoConfiguration -> IO (Id NSNumber)
horizontalFieldOfView avSpatialVideoConfiguration =
  sendMessage avSpatialVideoConfiguration horizontalFieldOfViewSelector

-- | Specifies horizontal field of view in thousandths of a degree. Can be nil if the value is unknown.
--
-- ObjC selector: @- setHorizontalFieldOfView:@
setHorizontalFieldOfView :: (IsAVSpatialVideoConfiguration avSpatialVideoConfiguration, IsNSNumber value) => avSpatialVideoConfiguration -> value -> IO ()
setHorizontalFieldOfView avSpatialVideoConfiguration value =
  sendMessage avSpatialVideoConfiguration setHorizontalFieldOfViewSelector (toNSNumber value)

-- | Specifies the distance between centers of the lenses of the camera system that created the video.
--
-- The distance is in micrometers or thousandths of a millimeter. Can be nil if the value is unknown.
--
-- ObjC selector: @- cameraSystemBaseline@
cameraSystemBaseline :: IsAVSpatialVideoConfiguration avSpatialVideoConfiguration => avSpatialVideoConfiguration -> IO (Id NSNumber)
cameraSystemBaseline avSpatialVideoConfiguration =
  sendMessage avSpatialVideoConfiguration cameraSystemBaselineSelector

-- | Specifies the distance between centers of the lenses of the camera system that created the video.
--
-- The distance is in micrometers or thousandths of a millimeter. Can be nil if the value is unknown.
--
-- ObjC selector: @- setCameraSystemBaseline:@
setCameraSystemBaseline :: (IsAVSpatialVideoConfiguration avSpatialVideoConfiguration, IsNSNumber value) => avSpatialVideoConfiguration -> value -> IO ()
setCameraSystemBaseline avSpatialVideoConfiguration value =
  sendMessage avSpatialVideoConfiguration setCameraSystemBaselineSelector (toNSNumber value)

-- | Specifies a relative shift of the left and right images, which changes the zero parallax plane.
--
-- The value is in normalized image space and measured over the range of -10000 to 10000 mapping to the uniform range [-1.0...1.0]. The interval of 0.0 to 1.0 or 0 to 10000 maps onto the stereo eye view image width. The negative interval 0.0 to -1.0 or 0 to -10000 similarly map onto the stereo eye view image width. Can be nil if the value is unknown.
--
-- ObjC selector: @- disparityAdjustment@
disparityAdjustment :: IsAVSpatialVideoConfiguration avSpatialVideoConfiguration => avSpatialVideoConfiguration -> IO (Id NSNumber)
disparityAdjustment avSpatialVideoConfiguration =
  sendMessage avSpatialVideoConfiguration disparityAdjustmentSelector

-- | Specifies a relative shift of the left and right images, which changes the zero parallax plane.
--
-- The value is in normalized image space and measured over the range of -10000 to 10000 mapping to the uniform range [-1.0...1.0]. The interval of 0.0 to 1.0 or 0 to 10000 maps onto the stereo eye view image width. The negative interval 0.0 to -1.0 or 0 to -10000 similarly map onto the stereo eye view image width. Can be nil if the value is unknown.
--
-- ObjC selector: @- setDisparityAdjustment:@
setDisparityAdjustment :: (IsAVSpatialVideoConfiguration avSpatialVideoConfiguration, IsNSNumber value) => avSpatialVideoConfiguration -> value -> IO ()
setDisparityAdjustment avSpatialVideoConfiguration value =
  sendMessage avSpatialVideoConfiguration setDisparityAdjustmentSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVSpatialVideoConfiguration)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithFormatDescription:@
initWithFormatDescriptionSelector :: Selector '[RawId] (Id AVSpatialVideoConfiguration)
initWithFormatDescriptionSelector = mkSelector "initWithFormatDescription:"

-- | @Selector@ for @cameraCalibrationDataLensCollection@
cameraCalibrationDataLensCollectionSelector :: Selector '[] (Id NSArray)
cameraCalibrationDataLensCollectionSelector = mkSelector "cameraCalibrationDataLensCollection"

-- | @Selector@ for @setCameraCalibrationDataLensCollection:@
setCameraCalibrationDataLensCollectionSelector :: Selector '[Id NSArray] ()
setCameraCalibrationDataLensCollectionSelector = mkSelector "setCameraCalibrationDataLensCollection:"

-- | @Selector@ for @horizontalFieldOfView@
horizontalFieldOfViewSelector :: Selector '[] (Id NSNumber)
horizontalFieldOfViewSelector = mkSelector "horizontalFieldOfView"

-- | @Selector@ for @setHorizontalFieldOfView:@
setHorizontalFieldOfViewSelector :: Selector '[Id NSNumber] ()
setHorizontalFieldOfViewSelector = mkSelector "setHorizontalFieldOfView:"

-- | @Selector@ for @cameraSystemBaseline@
cameraSystemBaselineSelector :: Selector '[] (Id NSNumber)
cameraSystemBaselineSelector = mkSelector "cameraSystemBaseline"

-- | @Selector@ for @setCameraSystemBaseline:@
setCameraSystemBaselineSelector :: Selector '[Id NSNumber] ()
setCameraSystemBaselineSelector = mkSelector "setCameraSystemBaseline:"

-- | @Selector@ for @disparityAdjustment@
disparityAdjustmentSelector :: Selector '[] (Id NSNumber)
disparityAdjustmentSelector = mkSelector "disparityAdjustment"

-- | @Selector@ for @setDisparityAdjustment:@
setDisparityAdjustmentSelector :: Selector '[Id NSNumber] ()
setDisparityAdjustmentSelector = mkSelector "setDisparityAdjustment:"

