{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCameraCalibrationData
--
-- AVCameraCalibrationData is a model object describing a camera's calibration information.
--
-- When rendering effects to images produced by cameras, or performing computer vision tasks such as correcting images for geometric distortions, it is necessary to characterize the camera's calibration information, such as its pixel focal length, principal point, lens distortion characteristics, etc. AVCameraCalibrationData provides this information.
--
-- Generated bindings for @AVCameraCalibrationData@.
module ObjC.AVFoundation.AVCameraCalibrationData
  ( AVCameraCalibrationData
  , IsAVCameraCalibrationData(..)
  , init_
  , new
  , pixelSize
  , lensDistortionLookupTable
  , inverseLensDistortionLookupTable
  , initSelector
  , inverseLensDistortionLookupTableSelector
  , lensDistortionLookupTableSelector
  , newSelector
  , pixelSizeSelector


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
init_ :: IsAVCameraCalibrationData avCameraCalibrationData => avCameraCalibrationData -> IO (Id AVCameraCalibrationData)
init_ avCameraCalibrationData =
  sendOwnedMessage avCameraCalibrationData initSelector

-- | @+ new@
new :: IO (Id AVCameraCalibrationData)
new  =
  do
    cls' <- getRequiredClass "AVCameraCalibrationData"
    sendOwnedClassMessage cls' newSelector

-- | pixelSize
--
-- The size of one pixel at intrinsicMatrixReferenceDimensions in millimeters.
--
-- ObjC selector: @- pixelSize@
pixelSize :: IsAVCameraCalibrationData avCameraCalibrationData => avCameraCalibrationData -> IO CFloat
pixelSize avCameraCalibrationData =
  sendMessage avCameraCalibrationData pixelSizeSelector

-- | lensDistortionLookupTable
--
-- An NSData of floats describing the camera lens' radial distortions.
--
-- Images captured by a camera are geometrically warped by radial distortions in the lens. In order to project from the 2D image plane back into the 3D world, the images must be distortion corrected, or made rectilinear. Lens distortion is modeled using a one-dimensional lookup table of 32-bit float values evenly distributed along a radius from the center of the distortion to the farthest corner, with each value representing an elongation or compression of the radius (0.0 for any given point indicates no elongation). This model assumes radially symmetric lens distortion. When dealing with AVDepthData, the disparity / depth map representations are geometrically distorted to align with images produced by the camera. For more information, see the reference implementation below.
--
-- If the camera lacks the calibration data needed to accurately characterize lens distortions, this property's value is nil.
--
-- ObjC selector: @- lensDistortionLookupTable@
lensDistortionLookupTable :: IsAVCameraCalibrationData avCameraCalibrationData => avCameraCalibrationData -> IO (Id NSData)
lensDistortionLookupTable avCameraCalibrationData =
  sendMessage avCameraCalibrationData lensDistortionLookupTableSelector

-- | inverseLensDistortionLookupTable
--
-- An NSData of floats describing the inverse lookup table required to reapply the camera lens' radial distortions to a rectified image.
--
-- See lensDistortionLookupTable. If you've rectified an image by removing the distortions characterized by the lensDistortionLookupTable, and now wish to go back to geometrically distorted, you may use the inverseLensDistortionLookupTable. For more information, see the reference implementation below.
--
-- If the camera lacks the calibration data needed to accurately characterize lens distortions, this property's value is nil.
--
-- ObjC selector: @- inverseLensDistortionLookupTable@
inverseLensDistortionLookupTable :: IsAVCameraCalibrationData avCameraCalibrationData => avCameraCalibrationData -> IO (Id NSData)
inverseLensDistortionLookupTable avCameraCalibrationData =
  sendMessage avCameraCalibrationData inverseLensDistortionLookupTableSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVCameraCalibrationData)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVCameraCalibrationData)
newSelector = mkSelector "new"

-- | @Selector@ for @pixelSize@
pixelSizeSelector :: Selector '[] CFloat
pixelSizeSelector = mkSelector "pixelSize"

-- | @Selector@ for @lensDistortionLookupTable@
lensDistortionLookupTableSelector :: Selector '[] (Id NSData)
lensDistortionLookupTableSelector = mkSelector "lensDistortionLookupTable"

-- | @Selector@ for @inverseLensDistortionLookupTable@
inverseLensDistortionLookupTableSelector :: Selector '[] (Id NSData)
inverseLensDistortionLookupTableSelector = mkSelector "inverseLensDistortionLookupTable"

