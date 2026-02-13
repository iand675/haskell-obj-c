{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSImageScale
--
-- Resize an image and / or change its aspect ratio
--
-- The MPSImageScale filter can be used to resample an existing image              using a different sampling frequency in each dimension. This can be              used to enlarge or reduce the size of an image, or change the aspect              ratio of an image.
--
-- The resample methods supported are:                    Bilinear                    Bicubcic                    Lanczos
--
-- Generated bindings for @MPSImageScale@.
module ObjC.MetalPerformanceShaders.MPSImageScale
  ( MPSImageScale
  , IsMPSImageScale(..)
  , initWithDevice
  , initWithCoder_device
  , scaleTransform
  , setScaleTransform
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , scaleTransformSelector
  , setScaleTransformSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.MetalPerformanceShaders.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- initWithDevice:@
initWithDevice :: IsMPSImageScale mpsImageScale => mpsImageScale -> RawId -> IO (Id MPSImageScale)
initWithDevice mpsImageScale device =
  sendOwnedMessage mpsImageScale initWithDeviceSelector device

-- | NSSecureCoding compatability
--
-- While the standard NSSecureCoding/NSCoding method              -initWithCoder: should work, since the file can't              know which device your data is allocated on, we              have to guess and may guess incorrectly.  To avoid              that problem, use initWithCoder:device instead.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSKernel
--
-- @device@ — The MTLDevice on which to make the MPSKernel
--
-- Returns: A new MPSKernel object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSImageScale mpsImageScale, IsNSCoder aDecoder) => mpsImageScale -> aDecoder -> RawId -> IO (Id MPSImageScale)
initWithCoder_device mpsImageScale aDecoder device =
  sendOwnedMessage mpsImageScale initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- | scaleTransform
--
-- An optional transform that describes how to scale and translate the source image
--
-- If the scaleTransform is NULL, then any image scaling factor such as MPSImageLanczosScale              will rescale the image so that the source image fits exactly into the destination              texture.  If the transform is not NULL, then the transform is used for determining              how to map the source image to the destination. Default: NULL
--
-- When the scaleTransform is set to non-NULL, the values pointed to by the new              scaleTransform are copied to object storage, and the pointer is updated to point              to internal storage. Do not attempt to free it.  You may free your copy of              the MPSScaleTransform as soon as the property set operation is complete.
--
-- When calculating a scaleTransform, use the limits of the bounding box for the intended              source region of interest and the destination clipRect. Adjustments for pixel center              coordinates are handled internally to the function.  For example,              the scale transform to convert the entire source image to the entire destination image              size (clipRect = MPSRectNoClip) would be:
--
-- scaleTransform.scaleX = (double) dest.width / source.width;
-- scaleTransform.scaleY = (double) dest.height / source.height;
-- scaleTransform.translateX = scaleTransform.translateY = 0.0;
--
-- The translation parameters allow you to adjust the region of the source image used              to create the destination image. They are in destination coordinates. To place the              top left corner of the destination clipRect to represent the position {x,y} in source              coordinates, we solve for the translation based on the standard scale matrix operation              for each axis:
--
-- dest_position = source_position * scale + translation;
-- translation = dest_position - source_position * scale;
--
-- For the top left corner of the clipRect, the dest_position is considered to be {0,0}.              This gives us a translation of:
--
-- scaleTransform.translateX = -source_origin.x * scaleTransform.scaleX;
-- scaleTransform.translateY = -source_origin.y * scaleTransform.scaleY;
--
-- One would typically use non-zero translations to do tiling, or provide a resized              view into a internal segment of an image.
--
-- NOTE:  Changing the Lanczos scale factor may trigger recalculation of signficant state internal              to the object when the filter is encoded to the command buffer. The scale factor is              scaleTransform->scaleX,Y, or the ratio of source and destination image sizes if              scaleTransform is NULL. Reuse a MPSImageLanczosScale object for frequently used scalings              to avoid redundantly recreating expensive resampling state.
--
-- ObjC selector: @- scaleTransform@
scaleTransform :: IsMPSImageScale mpsImageScale => mpsImageScale -> IO (Const (Ptr MPSScaleTransform))
scaleTransform mpsImageScale =
  sendMessage mpsImageScale scaleTransformSelector

-- | scaleTransform
--
-- An optional transform that describes how to scale and translate the source image
--
-- If the scaleTransform is NULL, then any image scaling factor such as MPSImageLanczosScale              will rescale the image so that the source image fits exactly into the destination              texture.  If the transform is not NULL, then the transform is used for determining              how to map the source image to the destination. Default: NULL
--
-- When the scaleTransform is set to non-NULL, the values pointed to by the new              scaleTransform are copied to object storage, and the pointer is updated to point              to internal storage. Do not attempt to free it.  You may free your copy of              the MPSScaleTransform as soon as the property set operation is complete.
--
-- When calculating a scaleTransform, use the limits of the bounding box for the intended              source region of interest and the destination clipRect. Adjustments for pixel center              coordinates are handled internally to the function.  For example,              the scale transform to convert the entire source image to the entire destination image              size (clipRect = MPSRectNoClip) would be:
--
-- scaleTransform.scaleX = (double) dest.width / source.width;
-- scaleTransform.scaleY = (double) dest.height / source.height;
-- scaleTransform.translateX = scaleTransform.translateY = 0.0;
--
-- The translation parameters allow you to adjust the region of the source image used              to create the destination image. They are in destination coordinates. To place the              top left corner of the destination clipRect to represent the position {x,y} in source              coordinates, we solve for the translation based on the standard scale matrix operation              for each axis:
--
-- dest_position = source_position * scale + translation;
-- translation = dest_position - source_position * scale;
--
-- For the top left corner of the clipRect, the dest_position is considered to be {0,0}.              This gives us a translation of:
--
-- scaleTransform.translateX = -source_origin.x * scaleTransform.scaleX;
-- scaleTransform.translateY = -source_origin.y * scaleTransform.scaleY;
--
-- One would typically use non-zero translations to do tiling, or provide a resized              view into a internal segment of an image.
--
-- NOTE:  Changing the Lanczos scale factor may trigger recalculation of signficant state internal              to the object when the filter is encoded to the command buffer. The scale factor is              scaleTransform->scaleX,Y, or the ratio of source and destination image sizes if              scaleTransform is NULL. Reuse a MPSImageLanczosScale object for frequently used scalings              to avoid redundantly recreating expensive resampling state.
--
-- ObjC selector: @- setScaleTransform:@
setScaleTransform :: IsMPSImageScale mpsImageScale => mpsImageScale -> Const (Ptr MPSScaleTransform) -> IO ()
setScaleTransform mpsImageScale value =
  sendMessage mpsImageScale setScaleTransformSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSImageScale)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSImageScale)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @scaleTransform@
scaleTransformSelector :: Selector '[] (Const (Ptr MPSScaleTransform))
scaleTransformSelector = mkSelector "scaleTransform"

-- | @Selector@ for @setScaleTransform:@
setScaleTransformSelector :: Selector '[Const (Ptr MPSScaleTransform)] ()
setScaleTransformSelector = mkSelector "setScaleTransform:"

