{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNPyramid
--
-- SCNPyramid represents a right pyramid with a rectangular base.
--
-- Generated bindings for @SCNPyramid@.
module ObjC.SceneKit.SCNPyramid
  ( SCNPyramid
  , IsSCNPyramid(..)
  , pyramidWithWidth_height_length
  , width
  , setWidth
  , height
  , setHeight
  , length_
  , setLength
  , widthSegmentCount
  , setWidthSegmentCount
  , heightSegmentCount
  , setHeightSegmentCount
  , lengthSegmentCount
  , setLengthSegmentCount
  , heightSegmentCountSelector
  , heightSelector
  , lengthSegmentCountSelector
  , lengthSelector
  , pyramidWithWidth_height_lengthSelector
  , setHeightSegmentCountSelector
  , setHeightSelector
  , setLengthSegmentCountSelector
  , setLengthSelector
  , setWidthSegmentCountSelector
  , setWidthSelector
  , widthSegmentCountSelector
  , widthSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SceneKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | pyramidWithWidth:height:length:
--
-- Creates and returns a pyramid with given width, height, and length.
--
-- @width@ — The width of the pyramid.
--
-- @height@ — The height of the pyramid.
--
-- @length@ — The length of the pyramid.
--
-- ObjC selector: @+ pyramidWithWidth:height:length:@
pyramidWithWidth_height_length :: CDouble -> CDouble -> CDouble -> IO (Id SCNPyramid)
pyramidWithWidth_height_length width height length_ =
  do
    cls' <- getRequiredClass "SCNPyramid"
    sendClassMessage cls' pyramidWithWidth_height_lengthSelector width height length_

-- | width
--
-- The width of the pyramid base. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 1.
--
-- ObjC selector: @- width@
width :: IsSCNPyramid scnPyramid => scnPyramid -> IO CDouble
width scnPyramid =
  sendMessage scnPyramid widthSelector

-- | width
--
-- The width of the pyramid base. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 1.
--
-- ObjC selector: @- setWidth:@
setWidth :: IsSCNPyramid scnPyramid => scnPyramid -> CDouble -> IO ()
setWidth scnPyramid value =
  sendMessage scnPyramid setWidthSelector value

-- | height
--
-- The height of the pyramid. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 1.
--
-- ObjC selector: @- height@
height :: IsSCNPyramid scnPyramid => scnPyramid -> IO CDouble
height scnPyramid =
  sendMessage scnPyramid heightSelector

-- | height
--
-- The height of the pyramid. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 1.
--
-- ObjC selector: @- setHeight:@
setHeight :: IsSCNPyramid scnPyramid => scnPyramid -> CDouble -> IO ()
setHeight scnPyramid value =
  sendMessage scnPyramid setHeightSelector value

-- | length
--
-- The length of the pyramid base. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 1.
--
-- ObjC selector: @- length@
length_ :: IsSCNPyramid scnPyramid => scnPyramid -> IO CDouble
length_ scnPyramid =
  sendMessage scnPyramid lengthSelector

-- | length
--
-- The length of the pyramid base. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 1.
--
-- ObjC selector: @- setLength:@
setLength :: IsSCNPyramid scnPyramid => scnPyramid -> CDouble -> IO ()
setLength scnPyramid value =
  sendMessage scnPyramid setLengthSelector value

-- | widthSegmentCount
--
-- The number of subdivisions along the X axis. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 1.
--
-- ObjC selector: @- widthSegmentCount@
widthSegmentCount :: IsSCNPyramid scnPyramid => scnPyramid -> IO CLong
widthSegmentCount scnPyramid =
  sendMessage scnPyramid widthSegmentCountSelector

-- | widthSegmentCount
--
-- The number of subdivisions along the X axis. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 1.
--
-- ObjC selector: @- setWidthSegmentCount:@
setWidthSegmentCount :: IsSCNPyramid scnPyramid => scnPyramid -> CLong -> IO ()
setWidthSegmentCount scnPyramid value =
  sendMessage scnPyramid setWidthSegmentCountSelector value

-- | heightSegmentCount
--
-- The number of subdivisions along the Y axis. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 1.
--
-- ObjC selector: @- heightSegmentCount@
heightSegmentCount :: IsSCNPyramid scnPyramid => scnPyramid -> IO CLong
heightSegmentCount scnPyramid =
  sendMessage scnPyramid heightSegmentCountSelector

-- | heightSegmentCount
--
-- The number of subdivisions along the Y axis. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 1.
--
-- ObjC selector: @- setHeightSegmentCount:@
setHeightSegmentCount :: IsSCNPyramid scnPyramid => scnPyramid -> CLong -> IO ()
setHeightSegmentCount scnPyramid value =
  sendMessage scnPyramid setHeightSegmentCountSelector value

-- | lengthSegmentCount
--
-- The number of subdivisions along the Z axis. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 1.
--
-- ObjC selector: @- lengthSegmentCount@
lengthSegmentCount :: IsSCNPyramid scnPyramid => scnPyramid -> IO CLong
lengthSegmentCount scnPyramid =
  sendMessage scnPyramid lengthSegmentCountSelector

-- | lengthSegmentCount
--
-- The number of subdivisions along the Z axis. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 1.
--
-- ObjC selector: @- setLengthSegmentCount:@
setLengthSegmentCount :: IsSCNPyramid scnPyramid => scnPyramid -> CLong -> IO ()
setLengthSegmentCount scnPyramid value =
  sendMessage scnPyramid setLengthSegmentCountSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @pyramidWithWidth:height:length:@
pyramidWithWidth_height_lengthSelector :: Selector '[CDouble, CDouble, CDouble] (Id SCNPyramid)
pyramidWithWidth_height_lengthSelector = mkSelector "pyramidWithWidth:height:length:"

-- | @Selector@ for @width@
widthSelector :: Selector '[] CDouble
widthSelector = mkSelector "width"

-- | @Selector@ for @setWidth:@
setWidthSelector :: Selector '[CDouble] ()
setWidthSelector = mkSelector "setWidth:"

-- | @Selector@ for @height@
heightSelector :: Selector '[] CDouble
heightSelector = mkSelector "height"

-- | @Selector@ for @setHeight:@
setHeightSelector :: Selector '[CDouble] ()
setHeightSelector = mkSelector "setHeight:"

-- | @Selector@ for @length@
lengthSelector :: Selector '[] CDouble
lengthSelector = mkSelector "length"

-- | @Selector@ for @setLength:@
setLengthSelector :: Selector '[CDouble] ()
setLengthSelector = mkSelector "setLength:"

-- | @Selector@ for @widthSegmentCount@
widthSegmentCountSelector :: Selector '[] CLong
widthSegmentCountSelector = mkSelector "widthSegmentCount"

-- | @Selector@ for @setWidthSegmentCount:@
setWidthSegmentCountSelector :: Selector '[CLong] ()
setWidthSegmentCountSelector = mkSelector "setWidthSegmentCount:"

-- | @Selector@ for @heightSegmentCount@
heightSegmentCountSelector :: Selector '[] CLong
heightSegmentCountSelector = mkSelector "heightSegmentCount"

-- | @Selector@ for @setHeightSegmentCount:@
setHeightSegmentCountSelector :: Selector '[CLong] ()
setHeightSegmentCountSelector = mkSelector "setHeightSegmentCount:"

-- | @Selector@ for @lengthSegmentCount@
lengthSegmentCountSelector :: Selector '[] CLong
lengthSegmentCountSelector = mkSelector "lengthSegmentCount"

-- | @Selector@ for @setLengthSegmentCount:@
setLengthSegmentCountSelector :: Selector '[CLong] ()
setLengthSegmentCountSelector = mkSelector "setLengthSegmentCount:"

