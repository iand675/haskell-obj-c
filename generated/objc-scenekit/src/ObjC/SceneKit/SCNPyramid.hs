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
  , pyramidWithWidth_height_lengthSelector
  , widthSelector
  , setWidthSelector
  , heightSelector
  , setHeightSelector
  , lengthSelector
  , setLengthSelector
  , widthSegmentCountSelector
  , setWidthSegmentCountSelector
  , heightSegmentCountSelector
  , setHeightSegmentCountSelector
  , lengthSegmentCountSelector
  , setLengthSegmentCountSelector


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
    sendClassMsg cls' (mkSelector "pyramidWithWidth:height:length:") (retPtr retVoid) [argCDouble (fromIntegral width), argCDouble (fromIntegral height), argCDouble (fromIntegral length_)] >>= retainedObject . castPtr

-- | width
--
-- The width of the pyramid base. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 1.
--
-- ObjC selector: @- width@
width :: IsSCNPyramid scnPyramid => scnPyramid -> IO CDouble
width scnPyramid  =
  sendMsg scnPyramid (mkSelector "width") retCDouble []

-- | width
--
-- The width of the pyramid base. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 1.
--
-- ObjC selector: @- setWidth:@
setWidth :: IsSCNPyramid scnPyramid => scnPyramid -> CDouble -> IO ()
setWidth scnPyramid  value =
  sendMsg scnPyramid (mkSelector "setWidth:") retVoid [argCDouble (fromIntegral value)]

-- | height
--
-- The height of the pyramid. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 1.
--
-- ObjC selector: @- height@
height :: IsSCNPyramid scnPyramid => scnPyramid -> IO CDouble
height scnPyramid  =
  sendMsg scnPyramid (mkSelector "height") retCDouble []

-- | height
--
-- The height of the pyramid. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 1.
--
-- ObjC selector: @- setHeight:@
setHeight :: IsSCNPyramid scnPyramid => scnPyramid -> CDouble -> IO ()
setHeight scnPyramid  value =
  sendMsg scnPyramid (mkSelector "setHeight:") retVoid [argCDouble (fromIntegral value)]

-- | length
--
-- The length of the pyramid base. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 1.
--
-- ObjC selector: @- length@
length_ :: IsSCNPyramid scnPyramid => scnPyramid -> IO CDouble
length_ scnPyramid  =
  sendMsg scnPyramid (mkSelector "length") retCDouble []

-- | length
--
-- The length of the pyramid base. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 1.
--
-- ObjC selector: @- setLength:@
setLength :: IsSCNPyramid scnPyramid => scnPyramid -> CDouble -> IO ()
setLength scnPyramid  value =
  sendMsg scnPyramid (mkSelector "setLength:") retVoid [argCDouble (fromIntegral value)]

-- | widthSegmentCount
--
-- The number of subdivisions along the X axis. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 1.
--
-- ObjC selector: @- widthSegmentCount@
widthSegmentCount :: IsSCNPyramid scnPyramid => scnPyramid -> IO CLong
widthSegmentCount scnPyramid  =
  sendMsg scnPyramid (mkSelector "widthSegmentCount") retCLong []

-- | widthSegmentCount
--
-- The number of subdivisions along the X axis. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 1.
--
-- ObjC selector: @- setWidthSegmentCount:@
setWidthSegmentCount :: IsSCNPyramid scnPyramid => scnPyramid -> CLong -> IO ()
setWidthSegmentCount scnPyramid  value =
  sendMsg scnPyramid (mkSelector "setWidthSegmentCount:") retVoid [argCLong (fromIntegral value)]

-- | heightSegmentCount
--
-- The number of subdivisions along the Y axis. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 1.
--
-- ObjC selector: @- heightSegmentCount@
heightSegmentCount :: IsSCNPyramid scnPyramid => scnPyramid -> IO CLong
heightSegmentCount scnPyramid  =
  sendMsg scnPyramid (mkSelector "heightSegmentCount") retCLong []

-- | heightSegmentCount
--
-- The number of subdivisions along the Y axis. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 1.
--
-- ObjC selector: @- setHeightSegmentCount:@
setHeightSegmentCount :: IsSCNPyramid scnPyramid => scnPyramid -> CLong -> IO ()
setHeightSegmentCount scnPyramid  value =
  sendMsg scnPyramid (mkSelector "setHeightSegmentCount:") retVoid [argCLong (fromIntegral value)]

-- | lengthSegmentCount
--
-- The number of subdivisions along the Z axis. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 1.
--
-- ObjC selector: @- lengthSegmentCount@
lengthSegmentCount :: IsSCNPyramid scnPyramid => scnPyramid -> IO CLong
lengthSegmentCount scnPyramid  =
  sendMsg scnPyramid (mkSelector "lengthSegmentCount") retCLong []

-- | lengthSegmentCount
--
-- The number of subdivisions along the Z axis. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 1.
--
-- ObjC selector: @- setLengthSegmentCount:@
setLengthSegmentCount :: IsSCNPyramid scnPyramid => scnPyramid -> CLong -> IO ()
setLengthSegmentCount scnPyramid  value =
  sendMsg scnPyramid (mkSelector "setLengthSegmentCount:") retVoid [argCLong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @pyramidWithWidth:height:length:@
pyramidWithWidth_height_lengthSelector :: Selector
pyramidWithWidth_height_lengthSelector = mkSelector "pyramidWithWidth:height:length:"

-- | @Selector@ for @width@
widthSelector :: Selector
widthSelector = mkSelector "width"

-- | @Selector@ for @setWidth:@
setWidthSelector :: Selector
setWidthSelector = mkSelector "setWidth:"

-- | @Selector@ for @height@
heightSelector :: Selector
heightSelector = mkSelector "height"

-- | @Selector@ for @setHeight:@
setHeightSelector :: Selector
setHeightSelector = mkSelector "setHeight:"

-- | @Selector@ for @length@
lengthSelector :: Selector
lengthSelector = mkSelector "length"

-- | @Selector@ for @setLength:@
setLengthSelector :: Selector
setLengthSelector = mkSelector "setLength:"

-- | @Selector@ for @widthSegmentCount@
widthSegmentCountSelector :: Selector
widthSegmentCountSelector = mkSelector "widthSegmentCount"

-- | @Selector@ for @setWidthSegmentCount:@
setWidthSegmentCountSelector :: Selector
setWidthSegmentCountSelector = mkSelector "setWidthSegmentCount:"

-- | @Selector@ for @heightSegmentCount@
heightSegmentCountSelector :: Selector
heightSegmentCountSelector = mkSelector "heightSegmentCount"

-- | @Selector@ for @setHeightSegmentCount:@
setHeightSegmentCountSelector :: Selector
setHeightSegmentCountSelector = mkSelector "setHeightSegmentCount:"

-- | @Selector@ for @lengthSegmentCount@
lengthSegmentCountSelector :: Selector
lengthSegmentCountSelector = mkSelector "lengthSegmentCount"

-- | @Selector@ for @setLengthSegmentCount:@
setLengthSegmentCountSelector :: Selector
setLengthSegmentCountSelector = mkSelector "setLengthSegmentCount:"

