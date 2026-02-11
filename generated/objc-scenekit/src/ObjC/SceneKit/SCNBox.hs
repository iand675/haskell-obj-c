{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNBox
--
-- SCNBox represents a box with rectangular sides and optional chamfers.
--
-- Generated bindings for @SCNBox@.
module ObjC.SceneKit.SCNBox
  ( SCNBox
  , IsSCNBox(..)
  , boxWithWidth_height_length_chamferRadius
  , width
  , setWidth
  , height
  , setHeight
  , length_
  , setLength
  , chamferRadius
  , setChamferRadius
  , widthSegmentCount
  , setWidthSegmentCount
  , heightSegmentCount
  , setHeightSegmentCount
  , lengthSegmentCount
  , setLengthSegmentCount
  , chamferSegmentCount
  , setChamferSegmentCount
  , boxWithWidth_height_length_chamferRadiusSelector
  , widthSelector
  , setWidthSelector
  , heightSelector
  , setHeightSelector
  , lengthSelector
  , setLengthSelector
  , chamferRadiusSelector
  , setChamferRadiusSelector
  , widthSegmentCountSelector
  , setWidthSegmentCountSelector
  , heightSegmentCountSelector
  , setHeightSegmentCountSelector
  , lengthSegmentCountSelector
  , setLengthSegmentCountSelector
  , chamferSegmentCountSelector
  , setChamferSegmentCountSelector


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

-- | boxWithWidth:height:length:chamferRadius:
--
-- Creates and returns a box with given width, height, length and chamfer radius.
--
-- @width@ — The width of the box.
--
-- @height@ — The height of the box.
--
-- @length@ — The length of the box.
--
-- @chamferRadius@ — The chamfer radius of the box.
--
-- ObjC selector: @+ boxWithWidth:height:length:chamferRadius:@
boxWithWidth_height_length_chamferRadius :: CDouble -> CDouble -> CDouble -> CDouble -> IO (Id SCNBox)
boxWithWidth_height_length_chamferRadius width height length_ chamferRadius =
  do
    cls' <- getRequiredClass "SCNBox"
    sendClassMsg cls' (mkSelector "boxWithWidth:height:length:chamferRadius:") (retPtr retVoid) [argCDouble (fromIntegral width), argCDouble (fromIntegral height), argCDouble (fromIntegral length_), argCDouble (fromIntegral chamferRadius)] >>= retainedObject . castPtr

-- | width
--
-- The width of the box. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 1.
--
-- ObjC selector: @- width@
width :: IsSCNBox scnBox => scnBox -> IO CDouble
width scnBox  =
  sendMsg scnBox (mkSelector "width") retCDouble []

-- | width
--
-- The width of the box. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 1.
--
-- ObjC selector: @- setWidth:@
setWidth :: IsSCNBox scnBox => scnBox -> CDouble -> IO ()
setWidth scnBox  value =
  sendMsg scnBox (mkSelector "setWidth:") retVoid [argCDouble (fromIntegral value)]

-- | height
--
-- The height of the box. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 1.
--
-- ObjC selector: @- height@
height :: IsSCNBox scnBox => scnBox -> IO CDouble
height scnBox  =
  sendMsg scnBox (mkSelector "height") retCDouble []

-- | height
--
-- The height of the box. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 1.
--
-- ObjC selector: @- setHeight:@
setHeight :: IsSCNBox scnBox => scnBox -> CDouble -> IO ()
setHeight scnBox  value =
  sendMsg scnBox (mkSelector "setHeight:") retVoid [argCDouble (fromIntegral value)]

-- | length
--
-- The length of the box. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 1.
--
-- ObjC selector: @- length@
length_ :: IsSCNBox scnBox => scnBox -> IO CDouble
length_ scnBox  =
  sendMsg scnBox (mkSelector "length") retCDouble []

-- | length
--
-- The length of the box. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 1.
--
-- ObjC selector: @- setLength:@
setLength :: IsSCNBox scnBox => scnBox -> CDouble -> IO ()
setLength scnBox  value =
  sendMsg scnBox (mkSelector "setLength:") retVoid [argCDouble (fromIntegral value)]

-- | chamferRadius
--
-- The chamfer radius. Animatable.
--
-- If the value is strictly less than 0, the geometry is empty. The default value is 0.
--
-- ObjC selector: @- chamferRadius@
chamferRadius :: IsSCNBox scnBox => scnBox -> IO CDouble
chamferRadius scnBox  =
  sendMsg scnBox (mkSelector "chamferRadius") retCDouble []

-- | chamferRadius
--
-- The chamfer radius. Animatable.
--
-- If the value is strictly less than 0, the geometry is empty. The default value is 0.
--
-- ObjC selector: @- setChamferRadius:@
setChamferRadius :: IsSCNBox scnBox => scnBox -> CDouble -> IO ()
setChamferRadius scnBox  value =
  sendMsg scnBox (mkSelector "setChamferRadius:") retVoid [argCDouble (fromIntegral value)]

-- | widthSegmentCount
--
-- The number of subdivisions along the X axis. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 1.
--
-- ObjC selector: @- widthSegmentCount@
widthSegmentCount :: IsSCNBox scnBox => scnBox -> IO CLong
widthSegmentCount scnBox  =
  sendMsg scnBox (mkSelector "widthSegmentCount") retCLong []

-- | widthSegmentCount
--
-- The number of subdivisions along the X axis. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 1.
--
-- ObjC selector: @- setWidthSegmentCount:@
setWidthSegmentCount :: IsSCNBox scnBox => scnBox -> CLong -> IO ()
setWidthSegmentCount scnBox  value =
  sendMsg scnBox (mkSelector "setWidthSegmentCount:") retVoid [argCLong (fromIntegral value)]

-- | heightSegmentCount
--
-- The number of subdivisions along the Y axis. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 1.
--
-- ObjC selector: @- heightSegmentCount@
heightSegmentCount :: IsSCNBox scnBox => scnBox -> IO CLong
heightSegmentCount scnBox  =
  sendMsg scnBox (mkSelector "heightSegmentCount") retCLong []

-- | heightSegmentCount
--
-- The number of subdivisions along the Y axis. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 1.
--
-- ObjC selector: @- setHeightSegmentCount:@
setHeightSegmentCount :: IsSCNBox scnBox => scnBox -> CLong -> IO ()
setHeightSegmentCount scnBox  value =
  sendMsg scnBox (mkSelector "setHeightSegmentCount:") retVoid [argCLong (fromIntegral value)]

-- | lengthSegmentCount
--
-- The number of subdivisions along the Z axis. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 1.
--
-- ObjC selector: @- lengthSegmentCount@
lengthSegmentCount :: IsSCNBox scnBox => scnBox -> IO CLong
lengthSegmentCount scnBox  =
  sendMsg scnBox (mkSelector "lengthSegmentCount") retCLong []

-- | lengthSegmentCount
--
-- The number of subdivisions along the Z axis. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 1.
--
-- ObjC selector: @- setLengthSegmentCount:@
setLengthSegmentCount :: IsSCNBox scnBox => scnBox -> CLong -> IO ()
setLengthSegmentCount scnBox  value =
  sendMsg scnBox (mkSelector "setLengthSegmentCount:") retVoid [argCLong (fromIntegral value)]

-- | chamferSegmentCount
--
-- The number of chamfer subdivisions. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 10.
--
-- ObjC selector: @- chamferSegmentCount@
chamferSegmentCount :: IsSCNBox scnBox => scnBox -> IO CLong
chamferSegmentCount scnBox  =
  sendMsg scnBox (mkSelector "chamferSegmentCount") retCLong []

-- | chamferSegmentCount
--
-- The number of chamfer subdivisions. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 10.
--
-- ObjC selector: @- setChamferSegmentCount:@
setChamferSegmentCount :: IsSCNBox scnBox => scnBox -> CLong -> IO ()
setChamferSegmentCount scnBox  value =
  sendMsg scnBox (mkSelector "setChamferSegmentCount:") retVoid [argCLong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @boxWithWidth:height:length:chamferRadius:@
boxWithWidth_height_length_chamferRadiusSelector :: Selector
boxWithWidth_height_length_chamferRadiusSelector = mkSelector "boxWithWidth:height:length:chamferRadius:"

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

-- | @Selector@ for @chamferRadius@
chamferRadiusSelector :: Selector
chamferRadiusSelector = mkSelector "chamferRadius"

-- | @Selector@ for @setChamferRadius:@
setChamferRadiusSelector :: Selector
setChamferRadiusSelector = mkSelector "setChamferRadius:"

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

-- | @Selector@ for @chamferSegmentCount@
chamferSegmentCountSelector :: Selector
chamferSegmentCountSelector = mkSelector "chamferSegmentCount"

-- | @Selector@ for @setChamferSegmentCount:@
setChamferSegmentCountSelector :: Selector
setChamferSegmentCountSelector = mkSelector "setChamferSegmentCount:"

