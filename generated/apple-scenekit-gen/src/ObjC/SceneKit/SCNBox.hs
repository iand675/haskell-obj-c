{-# LANGUAGE DataKinds #-}
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
  , chamferRadiusSelector
  , chamferSegmentCountSelector
  , heightSegmentCountSelector
  , heightSelector
  , lengthSegmentCountSelector
  , lengthSelector
  , setChamferRadiusSelector
  , setChamferSegmentCountSelector
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
    sendClassMessage cls' boxWithWidth_height_length_chamferRadiusSelector width height length_ chamferRadius

-- | width
--
-- The width of the box. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 1.
--
-- ObjC selector: @- width@
width :: IsSCNBox scnBox => scnBox -> IO CDouble
width scnBox =
  sendMessage scnBox widthSelector

-- | width
--
-- The width of the box. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 1.
--
-- ObjC selector: @- setWidth:@
setWidth :: IsSCNBox scnBox => scnBox -> CDouble -> IO ()
setWidth scnBox value =
  sendMessage scnBox setWidthSelector value

-- | height
--
-- The height of the box. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 1.
--
-- ObjC selector: @- height@
height :: IsSCNBox scnBox => scnBox -> IO CDouble
height scnBox =
  sendMessage scnBox heightSelector

-- | height
--
-- The height of the box. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 1.
--
-- ObjC selector: @- setHeight:@
setHeight :: IsSCNBox scnBox => scnBox -> CDouble -> IO ()
setHeight scnBox value =
  sendMessage scnBox setHeightSelector value

-- | length
--
-- The length of the box. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 1.
--
-- ObjC selector: @- length@
length_ :: IsSCNBox scnBox => scnBox -> IO CDouble
length_ scnBox =
  sendMessage scnBox lengthSelector

-- | length
--
-- The length of the box. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 1.
--
-- ObjC selector: @- setLength:@
setLength :: IsSCNBox scnBox => scnBox -> CDouble -> IO ()
setLength scnBox value =
  sendMessage scnBox setLengthSelector value

-- | chamferRadius
--
-- The chamfer radius. Animatable.
--
-- If the value is strictly less than 0, the geometry is empty. The default value is 0.
--
-- ObjC selector: @- chamferRadius@
chamferRadius :: IsSCNBox scnBox => scnBox -> IO CDouble
chamferRadius scnBox =
  sendMessage scnBox chamferRadiusSelector

-- | chamferRadius
--
-- The chamfer radius. Animatable.
--
-- If the value is strictly less than 0, the geometry is empty. The default value is 0.
--
-- ObjC selector: @- setChamferRadius:@
setChamferRadius :: IsSCNBox scnBox => scnBox -> CDouble -> IO ()
setChamferRadius scnBox value =
  sendMessage scnBox setChamferRadiusSelector value

-- | widthSegmentCount
--
-- The number of subdivisions along the X axis. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 1.
--
-- ObjC selector: @- widthSegmentCount@
widthSegmentCount :: IsSCNBox scnBox => scnBox -> IO CLong
widthSegmentCount scnBox =
  sendMessage scnBox widthSegmentCountSelector

-- | widthSegmentCount
--
-- The number of subdivisions along the X axis. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 1.
--
-- ObjC selector: @- setWidthSegmentCount:@
setWidthSegmentCount :: IsSCNBox scnBox => scnBox -> CLong -> IO ()
setWidthSegmentCount scnBox value =
  sendMessage scnBox setWidthSegmentCountSelector value

-- | heightSegmentCount
--
-- The number of subdivisions along the Y axis. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 1.
--
-- ObjC selector: @- heightSegmentCount@
heightSegmentCount :: IsSCNBox scnBox => scnBox -> IO CLong
heightSegmentCount scnBox =
  sendMessage scnBox heightSegmentCountSelector

-- | heightSegmentCount
--
-- The number of subdivisions along the Y axis. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 1.
--
-- ObjC selector: @- setHeightSegmentCount:@
setHeightSegmentCount :: IsSCNBox scnBox => scnBox -> CLong -> IO ()
setHeightSegmentCount scnBox value =
  sendMessage scnBox setHeightSegmentCountSelector value

-- | lengthSegmentCount
--
-- The number of subdivisions along the Z axis. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 1.
--
-- ObjC selector: @- lengthSegmentCount@
lengthSegmentCount :: IsSCNBox scnBox => scnBox -> IO CLong
lengthSegmentCount scnBox =
  sendMessage scnBox lengthSegmentCountSelector

-- | lengthSegmentCount
--
-- The number of subdivisions along the Z axis. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 1.
--
-- ObjC selector: @- setLengthSegmentCount:@
setLengthSegmentCount :: IsSCNBox scnBox => scnBox -> CLong -> IO ()
setLengthSegmentCount scnBox value =
  sendMessage scnBox setLengthSegmentCountSelector value

-- | chamferSegmentCount
--
-- The number of chamfer subdivisions. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 10.
--
-- ObjC selector: @- chamferSegmentCount@
chamferSegmentCount :: IsSCNBox scnBox => scnBox -> IO CLong
chamferSegmentCount scnBox =
  sendMessage scnBox chamferSegmentCountSelector

-- | chamferSegmentCount
--
-- The number of chamfer subdivisions. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 10.
--
-- ObjC selector: @- setChamferSegmentCount:@
setChamferSegmentCount :: IsSCNBox scnBox => scnBox -> CLong -> IO ()
setChamferSegmentCount scnBox value =
  sendMessage scnBox setChamferSegmentCountSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @boxWithWidth:height:length:chamferRadius:@
boxWithWidth_height_length_chamferRadiusSelector :: Selector '[CDouble, CDouble, CDouble, CDouble] (Id SCNBox)
boxWithWidth_height_length_chamferRadiusSelector = mkSelector "boxWithWidth:height:length:chamferRadius:"

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

-- | @Selector@ for @chamferRadius@
chamferRadiusSelector :: Selector '[] CDouble
chamferRadiusSelector = mkSelector "chamferRadius"

-- | @Selector@ for @setChamferRadius:@
setChamferRadiusSelector :: Selector '[CDouble] ()
setChamferRadiusSelector = mkSelector "setChamferRadius:"

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

-- | @Selector@ for @chamferSegmentCount@
chamferSegmentCountSelector :: Selector '[] CLong
chamferSegmentCountSelector = mkSelector "chamferSegmentCount"

-- | @Selector@ for @setChamferSegmentCount:@
setChamferSegmentCountSelector :: Selector '[CLong] ()
setChamferSegmentCountSelector = mkSelector "setChamferSegmentCount:"

