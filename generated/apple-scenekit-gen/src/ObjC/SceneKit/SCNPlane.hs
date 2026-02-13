{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNPlane
--
-- SCNPlane represents a rectangle with controllable width and height. The plane has one visible side.
--
-- Generated bindings for @SCNPlane@.
module ObjC.SceneKit.SCNPlane
  ( SCNPlane
  , IsSCNPlane(..)
  , planeWithWidth_height
  , width
  , setWidth
  , height
  , setHeight
  , widthSegmentCount
  , setWidthSegmentCount
  , heightSegmentCount
  , setHeightSegmentCount
  , cornerRadius
  , setCornerRadius
  , cornerSegmentCount
  , setCornerSegmentCount
  , cornerRadiusSelector
  , cornerSegmentCountSelector
  , heightSegmentCountSelector
  , heightSelector
  , planeWithWidth_heightSelector
  , setCornerRadiusSelector
  , setCornerSegmentCountSelector
  , setHeightSegmentCountSelector
  , setHeightSelector
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

-- | planeWithWidth:height:
--
-- Creates and returns a plane with given width and height.
--
-- @width@ — The width of the plane.
--
-- @height@ — The height of the plane.
--
-- ObjC selector: @+ planeWithWidth:height:@
planeWithWidth_height :: CDouble -> CDouble -> IO (Id SCNPlane)
planeWithWidth_height width height =
  do
    cls' <- getRequiredClass "SCNPlane"
    sendClassMessage cls' planeWithWidth_heightSelector width height

-- | width
--
-- The plane extent along the X axis. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 1.
--
-- ObjC selector: @- width@
width :: IsSCNPlane scnPlane => scnPlane -> IO CDouble
width scnPlane =
  sendMessage scnPlane widthSelector

-- | width
--
-- The plane extent along the X axis. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 1.
--
-- ObjC selector: @- setWidth:@
setWidth :: IsSCNPlane scnPlane => scnPlane -> CDouble -> IO ()
setWidth scnPlane value =
  sendMessage scnPlane setWidthSelector value

-- | height
--
-- The plane extent along the Y axis. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 1.
--
-- ObjC selector: @- height@
height :: IsSCNPlane scnPlane => scnPlane -> IO CDouble
height scnPlane =
  sendMessage scnPlane heightSelector

-- | height
--
-- The plane extent along the Y axis. Animatable.
--
-- If the value is less than or equal to 0, the geometry is empty. The default value is 1.
--
-- ObjC selector: @- setHeight:@
setHeight :: IsSCNPlane scnPlane => scnPlane -> CDouble -> IO ()
setHeight scnPlane value =
  sendMessage scnPlane setHeightSelector value

-- | widthSegmentCount
--
-- The number of subdivisions along the X axis. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 1.
--
-- ObjC selector: @- widthSegmentCount@
widthSegmentCount :: IsSCNPlane scnPlane => scnPlane -> IO CLong
widthSegmentCount scnPlane =
  sendMessage scnPlane widthSegmentCountSelector

-- | widthSegmentCount
--
-- The number of subdivisions along the X axis. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 1.
--
-- ObjC selector: @- setWidthSegmentCount:@
setWidthSegmentCount :: IsSCNPlane scnPlane => scnPlane -> CLong -> IO ()
setWidthSegmentCount scnPlane value =
  sendMessage scnPlane setWidthSegmentCountSelector value

-- | heightSegmentCount
--
-- The number of subdivisions along the Y axis. The default value is 1. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 1.
--
-- ObjC selector: @- heightSegmentCount@
heightSegmentCount :: IsSCNPlane scnPlane => scnPlane -> IO CLong
heightSegmentCount scnPlane =
  sendMessage scnPlane heightSegmentCountSelector

-- | heightSegmentCount
--
-- The number of subdivisions along the Y axis. The default value is 1. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 1.
--
-- ObjC selector: @- setHeightSegmentCount:@
setHeightSegmentCount :: IsSCNPlane scnPlane => scnPlane -> CLong -> IO ()
setHeightSegmentCount scnPlane value =
  sendMessage scnPlane setHeightSegmentCountSelector value

-- | cornerRadius
--
-- The corner radius. Animatable.
--
-- If the value is strictly less than 0, the geometry is empty. The default value is 0.
--
-- ObjC selector: @- cornerRadius@
cornerRadius :: IsSCNPlane scnPlane => scnPlane -> IO CDouble
cornerRadius scnPlane =
  sendMessage scnPlane cornerRadiusSelector

-- | cornerRadius
--
-- The corner radius. Animatable.
--
-- If the value is strictly less than 0, the geometry is empty. The default value is 0.
--
-- ObjC selector: @- setCornerRadius:@
setCornerRadius :: IsSCNPlane scnPlane => scnPlane -> CDouble -> IO ()
setCornerRadius scnPlane value =
  sendMessage scnPlane setCornerRadiusSelector value

-- | cornerSegmentCount
--
-- The number of subdivisions for the rounded corners. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 10.
--
-- ObjC selector: @- cornerSegmentCount@
cornerSegmentCount :: IsSCNPlane scnPlane => scnPlane -> IO CLong
cornerSegmentCount scnPlane =
  sendMessage scnPlane cornerSegmentCountSelector

-- | cornerSegmentCount
--
-- The number of subdivisions for the rounded corners. Animatable.
--
-- If the value is less than 1, the behavior is undefined. The default value is 10.
--
-- ObjC selector: @- setCornerSegmentCount:@
setCornerSegmentCount :: IsSCNPlane scnPlane => scnPlane -> CLong -> IO ()
setCornerSegmentCount scnPlane value =
  sendMessage scnPlane setCornerSegmentCountSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @planeWithWidth:height:@
planeWithWidth_heightSelector :: Selector '[CDouble, CDouble] (Id SCNPlane)
planeWithWidth_heightSelector = mkSelector "planeWithWidth:height:"

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

-- | @Selector@ for @cornerRadius@
cornerRadiusSelector :: Selector '[] CDouble
cornerRadiusSelector = mkSelector "cornerRadius"

-- | @Selector@ for @setCornerRadius:@
setCornerRadiusSelector :: Selector '[CDouble] ()
setCornerRadiusSelector = mkSelector "setCornerRadius:"

-- | @Selector@ for @cornerSegmentCount@
cornerSegmentCountSelector :: Selector '[] CLong
cornerSegmentCountSelector = mkSelector "cornerSegmentCount"

-- | @Selector@ for @setCornerSegmentCount:@
setCornerSegmentCountSelector :: Selector '[CLong] ()
setCornerSegmentCountSelector = mkSelector "setCornerSegmentCount:"

