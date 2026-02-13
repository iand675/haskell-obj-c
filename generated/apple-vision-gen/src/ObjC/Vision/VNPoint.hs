{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | VNPoint
--
-- VNPoint represents a single, immutable, two-dimensional point in an image.
--
-- It should be noted that VNPoint is not intended as an overall replacement of CGPoint, NSPoint or vec2, but is used by observations that need to present points which may contain additional metadata.
--
-- Generated bindings for @VNPoint@.
module ObjC.Vision.VNPoint
  ( VNPoint
  , IsVNPoint(..)
  , pointByApplyingVector_toPoint
  , distanceBetweenPoint_point
  , distanceToPoint
  , initWithX_y
  , zeroPoint
  , x
  , y
  , distanceBetweenPoint_pointSelector
  , distanceToPointSelector
  , initWithX_ySelector
  , pointByApplyingVector_toPointSelector
  , xSelector
  , ySelector
  , zeroPointSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Returns a new VNPoint object that is shifted by X and Y offsets of the vector.
--
-- @vector@ — The vector offset to be applied to a source point.
--
-- @point@ — The source point.
--
-- Returns: the translated point.
--
-- ObjC selector: @+ pointByApplyingVector:toPoint:@
pointByApplyingVector_toPoint :: (IsVNVector vector, IsVNPoint point) => vector -> point -> IO (Id VNPoint)
pointByApplyingVector_toPoint vector point =
  do
    cls' <- getRequiredClass "VNPoint"
    sendClassMessage cls' pointByApplyingVector_toPointSelector (toVNVector vector) (toVNPoint point)

-- | Returns the Euclidean distance between two VNPoint objects.
--
-- ObjC selector: @+ distanceBetweenPoint:point:@
distanceBetweenPoint_point :: (IsVNPoint point1, IsVNPoint point2) => point1 -> point2 -> IO CDouble
distanceBetweenPoint_point point1 point2 =
  do
    cls' <- getRequiredClass "VNPoint"
    sendClassMessage cls' distanceBetweenPoint_pointSelector (toVNPoint point1) (toVNPoint point2)

-- | Returns the Euclidean distance to another point.
--
-- @point@ — The destination point.
--
-- Returns: the Euclidean distance between the target and specified points.
--
-- ObjC selector: @- distanceToPoint:@
distanceToPoint :: (IsVNPoint vnPoint, IsVNPoint point) => vnPoint -> point -> IO CDouble
distanceToPoint vnPoint point =
  sendMessage vnPoint distanceToPointSelector (toVNPoint point)

-- | Initializes a VNPoint object from X and Y coordinates.
--
-- ObjC selector: @- initWithX:y:@
initWithX_y :: IsVNPoint vnPoint => vnPoint -> CDouble -> CDouble -> IO (Id VNPoint)
initWithX_y vnPoint x y =
  sendOwnedMessage vnPoint initWithX_ySelector x y

-- | Returns a VNPoint object that represents the location of (0.0, 0.0).
--
-- ObjC selector: @+ zeroPoint@
zeroPoint :: IO (Id VNPoint)
zeroPoint  =
  do
    cls' <- getRequiredClass "VNPoint"
    sendClassMessage cls' zeroPointSelector

-- | Returns the X coordinate of the point with respect to the origin of the coordinate system the point is defined in.
--
-- ObjC selector: @- x@
x :: IsVNPoint vnPoint => vnPoint -> IO CDouble
x vnPoint =
  sendMessage vnPoint xSelector

-- | Returns the Y coordinate of the point with respect to the origin of the coordinate system the point is defined in.
--
-- ObjC selector: @- y@
y :: IsVNPoint vnPoint => vnPoint -> IO CDouble
y vnPoint =
  sendMessage vnPoint ySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @pointByApplyingVector:toPoint:@
pointByApplyingVector_toPointSelector :: Selector '[Id VNVector, Id VNPoint] (Id VNPoint)
pointByApplyingVector_toPointSelector = mkSelector "pointByApplyingVector:toPoint:"

-- | @Selector@ for @distanceBetweenPoint:point:@
distanceBetweenPoint_pointSelector :: Selector '[Id VNPoint, Id VNPoint] CDouble
distanceBetweenPoint_pointSelector = mkSelector "distanceBetweenPoint:point:"

-- | @Selector@ for @distanceToPoint:@
distanceToPointSelector :: Selector '[Id VNPoint] CDouble
distanceToPointSelector = mkSelector "distanceToPoint:"

-- | @Selector@ for @initWithX:y:@
initWithX_ySelector :: Selector '[CDouble, CDouble] (Id VNPoint)
initWithX_ySelector = mkSelector "initWithX:y:"

-- | @Selector@ for @zeroPoint@
zeroPointSelector :: Selector '[] (Id VNPoint)
zeroPointSelector = mkSelector "zeroPoint"

-- | @Selector@ for @x@
xSelector :: Selector '[] CDouble
xSelector = mkSelector "x"

-- | @Selector@ for @y@
ySelector :: Selector '[] CDouble
ySelector = mkSelector "y"

