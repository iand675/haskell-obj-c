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
  , pointByApplyingVector_toPointSelector
  , distanceBetweenPoint_pointSelector
  , distanceToPointSelector
  , initWithX_ySelector
  , zeroPointSelector
  , xSelector
  , ySelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
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
    withObjCPtr vector $ \raw_vector ->
      withObjCPtr point $ \raw_point ->
        sendClassMsg cls' (mkSelector "pointByApplyingVector:toPoint:") (retPtr retVoid) [argPtr (castPtr raw_vector :: Ptr ()), argPtr (castPtr raw_point :: Ptr ())] >>= retainedObject . castPtr

-- | Returns the Euclidean distance between two VNPoint objects.
--
-- ObjC selector: @+ distanceBetweenPoint:point:@
distanceBetweenPoint_point :: (IsVNPoint point1, IsVNPoint point2) => point1 -> point2 -> IO CDouble
distanceBetweenPoint_point point1 point2 =
  do
    cls' <- getRequiredClass "VNPoint"
    withObjCPtr point1 $ \raw_point1 ->
      withObjCPtr point2 $ \raw_point2 ->
        sendClassMsg cls' (mkSelector "distanceBetweenPoint:point:") retCDouble [argPtr (castPtr raw_point1 :: Ptr ()), argPtr (castPtr raw_point2 :: Ptr ())]

-- | Returns the Euclidean distance to another point.
--
-- @point@ — The destination point.
--
-- Returns: the Euclidean distance between the target and specified points.
--
-- ObjC selector: @- distanceToPoint:@
distanceToPoint :: (IsVNPoint vnPoint, IsVNPoint point) => vnPoint -> point -> IO CDouble
distanceToPoint vnPoint  point =
withObjCPtr point $ \raw_point ->
    sendMsg vnPoint (mkSelector "distanceToPoint:") retCDouble [argPtr (castPtr raw_point :: Ptr ())]

-- | Initializes a VNPoint object from X and Y coordinates.
--
-- ObjC selector: @- initWithX:y:@
initWithX_y :: IsVNPoint vnPoint => vnPoint -> CDouble -> CDouble -> IO (Id VNPoint)
initWithX_y vnPoint  x y =
  sendMsg vnPoint (mkSelector "initWithX:y:") (retPtr retVoid) [argCDouble (fromIntegral x), argCDouble (fromIntegral y)] >>= ownedObject . castPtr

-- | Returns a VNPoint object that represents the location of (0.0, 0.0).
--
-- ObjC selector: @+ zeroPoint@
zeroPoint :: IO (Id VNPoint)
zeroPoint  =
  do
    cls' <- getRequiredClass "VNPoint"
    sendClassMsg cls' (mkSelector "zeroPoint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the X coordinate of the point with respect to the origin of the coordinate system the point is defined in.
--
-- ObjC selector: @- x@
x :: IsVNPoint vnPoint => vnPoint -> IO CDouble
x vnPoint  =
  sendMsg vnPoint (mkSelector "x") retCDouble []

-- | Returns the Y coordinate of the point with respect to the origin of the coordinate system the point is defined in.
--
-- ObjC selector: @- y@
y :: IsVNPoint vnPoint => vnPoint -> IO CDouble
y vnPoint  =
  sendMsg vnPoint (mkSelector "y") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @pointByApplyingVector:toPoint:@
pointByApplyingVector_toPointSelector :: Selector
pointByApplyingVector_toPointSelector = mkSelector "pointByApplyingVector:toPoint:"

-- | @Selector@ for @distanceBetweenPoint:point:@
distanceBetweenPoint_pointSelector :: Selector
distanceBetweenPoint_pointSelector = mkSelector "distanceBetweenPoint:point:"

-- | @Selector@ for @distanceToPoint:@
distanceToPointSelector :: Selector
distanceToPointSelector = mkSelector "distanceToPoint:"

-- | @Selector@ for @initWithX:y:@
initWithX_ySelector :: Selector
initWithX_ySelector = mkSelector "initWithX:y:"

-- | @Selector@ for @zeroPoint@
zeroPointSelector :: Selector
zeroPointSelector = mkSelector "zeroPoint"

-- | @Selector@ for @x@
xSelector :: Selector
xSelector = mkSelector "x"

-- | @Selector@ for @y@
ySelector :: Selector
ySelector = mkSelector "y"

