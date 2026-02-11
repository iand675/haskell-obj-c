{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | VNCircle
--
-- VNCircle is two-dimensional circle represented by the center point 'center' and its radius 'radius'. Once created, VNCircle objects are immutable.
--
-- Generated bindings for @VNCircle@.
module ObjC.Vision.VNCircle
  ( VNCircle
  , IsVNCircle(..)
  , initWithCenter_radius
  , initWithCenter_diameter
  , containsPoint
  , containsPoint_inCircumferentialRingOfWidth
  , zeroCircle
  , center
  , radius
  , diameter
  , initWithCenter_radiusSelector
  , initWithCenter_diameterSelector
  , containsPointSelector
  , containsPoint_inCircumferentialRingOfWidthSelector
  , zeroCircleSelector
  , centerSelector
  , radiusSelector
  , diameterSelector


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

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initializes VNCircle object with given circle center and circle radius.
--
-- ObjC selector: @- initWithCenter:radius:@
initWithCenter_radius :: (IsVNCircle vnCircle, IsVNPoint center) => vnCircle -> center -> CDouble -> IO (Id VNCircle)
initWithCenter_radius vnCircle  center radius =
withObjCPtr center $ \raw_center ->
    sendMsg vnCircle (mkSelector "initWithCenter:radius:") (retPtr retVoid) [argPtr (castPtr raw_center :: Ptr ()), argCDouble (fromIntegral radius)] >>= ownedObject . castPtr

-- | Initializes VNCircle object with given circle center and circle diameter.
--
-- ObjC selector: @- initWithCenter:diameter:@
initWithCenter_diameter :: (IsVNCircle vnCircle, IsVNPoint center) => vnCircle -> center -> CDouble -> IO (Id VNCircle)
initWithCenter_diameter vnCircle  center diameter =
withObjCPtr center $ \raw_center ->
    sendMsg vnCircle (mkSelector "initWithCenter:diameter:") (retPtr retVoid) [argPtr (castPtr raw_center :: Ptr ()), argCDouble (fromIntegral diameter)] >>= ownedObject . castPtr

-- | Returns YES if the point is inside the circle, including the boundary.
--
-- ObjC selector: @- containsPoint:@
containsPoint :: (IsVNCircle vnCircle, IsVNPoint point) => vnCircle -> point -> IO Bool
containsPoint vnCircle  point =
withObjCPtr point $ \raw_point ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg vnCircle (mkSelector "containsPoint:") retCULong [argPtr (castPtr raw_point :: Ptr ())]

-- | Returns YES if the point is within the ring bound by two circles [radius - delta; radius + delta].
--
-- ObjC selector: @- containsPoint:inCircumferentialRingOfWidth:@
containsPoint_inCircumferentialRingOfWidth :: (IsVNCircle vnCircle, IsVNPoint point) => vnCircle -> point -> CDouble -> IO Bool
containsPoint_inCircumferentialRingOfWidth vnCircle  point ringWidth =
withObjCPtr point $ \raw_point ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg vnCircle (mkSelector "containsPoint:inCircumferentialRingOfWidth:") retCULong [argPtr (castPtr raw_point :: Ptr ()), argCDouble (fromIntegral ringWidth)]

-- | Returns a VNCircle object with center at the Origin [0.0; 0.0] and zero radius.
--
-- ObjC selector: @+ zeroCircle@
zeroCircle :: IO (Id VNCircle)
zeroCircle  =
  do
    cls' <- getRequiredClass "VNCircle"
    sendClassMsg cls' (mkSelector "zeroCircle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns circle center.
--
-- ObjC selector: @- center@
center :: IsVNCircle vnCircle => vnCircle -> IO (Id VNPoint)
center vnCircle  =
  sendMsg vnCircle (mkSelector "center") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns circle radius.
--
-- ObjC selector: @- radius@
radius :: IsVNCircle vnCircle => vnCircle -> IO CDouble
radius vnCircle  =
  sendMsg vnCircle (mkSelector "radius") retCDouble []

-- | Returns circle diameter.
--
-- ObjC selector: @- diameter@
diameter :: IsVNCircle vnCircle => vnCircle -> IO CDouble
diameter vnCircle  =
  sendMsg vnCircle (mkSelector "diameter") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCenter:radius:@
initWithCenter_radiusSelector :: Selector
initWithCenter_radiusSelector = mkSelector "initWithCenter:radius:"

-- | @Selector@ for @initWithCenter:diameter:@
initWithCenter_diameterSelector :: Selector
initWithCenter_diameterSelector = mkSelector "initWithCenter:diameter:"

-- | @Selector@ for @containsPoint:@
containsPointSelector :: Selector
containsPointSelector = mkSelector "containsPoint:"

-- | @Selector@ for @containsPoint:inCircumferentialRingOfWidth:@
containsPoint_inCircumferentialRingOfWidthSelector :: Selector
containsPoint_inCircumferentialRingOfWidthSelector = mkSelector "containsPoint:inCircumferentialRingOfWidth:"

-- | @Selector@ for @zeroCircle@
zeroCircleSelector :: Selector
zeroCircleSelector = mkSelector "zeroCircle"

-- | @Selector@ for @center@
centerSelector :: Selector
centerSelector = mkSelector "center"

-- | @Selector@ for @radius@
radiusSelector :: Selector
radiusSelector = mkSelector "radius"

-- | @Selector@ for @diameter@
diameterSelector :: Selector
diameterSelector = mkSelector "diameter"

