{-# LANGUAGE DataKinds #-}
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
  , centerSelector
  , containsPointSelector
  , containsPoint_inCircumferentialRingOfWidthSelector
  , diameterSelector
  , initWithCenter_diameterSelector
  , initWithCenter_radiusSelector
  , radiusSelector
  , zeroCircleSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initializes VNCircle object with given circle center and circle radius.
--
-- ObjC selector: @- initWithCenter:radius:@
initWithCenter_radius :: (IsVNCircle vnCircle, IsVNPoint center) => vnCircle -> center -> CDouble -> IO (Id VNCircle)
initWithCenter_radius vnCircle center radius =
  sendOwnedMessage vnCircle initWithCenter_radiusSelector (toVNPoint center) radius

-- | Initializes VNCircle object with given circle center and circle diameter.
--
-- ObjC selector: @- initWithCenter:diameter:@
initWithCenter_diameter :: (IsVNCircle vnCircle, IsVNPoint center) => vnCircle -> center -> CDouble -> IO (Id VNCircle)
initWithCenter_diameter vnCircle center diameter =
  sendOwnedMessage vnCircle initWithCenter_diameterSelector (toVNPoint center) diameter

-- | Returns YES if the point is inside the circle, including the boundary.
--
-- ObjC selector: @- containsPoint:@
containsPoint :: (IsVNCircle vnCircle, IsVNPoint point) => vnCircle -> point -> IO Bool
containsPoint vnCircle point =
  sendMessage vnCircle containsPointSelector (toVNPoint point)

-- | Returns YES if the point is within the ring bound by two circles [radius - delta; radius + delta].
--
-- ObjC selector: @- containsPoint:inCircumferentialRingOfWidth:@
containsPoint_inCircumferentialRingOfWidth :: (IsVNCircle vnCircle, IsVNPoint point) => vnCircle -> point -> CDouble -> IO Bool
containsPoint_inCircumferentialRingOfWidth vnCircle point ringWidth =
  sendMessage vnCircle containsPoint_inCircumferentialRingOfWidthSelector (toVNPoint point) ringWidth

-- | Returns a VNCircle object with center at the Origin [0.0; 0.0] and zero radius.
--
-- ObjC selector: @+ zeroCircle@
zeroCircle :: IO (Id VNCircle)
zeroCircle  =
  do
    cls' <- getRequiredClass "VNCircle"
    sendClassMessage cls' zeroCircleSelector

-- | Returns circle center.
--
-- ObjC selector: @- center@
center :: IsVNCircle vnCircle => vnCircle -> IO (Id VNPoint)
center vnCircle =
  sendMessage vnCircle centerSelector

-- | Returns circle radius.
--
-- ObjC selector: @- radius@
radius :: IsVNCircle vnCircle => vnCircle -> IO CDouble
radius vnCircle =
  sendMessage vnCircle radiusSelector

-- | Returns circle diameter.
--
-- ObjC selector: @- diameter@
diameter :: IsVNCircle vnCircle => vnCircle -> IO CDouble
diameter vnCircle =
  sendMessage vnCircle diameterSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCenter:radius:@
initWithCenter_radiusSelector :: Selector '[Id VNPoint, CDouble] (Id VNCircle)
initWithCenter_radiusSelector = mkSelector "initWithCenter:radius:"

-- | @Selector@ for @initWithCenter:diameter:@
initWithCenter_diameterSelector :: Selector '[Id VNPoint, CDouble] (Id VNCircle)
initWithCenter_diameterSelector = mkSelector "initWithCenter:diameter:"

-- | @Selector@ for @containsPoint:@
containsPointSelector :: Selector '[Id VNPoint] Bool
containsPointSelector = mkSelector "containsPoint:"

-- | @Selector@ for @containsPoint:inCircumferentialRingOfWidth:@
containsPoint_inCircumferentialRingOfWidthSelector :: Selector '[Id VNPoint, CDouble] Bool
containsPoint_inCircumferentialRingOfWidthSelector = mkSelector "containsPoint:inCircumferentialRingOfWidth:"

-- | @Selector@ for @zeroCircle@
zeroCircleSelector :: Selector '[] (Id VNCircle)
zeroCircleSelector = mkSelector "zeroCircle"

-- | @Selector@ for @center@
centerSelector :: Selector '[] (Id VNPoint)
centerSelector = mkSelector "center"

-- | @Selector@ for @radius@
radiusSelector :: Selector '[] CDouble
radiusSelector = mkSelector "radius"

-- | @Selector@ for @diameter@
diameterSelector :: Selector '[] CDouble
diameterSelector = mkSelector "diameter"

