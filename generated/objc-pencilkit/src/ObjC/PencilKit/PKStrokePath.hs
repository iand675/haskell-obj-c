{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A uniform cubic B-spline representing the point data of a @PKStroke@.
--
-- Generated bindings for @PKStrokePath@.
module ObjC.PencilKit.PKStrokePath
  ( PKStrokePath
  , IsPKStrokePath(..)
  , initWithControlPoints_creationDate
  , pointAtIndex
  , objectAtIndexedSubscript
  , interpolatedPointAt
  , enumerateInterpolatedPointsInRange_strideByDistance_usingBlock
  , enumerateInterpolatedPointsInRange_strideByTime_usingBlock
  , enumerateInterpolatedPointsInRange_strideByParametricStep_usingBlock
  , parametricValue_offsetByDistance
  , parametricValue_offsetByTime
  , count
  , creationDate
  , initWithControlPoints_creationDateSelector
  , pointAtIndexSelector
  , objectAtIndexedSubscriptSelector
  , interpolatedPointAtSelector
  , enumerateInterpolatedPointsInRange_strideByDistance_usingBlockSelector
  , enumerateInterpolatedPointsInRange_strideByTime_usingBlockSelector
  , enumerateInterpolatedPointsInRange_strideByParametricStep_usingBlockSelector
  , parametricValue_offsetByDistanceSelector
  , parametricValue_offsetByTimeSelector
  , countSelector
  , creationDateSelector


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

import ObjC.PencilKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Create a stroke path value with the given cubic B-spline control points.
--
-- @controlPoints@ — An array of control points for a cubic B-spline.
--
-- @creationDate@ — The start time of this path.
--
-- ObjC selector: @- initWithControlPoints:creationDate:@
initWithControlPoints_creationDate :: (IsPKStrokePath pkStrokePath, IsNSArray controlPoints, IsNSDate creationDate) => pkStrokePath -> controlPoints -> creationDate -> IO (Id PKStrokePath)
initWithControlPoints_creationDate pkStrokePath  controlPoints creationDate =
withObjCPtr controlPoints $ \raw_controlPoints ->
  withObjCPtr creationDate $ \raw_creationDate ->
      sendMsg pkStrokePath (mkSelector "initWithControlPoints:creationDate:") (retPtr retVoid) [argPtr (castPtr raw_controlPoints :: Ptr ()), argPtr (castPtr raw_creationDate :: Ptr ())] >>= ownedObject . castPtr

-- | Returns B-spline control point at index @i@.
--
-- ObjC selector: @- pointAtIndex:@
pointAtIndex :: IsPKStrokePath pkStrokePath => pkStrokePath -> CULong -> IO (Id PKStrokePoint)
pointAtIndex pkStrokePath  i =
  sendMsg pkStrokePath (mkSelector "pointAtIndex:") (retPtr retVoid) [argCULong (fromIntegral i)] >>= retainedObject . castPtr

-- | Returns B-spline control point at index @i@.
--
-- ObjC selector: @- objectAtIndexedSubscript:@
objectAtIndexedSubscript :: IsPKStrokePath pkStrokePath => pkStrokePath -> CULong -> IO (Id PKStrokePoint)
objectAtIndexedSubscript pkStrokePath  i =
  sendMsg pkStrokePath (mkSelector "objectAtIndexedSubscript:") (retPtr retVoid) [argCULong (fromIntegral i)] >>= retainedObject . castPtr

-- | The on-curve point for the floating point [0, count-1] @parametricValue@ parameter.
--
-- ObjC selector: @- interpolatedPointAt:@
interpolatedPointAt :: IsPKStrokePath pkStrokePath => pkStrokePath -> CDouble -> IO (Id PKStrokePoint)
interpolatedPointAt pkStrokePath  parametricValue =
  sendMsg pkStrokePath (mkSelector "interpolatedPointAt:") (retPtr retVoid) [argCDouble (fromIntegral parametricValue)] >>= retainedObject . castPtr

-- | Executes a given block using each point in a range with a distance step.
--
-- @range@ — The parametric range to enumerate points in.
--
-- @distanceStep@ — The distance to step between points.
--
-- @block@ — The block to execute for each point. This block takes two parameters        point The interpolated point on the spline.        stop A reference to a Boolean value. Setting the value to YES within the block stops further enumeration of the array. If a block stops further enumeration, that block continues to run until it’s finished.
--
-- ObjC selector: @- enumerateInterpolatedPointsInRange:strideByDistance:usingBlock:@
enumerateInterpolatedPointsInRange_strideByDistance_usingBlock :: (IsPKStrokePath pkStrokePath, IsPKFloatRange range) => pkStrokePath -> range -> CDouble -> Ptr () -> IO ()
enumerateInterpolatedPointsInRange_strideByDistance_usingBlock pkStrokePath  range distanceStep block =
withObjCPtr range $ \raw_range ->
    sendMsg pkStrokePath (mkSelector "enumerateInterpolatedPointsInRange:strideByDistance:usingBlock:") retVoid [argPtr (castPtr raw_range :: Ptr ()), argCDouble (fromIntegral distanceStep), argPtr (castPtr block :: Ptr ())]

-- | Executes a given block using each point in a range with a time step.
--
-- @range@ — The parametric range to enumerate points in.
--
-- @timeStep@ — The time interval to step between points.
--
-- @block@ — The block to execute for each point. This block takes two parameters        point The interpolated point on the spline.        stop A reference to a Boolean value. Setting the value to YES within the block stops further enumeration of the array. If a block stops further enumeration, that block continues to run until it’s finished.
--
-- ObjC selector: @- enumerateInterpolatedPointsInRange:strideByTime:usingBlock:@
enumerateInterpolatedPointsInRange_strideByTime_usingBlock :: (IsPKStrokePath pkStrokePath, IsPKFloatRange range) => pkStrokePath -> range -> CDouble -> Ptr () -> IO ()
enumerateInterpolatedPointsInRange_strideByTime_usingBlock pkStrokePath  range timeStep block =
withObjCPtr range $ \raw_range ->
    sendMsg pkStrokePath (mkSelector "enumerateInterpolatedPointsInRange:strideByTime:usingBlock:") retVoid [argPtr (castPtr raw_range :: Ptr ()), argCDouble (fromIntegral timeStep), argPtr (castPtr block :: Ptr ())]

-- | Executes a given block using each point in a range with a parametric step.
--
-- @range@ — The parametric range to enumerate points in.
--
-- @parametricStep@ — The parametric step between points.
--
-- @block@ — The block to execute for each point. This block takes two parameters        point The interpolated point on the spline.        stop A reference to a Boolean value. Setting the value to YES within the block stops further enumeration of the array. If a block stops further enumeration, that block continues to run until it’s finished.
--
-- ObjC selector: @- enumerateInterpolatedPointsInRange:strideByParametricStep:usingBlock:@
enumerateInterpolatedPointsInRange_strideByParametricStep_usingBlock :: (IsPKStrokePath pkStrokePath, IsPKFloatRange range) => pkStrokePath -> range -> CDouble -> Ptr () -> IO ()
enumerateInterpolatedPointsInRange_strideByParametricStep_usingBlock pkStrokePath  range parametricStep block =
withObjCPtr range $ \raw_range ->
    sendMsg pkStrokePath (mkSelector "enumerateInterpolatedPointsInRange:strideByParametricStep:usingBlock:") retVoid [argPtr (castPtr raw_range :: Ptr ()), argCDouble (fromIntegral parametricStep), argPtr (castPtr block :: Ptr ())]

-- | Returns a parametric value on the B-spline that is a specified distance from the given parametric value.
--
-- @parametricValue@ — The floating point [0, count-1] parametric value.
--
-- @distanceStep@ — The distance to offset @parametricValue@. @distanceStep@ can be positive or negative.
--
-- Returns: A parametric value offset by @distanceStep@ from @parametricValue@.
--
-- ObjC selector: @- parametricValue:offsetByDistance:@
parametricValue_offsetByDistance :: IsPKStrokePath pkStrokePath => pkStrokePath -> CDouble -> CDouble -> IO CDouble
parametricValue_offsetByDistance pkStrokePath  parametricValue distanceStep =
  sendMsg pkStrokePath (mkSelector "parametricValue:offsetByDistance:") retCDouble [argCDouble (fromIntegral parametricValue), argCDouble (fromIntegral distanceStep)]

-- | Returns a parametric value on the B-spline that is a specified time from the given parametric value.
--
-- @parametricValue@ — The floating point [0, count-1] parametric value.
--
-- @timeStep@ — The time to offset @parametricValue@. @timeStep@ can be positive or negative.
--
-- Returns: A parametric value offset by @timeStep@ from @parametricValue@.
--
-- ObjC selector: @- parametricValue:offsetByTime:@
parametricValue_offsetByTime :: IsPKStrokePath pkStrokePath => pkStrokePath -> CDouble -> CDouble -> IO CDouble
parametricValue_offsetByTime pkStrokePath  parametricValue timeStep =
  sendMsg pkStrokePath (mkSelector "parametricValue:offsetByTime:") retCDouble [argCDouble (fromIntegral parametricValue), argCDouble (fromIntegral timeStep)]

-- | The number of control points in this stroke path.
--
-- ObjC selector: @- count@
count :: IsPKStrokePath pkStrokePath => pkStrokePath -> IO CULong
count pkStrokePath  =
  sendMsg pkStrokePath (mkSelector "count") retCULong []

-- | The time at which this stroke path was started. The @timeOffset@ of contained PKStrokePoints is relative to this date.
--
-- ObjC selector: @- creationDate@
creationDate :: IsPKStrokePath pkStrokePath => pkStrokePath -> IO (Id NSDate)
creationDate pkStrokePath  =
  sendMsg pkStrokePath (mkSelector "creationDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithControlPoints:creationDate:@
initWithControlPoints_creationDateSelector :: Selector
initWithControlPoints_creationDateSelector = mkSelector "initWithControlPoints:creationDate:"

-- | @Selector@ for @pointAtIndex:@
pointAtIndexSelector :: Selector
pointAtIndexSelector = mkSelector "pointAtIndex:"

-- | @Selector@ for @objectAtIndexedSubscript:@
objectAtIndexedSubscriptSelector :: Selector
objectAtIndexedSubscriptSelector = mkSelector "objectAtIndexedSubscript:"

-- | @Selector@ for @interpolatedPointAt:@
interpolatedPointAtSelector :: Selector
interpolatedPointAtSelector = mkSelector "interpolatedPointAt:"

-- | @Selector@ for @enumerateInterpolatedPointsInRange:strideByDistance:usingBlock:@
enumerateInterpolatedPointsInRange_strideByDistance_usingBlockSelector :: Selector
enumerateInterpolatedPointsInRange_strideByDistance_usingBlockSelector = mkSelector "enumerateInterpolatedPointsInRange:strideByDistance:usingBlock:"

-- | @Selector@ for @enumerateInterpolatedPointsInRange:strideByTime:usingBlock:@
enumerateInterpolatedPointsInRange_strideByTime_usingBlockSelector :: Selector
enumerateInterpolatedPointsInRange_strideByTime_usingBlockSelector = mkSelector "enumerateInterpolatedPointsInRange:strideByTime:usingBlock:"

-- | @Selector@ for @enumerateInterpolatedPointsInRange:strideByParametricStep:usingBlock:@
enumerateInterpolatedPointsInRange_strideByParametricStep_usingBlockSelector :: Selector
enumerateInterpolatedPointsInRange_strideByParametricStep_usingBlockSelector = mkSelector "enumerateInterpolatedPointsInRange:strideByParametricStep:usingBlock:"

-- | @Selector@ for @parametricValue:offsetByDistance:@
parametricValue_offsetByDistanceSelector :: Selector
parametricValue_offsetByDistanceSelector = mkSelector "parametricValue:offsetByDistance:"

-- | @Selector@ for @parametricValue:offsetByTime:@
parametricValue_offsetByTimeSelector :: Selector
parametricValue_offsetByTimeSelector = mkSelector "parametricValue:offsetByTime:"

-- | @Selector@ for @count@
countSelector :: Selector
countSelector = mkSelector "count"

-- | @Selector@ for @creationDate@
creationDateSelector :: Selector
creationDateSelector = mkSelector "creationDate"

