{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @VNGeometryUtils@.
module ObjC.Vision.VNGeometryUtils
  ( VNGeometryUtils
  , IsVNGeometryUtils(..)
  , boundingCircleForContour_error
  , boundingCircleForPoints_error
  , calculateArea_forContour_orientedArea_error
  , calculatePerimeter_forContour_error
  , boundingCircleForContour_errorSelector
  , boundingCircleForPoints_errorSelector
  , calculateArea_forContour_orientedArea_errorSelector
  , calculatePerimeter_forContour_errorSelector


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

-- | Calculates a bounding circle that includes a collection of points or a VNContour object. Note that because this is based on a geometric shape the aspect ratio is important when using normalized points.				   This takes the aspect ratio of the contour into account when using a VNContour as an input. 				   boundingCircleForPoints and boundingCircleForSIMDPoints assume that the aspect ratio is correctly applied to the points.
--
-- @contour@ — A contour around which to find the bounding circle.
--
-- @points@ — A collection of points around which to find the bounding circle.
--
-- @pointCount@ — Number of points in points
--
-- @contour@ — VNContour object whose bounding circle needs to be calculated
--
-- @error@ — An output parameter, populated only in case of algorithmic failure
--
-- Returns: the VNCircle object describing the bounding circle or nil, if the algorithm failed. The latter case is accompanied by populating an 'error' output parameter
--
-- ObjC selector: @+ boundingCircleForContour:error:@
boundingCircleForContour_error :: (IsVNContour contour, IsNSError error_) => contour -> error_ -> IO (Id VNCircle)
boundingCircleForContour_error contour error_ =
  do
    cls' <- getRequiredClass "VNGeometryUtils"
    withObjCPtr contour $ \raw_contour ->
      withObjCPtr error_ $ \raw_error_ ->
        sendClassMsg cls' (mkSelector "boundingCircleForContour:error:") (retPtr retVoid) [argPtr (castPtr raw_contour :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @+ boundingCircleForPoints:error:@
boundingCircleForPoints_error :: (IsNSArray points, IsNSError error_) => points -> error_ -> IO (Id VNCircle)
boundingCircleForPoints_error points error_ =
  do
    cls' <- getRequiredClass "VNGeometryUtils"
    withObjCPtr points $ \raw_points ->
      withObjCPtr error_ $ \raw_error_ ->
        sendClassMsg cls' (mkSelector "boundingCircleForPoints:error:") (retPtr retVoid) [argPtr (castPtr raw_points :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | Calculates a closed contour area using Green's theorem. The contour is represented by a set of points in VNContour object,                   It's important to note that a random set of points, or a contour with self-crossing edges will likely produce undefined results				   Note that because this is based on a geometric shape the aspect ratio is important when using normalized points.				   This takes the aspect ratio of the contour into account when using a VNContour as an input.
--
-- @area@ — Output parameter to be populated with calculated contour area
--
-- @contour@ — A VNContour object whose area is being calculated
--
-- @orientedArea@ — If true, returns signed area - positive for CCW oriented contours and negative for CW oriented contours.                           If false, returned area is always positive.
--
-- @error@ — An output parameter, populated only in case of algorithmic failure
--
-- Returns: Area calculation status, YES indicates success, NO - failure. The failure case is accompanied by populating an 'error' output parameter
--
-- ObjC selector: @+ calculateArea:forContour:orientedArea:error:@
calculateArea_forContour_orientedArea_error :: (IsVNContour contour, IsNSError error_) => Ptr CDouble -> contour -> Bool -> error_ -> IO Bool
calculateArea_forContour_orientedArea_error area contour orientedArea error_ =
  do
    cls' <- getRequiredClass "VNGeometryUtils"
    withObjCPtr contour $ \raw_contour ->
      withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "calculateArea:forContour:orientedArea:error:") retCULong [argPtr area, argPtr (castPtr raw_contour :: Ptr ()), argCULong (if orientedArea then 1 else 0), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Calculates perimeter, or a sum of all arc-lengths (edges), of a closed contour. The contour is represented by a set of points in VNContour object.				   Note that because this is based on a geometric shape the aspect ratio is important when using normalized points.				   This takes the aspect ratio of the contour into account when using a VNContour as an input.
--
-- @perimeter@ — Output parameter to be populated with calculated contour perimeter
--
-- @contour@ — A VNContour object whose perimeter is being calculated
--
-- @error@ — An output parameter, populated only in case of algorithmic failure
--
-- Returns: Perimeter calculation status, YES indicates success, NO - failure. The failure case is accompanied by populating an 'error' output parameter
--
-- ObjC selector: @+ calculatePerimeter:forContour:error:@
calculatePerimeter_forContour_error :: (IsVNContour contour, IsNSError error_) => Ptr CDouble -> contour -> error_ -> IO Bool
calculatePerimeter_forContour_error perimeter contour error_ =
  do
    cls' <- getRequiredClass "VNGeometryUtils"
    withObjCPtr contour $ \raw_contour ->
      withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "calculatePerimeter:forContour:error:") retCULong [argPtr perimeter, argPtr (castPtr raw_contour :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @boundingCircleForContour:error:@
boundingCircleForContour_errorSelector :: Selector
boundingCircleForContour_errorSelector = mkSelector "boundingCircleForContour:error:"

-- | @Selector@ for @boundingCircleForPoints:error:@
boundingCircleForPoints_errorSelector :: Selector
boundingCircleForPoints_errorSelector = mkSelector "boundingCircleForPoints:error:"

-- | @Selector@ for @calculateArea:forContour:orientedArea:error:@
calculateArea_forContour_orientedArea_errorSelector :: Selector
calculateArea_forContour_orientedArea_errorSelector = mkSelector "calculateArea:forContour:orientedArea:error:"

-- | @Selector@ for @calculatePerimeter:forContour:error:@
calculatePerimeter_forContour_errorSelector :: Selector
calculatePerimeter_forContour_errorSelector = mkSelector "calculatePerimeter:forContour:error:"

