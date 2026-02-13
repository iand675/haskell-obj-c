{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The VNContour class describes a contour provided by a VNContoursObservation.
--
-- VNContour objects are lightweight objects that act as a façade which allows access to a small slice of the usually much larger block of data owned by a VNContoursObservation that represents all of the contours detected in an image.				While the interface does present the notion of a hierarchy of parent/child contours, the implementation purposefully does not contain any explicit internal bookkeeping for this relationship.  Instead, contours are uniquely identified via their indexPath property.				As a side effect of this choice, repeated calls to methods that would return relational contours (e.g., -childContours or -childContourAtIndex:error:) are NOT guaranteed to return the same VNContour instances over and over again.  If this kind of parent/child object stability is an absolute requirement of the client, then they are responsible for creating the necessary data structures to represent and build that instance-stable hierarchy.
--
-- Generated bindings for @VNContour@.
module ObjC.Vision.VNContour
  ( VNContour
  , IsVNContour(..)
  , new
  , init_
  , childContourAtIndex_error
  , polygonApproximationWithEpsilon_error
  , indexPath
  , childContourCount
  , childContours
  , pointCount
  , normalizedPoints
  , normalizedPath
  , aspectRatio
  , aspectRatioSelector
  , childContourAtIndex_errorSelector
  , childContourCountSelector
  , childContoursSelector
  , indexPathSelector
  , initSelector
  , newSelector
  , normalizedPathSelector
  , normalizedPointsSelector
  , pointCountSelector
  , polygonApproximationWithEpsilon_errorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id VNContour)
new  =
  do
    cls' <- getRequiredClass "VNContour"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsVNContour vnContour => vnContour -> IO (Id VNContour)
init_ vnContour =
  sendOwnedMessage vnContour initSelector

-- | Returns a VNContour object that is a child of this VNContour at the specified index.
--
-- @childContourIndex@ — The index into the childContours array.
--
-- @error@ — The error returned if the child contour cannot be provided.
--
-- Returns: The VNContour object at the specified index path, or nil of a failure occurs.
--
-- ObjC selector: @- childContourAtIndex:error:@
childContourAtIndex_error :: (IsVNContour vnContour, IsNSError error_) => vnContour -> CULong -> error_ -> IO (Id VNContour)
childContourAtIndex_error vnContour childContourIndex error_ =
  sendMessage vnContour childContourAtIndex_errorSelector childContourIndex (toNSError error_)

-- | Simplifies the contour's collection of points into a polygon using the Ramer Douglas Peucker Algorithm.
--
-- See <https://en.wikipedia.org/wiki/Ramer–Douglas–Peucker_algorithm>
--
-- @epsilon@ — Points that have a perpendicular distance to the line segment they are on which are greater than epsilon are kept, others are eliminated.
--
-- @error@ — The error returned if a simplified contour cannot be created.
--
-- Returns: A new VNContour object with a simplified polygon consisting of a subset of the points that defined the original VNContour.
--
-- ObjC selector: @- polygonApproximationWithEpsilon:error:@
polygonApproximationWithEpsilon_error :: (IsVNContour vnContour, IsNSError error_) => vnContour -> CFloat -> error_ -> IO (Id VNContour)
polygonApproximationWithEpsilon_error vnContour epsilon error_ =
  sendMessage vnContour polygonApproximationWithEpsilon_errorSelector epsilon (toNSError error_)

-- | The path to the target VNContour as it is stored in the owning VNContoursObservation's hierarchy of contours.
--
-- ObjC selector: @- indexPath@
indexPath :: IsVNContour vnContour => vnContour -> IO (Id NSIndexPath)
indexPath vnContour =
  sendMessage vnContour indexPathSelector

-- | The total number of child contours in the target contour.
--
-- The use of this property is preferred over childContours.count due to the cost of building the child objects.
--
-- ObjC selector: @- childContourCount@
childContourCount :: IsVNContour vnContour => vnContour -> IO CLong
childContourCount vnContour =
  sendMessage vnContour childContourCountSelector

-- | The array of the contours enclosed by the target contour.
--
-- This property may come with the cost of instantiating new VNContour objects; therefore, clients are strongly encouraged to hold the results in a local variable instead of repeatedly invoking it.
--
-- ObjC selector: @- childContours@
childContours :: IsVNContour vnContour => vnContour -> IO (Id NSArray)
childContours vnContour =
  sendMessage vnContour childContoursSelector

-- | The number of points that describe the contour.
--
-- ObjC selector: @- pointCount@
pointCount :: IsVNContour vnContour => vnContour -> IO CLong
pointCount vnContour =
  sendMessage vnContour pointCountSelector

-- | The array of points in normalized coordinates that describe the contour.
--
-- Provides the address of a buffer containing the array of (x,y) points stored as a simd_float2 value.  This buffer is owned by the target object and is guaranteed to exist as long as this VNContour instance exists.
--
-- ObjC selector: @- normalizedPoints@
normalizedPoints :: IsVNContour vnContour => vnContour -> IO RawId
normalizedPoints vnContour =
  sendMessage vnContour normalizedPointsSelector

-- | The contour represented as a CGPath in normalized coordinates.
--
-- The path is owned by this object and therefore will be alive as long as the the observation is alive.
--
-- ObjC selector: @- normalizedPath@
normalizedPath :: IsVNContour vnContour => vnContour -> IO RawId
normalizedPath vnContour =
  sendMessage vnContour normalizedPathSelector

-- | The aspect ratio of the contour from the original image aspect ratio expressed as width/height
--
-- ObjC selector: @- aspectRatio@
aspectRatio :: IsVNContour vnContour => vnContour -> IO CFloat
aspectRatio vnContour =
  sendMessage vnContour aspectRatioSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VNContour)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VNContour)
initSelector = mkSelector "init"

-- | @Selector@ for @childContourAtIndex:error:@
childContourAtIndex_errorSelector :: Selector '[CULong, Id NSError] (Id VNContour)
childContourAtIndex_errorSelector = mkSelector "childContourAtIndex:error:"

-- | @Selector@ for @polygonApproximationWithEpsilon:error:@
polygonApproximationWithEpsilon_errorSelector :: Selector '[CFloat, Id NSError] (Id VNContour)
polygonApproximationWithEpsilon_errorSelector = mkSelector "polygonApproximationWithEpsilon:error:"

-- | @Selector@ for @indexPath@
indexPathSelector :: Selector '[] (Id NSIndexPath)
indexPathSelector = mkSelector "indexPath"

-- | @Selector@ for @childContourCount@
childContourCountSelector :: Selector '[] CLong
childContourCountSelector = mkSelector "childContourCount"

-- | @Selector@ for @childContours@
childContoursSelector :: Selector '[] (Id NSArray)
childContoursSelector = mkSelector "childContours"

-- | @Selector@ for @pointCount@
pointCountSelector :: Selector '[] CLong
pointCountSelector = mkSelector "pointCount"

-- | @Selector@ for @normalizedPoints@
normalizedPointsSelector :: Selector '[] RawId
normalizedPointsSelector = mkSelector "normalizedPoints"

-- | @Selector@ for @normalizedPath@
normalizedPathSelector :: Selector '[] RawId
normalizedPathSelector = mkSelector "normalizedPath"

-- | @Selector@ for @aspectRatio@
aspectRatioSelector :: Selector '[] CFloat
aspectRatioSelector = mkSelector "aspectRatio"

