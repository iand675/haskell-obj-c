{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @VNContoursObservation@.
module ObjC.Vision.VNContoursObservation
  ( VNContoursObservation
  , IsVNContoursObservation(..)
  , contourAtIndex_error
  , contourAtIndexPath_error
  , contourCount
  , topLevelContourCount
  , topLevelContours
  , normalizedPath
  , contourAtIndexPath_errorSelector
  , contourAtIndex_errorSelector
  , contourCountSelector
  , normalizedPathSelector
  , topLevelContourCountSelector
  , topLevelContoursSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Returns the VNContour object at the specified index, irrespective of hierarchy.
--
-- @contourIndex@ — The index of the contour to request. Valid values are in the range [0..contourCount-1].
--
-- @error@ — The error returned if the index path is out of range.
--
-- Returns: The detected VNContour at the specified index without regard to hierarchy.
--
-- ObjC selector: @- contourAtIndex:error:@
contourAtIndex_error :: (IsVNContoursObservation vnContoursObservation, IsNSError error_) => vnContoursObservation -> CLong -> error_ -> IO (Id VNContour)
contourAtIndex_error vnContoursObservation contourIndex error_ =
  sendMessage vnContoursObservation contourAtIndex_errorSelector contourIndex (toNSError error_)

-- | Returns the VNContour object at the specified index path.
--
-- Use the indexPath property from a VNContour instance to pass to this method.
--
-- @indexPath@ — The index path is the heirarchical path to the contour.
--
-- @error@ — The error returned if the index path is out of range.
--
-- Returns: The VNContour object at the specified index path.
--
-- ObjC selector: @- contourAtIndexPath:error:@
contourAtIndexPath_error :: (IsVNContoursObservation vnContoursObservation, IsNSIndexPath indexPath, IsNSError error_) => vnContoursObservation -> indexPath -> error_ -> IO (Id VNContour)
contourAtIndexPath_error vnContoursObservation indexPath error_ =
  sendMessage vnContoursObservation contourAtIndexPath_errorSelector (toNSIndexPath indexPath) (toNSError error_)

-- | The total number of contours detected.
--
-- ObjC selector: @- contourCount@
contourCount :: IsVNContoursObservation vnContoursObservation => vnContoursObservation -> IO CLong
contourCount vnContoursObservation =
  sendMessage vnContoursObservation contourCountSelector

-- | The total number of top-level contours detected.
--
-- ObjC selector: @- topLevelContourCount@
topLevelContourCount :: IsVNContoursObservation vnContoursObservation => vnContoursObservation -> IO CLong
topLevelContourCount vnContoursObservation =
  sendMessage vnContoursObservation topLevelContourCountSelector

-- | An array of the top level contours (i.e. contours that are not enclosed inside another contour),.
--
-- This array constitutes the top of the contour hierarchy. Each contour object can be further iterated to determine its children.
--
-- See: VNContour for more information.
--
-- ObjC selector: @- topLevelContours@
topLevelContours :: IsVNContoursObservation vnContoursObservation => vnContoursObservation -> IO (Id NSArray)
topLevelContours vnContoursObservation =
  sendMessage vnContoursObservation topLevelContoursSelector

-- | Obtain all of the contours represented as a CGPath in normalized coordinates.
--
-- The path is owned by the observation and therefore will be alive as long as the the observation is alive.
--
-- ObjC selector: @- normalizedPath@
normalizedPath :: IsVNContoursObservation vnContoursObservation => vnContoursObservation -> IO RawId
normalizedPath vnContoursObservation =
  sendMessage vnContoursObservation normalizedPathSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @contourAtIndex:error:@
contourAtIndex_errorSelector :: Selector '[CLong, Id NSError] (Id VNContour)
contourAtIndex_errorSelector = mkSelector "contourAtIndex:error:"

-- | @Selector@ for @contourAtIndexPath:error:@
contourAtIndexPath_errorSelector :: Selector '[Id NSIndexPath, Id NSError] (Id VNContour)
contourAtIndexPath_errorSelector = mkSelector "contourAtIndexPath:error:"

-- | @Selector@ for @contourCount@
contourCountSelector :: Selector '[] CLong
contourCountSelector = mkSelector "contourCount"

-- | @Selector@ for @topLevelContourCount@
topLevelContourCountSelector :: Selector '[] CLong
topLevelContourCountSelector = mkSelector "topLevelContourCount"

-- | @Selector@ for @topLevelContours@
topLevelContoursSelector :: Selector '[] (Id NSArray)
topLevelContoursSelector = mkSelector "topLevelContours"

-- | @Selector@ for @normalizedPath@
normalizedPathSelector :: Selector '[] RawId
normalizedPathSelector = mkSelector "normalizedPath"

