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
  , contourAtIndex_errorSelector
  , contourAtIndexPath_errorSelector
  , contourCountSelector
  , topLevelContourCountSelector
  , topLevelContoursSelector
  , normalizedPathSelector


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
contourAtIndex_error vnContoursObservation  contourIndex error_ =
withObjCPtr error_ $ \raw_error_ ->
    sendMsg vnContoursObservation (mkSelector "contourAtIndex:error:") (retPtr retVoid) [argCLong (fromIntegral contourIndex), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

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
contourAtIndexPath_error vnContoursObservation  indexPath error_ =
withObjCPtr indexPath $ \raw_indexPath ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg vnContoursObservation (mkSelector "contourAtIndexPath:error:") (retPtr retVoid) [argPtr (castPtr raw_indexPath :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | The total number of contours detected.
--
-- ObjC selector: @- contourCount@
contourCount :: IsVNContoursObservation vnContoursObservation => vnContoursObservation -> IO CLong
contourCount vnContoursObservation  =
  sendMsg vnContoursObservation (mkSelector "contourCount") retCLong []

-- | The total number of top-level contours detected.
--
-- ObjC selector: @- topLevelContourCount@
topLevelContourCount :: IsVNContoursObservation vnContoursObservation => vnContoursObservation -> IO CLong
topLevelContourCount vnContoursObservation  =
  sendMsg vnContoursObservation (mkSelector "topLevelContourCount") retCLong []

-- | An array of the top level contours (i.e. contours that are not enclosed inside another contour),.
--
-- This array constitutes the top of the contour hierarchy. Each contour object can be further iterated to determine its children.
--
-- See: VNContour for more information.
--
-- ObjC selector: @- topLevelContours@
topLevelContours :: IsVNContoursObservation vnContoursObservation => vnContoursObservation -> IO (Id NSArray)
topLevelContours vnContoursObservation  =
  sendMsg vnContoursObservation (mkSelector "topLevelContours") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Obtain all of the contours represented as a CGPath in normalized coordinates.
--
-- The path is owned by the observation and therefore will be alive as long as the the observation is alive.
--
-- ObjC selector: @- normalizedPath@
normalizedPath :: IsVNContoursObservation vnContoursObservation => vnContoursObservation -> IO RawId
normalizedPath vnContoursObservation  =
  fmap (RawId . castPtr) $ sendMsg vnContoursObservation (mkSelector "normalizedPath") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @contourAtIndex:error:@
contourAtIndex_errorSelector :: Selector
contourAtIndex_errorSelector = mkSelector "contourAtIndex:error:"

-- | @Selector@ for @contourAtIndexPath:error:@
contourAtIndexPath_errorSelector :: Selector
contourAtIndexPath_errorSelector = mkSelector "contourAtIndexPath:error:"

-- | @Selector@ for @contourCount@
contourCountSelector :: Selector
contourCountSelector = mkSelector "contourCount"

-- | @Selector@ for @topLevelContourCount@
topLevelContourCountSelector :: Selector
topLevelContourCountSelector = mkSelector "topLevelContourCount"

-- | @Selector@ for @topLevelContours@
topLevelContoursSelector :: Selector
topLevelContoursSelector = mkSelector "topLevelContours"

-- | @Selector@ for @normalizedPath@
normalizedPathSelector :: Selector
normalizedPathSelector = mkSelector "normalizedPath"

