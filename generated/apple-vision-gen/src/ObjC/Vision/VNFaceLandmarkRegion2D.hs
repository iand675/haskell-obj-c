{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | VNFaceLandmarkRegion2D
--
-- VNFaceLandmarkRegion2D gives access to the 2D landmark points for the region. The points are stored as vector_float2 and must not be modified.
--
-- Generated bindings for @VNFaceLandmarkRegion2D@.
module ObjC.Vision.VNFaceLandmarkRegion2D
  ( VNFaceLandmarkRegion2D
  , IsVNFaceLandmarkRegion2D(..)
  , normalizedPoints
  , precisionEstimatesPerPoint
  , pointsClassification
  , normalizedPointsSelector
  , precisionEstimatesPerPointSelector
  , pointsClassificationSelector

  -- * Enum types
  , VNPointsClassification(VNPointsClassification)
  , pattern VNPointsClassificationDisconnected
  , pattern VNPointsClassificationOpenPath
  , pattern VNPointsClassificationClosedPath

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
import ObjC.Vision.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Obtains the array of normalized landmark points.
--
-- Provides the address of a buffer containing the array of CGPoints representing the landmark points.  This buffer is owned by the target object and is guaranteed to exist as long as the VNFaceLandmarkRegion2D does.
--
-- Returns: the address of the array of pointCount points.
--
-- ObjC selector: @- normalizedPoints@
normalizedPoints :: IsVNFaceLandmarkRegion2D vnFaceLandmarkRegion2D => vnFaceLandmarkRegion2D -> IO RawId
normalizedPoints vnFaceLandmarkRegion2D  =
    fmap (RawId . castPtr) $ sendMsg vnFaceLandmarkRegion2D (mkSelector "normalizedPoints") (retPtr retVoid) []

-- | Obtains the array of accuracy placement estimates per landmark point.
--
-- Provides the NSArray object containing landmarks accuracy placement estimates per landmark point. This property is only                populated when VNDetectFaceLandmarksRequest object is configured with VNRequestFaceLandmarksConstellation76Points. It is                set to nil for other constellations
--
-- Returns: NSArray object of NSNumber(s) initialized to floating point values.
--
-- ObjC selector: @- precisionEstimatesPerPoint@
precisionEstimatesPerPoint :: IsVNFaceLandmarkRegion2D vnFaceLandmarkRegion2D => vnFaceLandmarkRegion2D -> IO (Id NSArray)
precisionEstimatesPerPoint vnFaceLandmarkRegion2D  =
    sendMsg vnFaceLandmarkRegion2D (mkSelector "precisionEstimatesPerPoint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Describes how to interpret the points provided by the region.
--
-- ObjC selector: @- pointsClassification@
pointsClassification :: IsVNFaceLandmarkRegion2D vnFaceLandmarkRegion2D => vnFaceLandmarkRegion2D -> IO VNPointsClassification
pointsClassification vnFaceLandmarkRegion2D  =
    fmap (coerce :: CLong -> VNPointsClassification) $ sendMsg vnFaceLandmarkRegion2D (mkSelector "pointsClassification") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @normalizedPoints@
normalizedPointsSelector :: Selector
normalizedPointsSelector = mkSelector "normalizedPoints"

-- | @Selector@ for @precisionEstimatesPerPoint@
precisionEstimatesPerPointSelector :: Selector
precisionEstimatesPerPointSelector = mkSelector "precisionEstimatesPerPoint"

-- | @Selector@ for @pointsClassification@
pointsClassificationSelector :: Selector
pointsClassificationSelector = mkSelector "pointsClassification"

