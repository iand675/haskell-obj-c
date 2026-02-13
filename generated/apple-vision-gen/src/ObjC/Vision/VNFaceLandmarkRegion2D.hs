{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , pointsClassificationSelector
  , precisionEstimatesPerPointSelector

  -- * Enum types
  , VNPointsClassification(VNPointsClassification)
  , pattern VNPointsClassificationDisconnected
  , pattern VNPointsClassificationOpenPath
  , pattern VNPointsClassificationClosedPath

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
normalizedPoints vnFaceLandmarkRegion2D =
  sendMessage vnFaceLandmarkRegion2D normalizedPointsSelector

-- | Obtains the array of accuracy placement estimates per landmark point.
--
-- Provides the NSArray object containing landmarks accuracy placement estimates per landmark point. This property is only                populated when VNDetectFaceLandmarksRequest object is configured with VNRequestFaceLandmarksConstellation76Points. It is                set to nil for other constellations
--
-- Returns: NSArray object of NSNumber(s) initialized to floating point values.
--
-- ObjC selector: @- precisionEstimatesPerPoint@
precisionEstimatesPerPoint :: IsVNFaceLandmarkRegion2D vnFaceLandmarkRegion2D => vnFaceLandmarkRegion2D -> IO (Id NSArray)
precisionEstimatesPerPoint vnFaceLandmarkRegion2D =
  sendMessage vnFaceLandmarkRegion2D precisionEstimatesPerPointSelector

-- | Describes how to interpret the points provided by the region.
--
-- ObjC selector: @- pointsClassification@
pointsClassification :: IsVNFaceLandmarkRegion2D vnFaceLandmarkRegion2D => vnFaceLandmarkRegion2D -> IO VNPointsClassification
pointsClassification vnFaceLandmarkRegion2D =
  sendMessage vnFaceLandmarkRegion2D pointsClassificationSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @normalizedPoints@
normalizedPointsSelector :: Selector '[] RawId
normalizedPointsSelector = mkSelector "normalizedPoints"

-- | @Selector@ for @precisionEstimatesPerPoint@
precisionEstimatesPerPointSelector :: Selector '[] (Id NSArray)
precisionEstimatesPerPointSelector = mkSelector "precisionEstimatesPerPoint"

-- | @Selector@ for @pointsClassification@
pointsClassificationSelector :: Selector '[] VNPointsClassification
pointsClassificationSelector = mkSelector "pointsClassification"

