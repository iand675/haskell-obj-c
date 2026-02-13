{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A request that will produce face landmark information.
--
-- This request will generate VNFaceObservation objects with the landmarks property populated with information describing face landmarks. If VNFaceObservations are provided via the VNFaceObservationAccepting protocol without the landmarks property populated, new observations will be created as copies of the input VNFaceObservations with the landmarks property populated. If the landmarks property has already been populated, the original VNFaceObservations will be returned. If no VNFaceObservations are provided, face detection will be run first.
--
-- Generated bindings for @VNDetectFaceLandmarksRequest@.
module ObjC.Vision.VNDetectFaceLandmarksRequest
  ( VNDetectFaceLandmarksRequest
  , IsVNDetectFaceLandmarksRequest(..)
  , revision_supportsConstellation
  , constellation
  , setConstellation
  , results
  , constellationSelector
  , resultsSelector
  , revision_supportsConstellationSelector
  , setConstellationSelector

  -- * Enum types
  , VNRequestFaceLandmarksConstellation(VNRequestFaceLandmarksConstellation)
  , pattern VNRequestFaceLandmarksConstellationNotDefined
  , pattern VNRequestFaceLandmarksConstellation65Points
  , pattern VNRequestFaceLandmarksConstellation76Points

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

-- | @+ revision:supportsConstellation:@
revision_supportsConstellation :: CULong -> VNRequestFaceLandmarksConstellation -> IO Bool
revision_supportsConstellation requestRevision constellation =
  do
    cls' <- getRequiredClass "VNDetectFaceLandmarksRequest"
    sendClassMessage cls' revision_supportsConstellationSelector requestRevision constellation

-- | property constellation
--
-- Constellation type defines how many landmark points are used to map a face. Revisions 1, 2, and 3 of the request support 65 points, where Revision 3 also supports 76 points.
--
-- ObjC selector: @- constellation@
constellation :: IsVNDetectFaceLandmarksRequest vnDetectFaceLandmarksRequest => vnDetectFaceLandmarksRequest -> IO VNRequestFaceLandmarksConstellation
constellation vnDetectFaceLandmarksRequest =
  sendMessage vnDetectFaceLandmarksRequest constellationSelector

-- | property constellation
--
-- Constellation type defines how many landmark points are used to map a face. Revisions 1, 2, and 3 of the request support 65 points, where Revision 3 also supports 76 points.
--
-- ObjC selector: @- setConstellation:@
setConstellation :: IsVNDetectFaceLandmarksRequest vnDetectFaceLandmarksRequest => vnDetectFaceLandmarksRequest -> VNRequestFaceLandmarksConstellation -> IO ()
setConstellation vnDetectFaceLandmarksRequest value =
  sendMessage vnDetectFaceLandmarksRequest setConstellationSelector value

-- | VNFaceObservation with populated landmarks-related properties results.
--
-- ObjC selector: @- results@
results :: IsVNDetectFaceLandmarksRequest vnDetectFaceLandmarksRequest => vnDetectFaceLandmarksRequest -> IO (Id NSArray)
results vnDetectFaceLandmarksRequest =
  sendMessage vnDetectFaceLandmarksRequest resultsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @revision:supportsConstellation:@
revision_supportsConstellationSelector :: Selector '[CULong, VNRequestFaceLandmarksConstellation] Bool
revision_supportsConstellationSelector = mkSelector "revision:supportsConstellation:"

-- | @Selector@ for @constellation@
constellationSelector :: Selector '[] VNRequestFaceLandmarksConstellation
constellationSelector = mkSelector "constellation"

-- | @Selector@ for @setConstellation:@
setConstellationSelector :: Selector '[VNRequestFaceLandmarksConstellation] ()
setConstellationSelector = mkSelector "setConstellation:"

-- | @Selector@ for @results@
resultsSelector :: Selector '[] (Id NSArray)
resultsSelector = mkSelector "results"

