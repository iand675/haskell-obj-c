{-# LANGUAGE PatternSynonyms #-}
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
  , revision_supportsConstellationSelector
  , constellationSelector
  , setConstellationSelector
  , resultsSelector

  -- * Enum types
  , VNRequestFaceLandmarksConstellation(VNRequestFaceLandmarksConstellation)
  , pattern VNRequestFaceLandmarksConstellationNotDefined
  , pattern VNRequestFaceLandmarksConstellation65Points
  , pattern VNRequestFaceLandmarksConstellation76Points

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

-- | @+ revision:supportsConstellation:@
revision_supportsConstellation :: CULong -> VNRequestFaceLandmarksConstellation -> IO Bool
revision_supportsConstellation requestRevision constellation =
  do
    cls' <- getRequiredClass "VNDetectFaceLandmarksRequest"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "revision:supportsConstellation:") retCULong [argCULong (fromIntegral requestRevision), argCULong (coerce constellation)]

-- | property constellation
--
-- Constellation type defines how many landmark points are used to map a face. Revisions 1, 2, and 3 of the request support 65 points, where Revision 3 also supports 76 points.
--
-- ObjC selector: @- constellation@
constellation :: IsVNDetectFaceLandmarksRequest vnDetectFaceLandmarksRequest => vnDetectFaceLandmarksRequest -> IO VNRequestFaceLandmarksConstellation
constellation vnDetectFaceLandmarksRequest  =
  fmap (coerce :: CULong -> VNRequestFaceLandmarksConstellation) $ sendMsg vnDetectFaceLandmarksRequest (mkSelector "constellation") retCULong []

-- | property constellation
--
-- Constellation type defines how many landmark points are used to map a face. Revisions 1, 2, and 3 of the request support 65 points, where Revision 3 also supports 76 points.
--
-- ObjC selector: @- setConstellation:@
setConstellation :: IsVNDetectFaceLandmarksRequest vnDetectFaceLandmarksRequest => vnDetectFaceLandmarksRequest -> VNRequestFaceLandmarksConstellation -> IO ()
setConstellation vnDetectFaceLandmarksRequest  value =
  sendMsg vnDetectFaceLandmarksRequest (mkSelector "setConstellation:") retVoid [argCULong (coerce value)]

-- | VNFaceObservation with populated landmarks-related properties results.
--
-- ObjC selector: @- results@
results :: IsVNDetectFaceLandmarksRequest vnDetectFaceLandmarksRequest => vnDetectFaceLandmarksRequest -> IO (Id NSArray)
results vnDetectFaceLandmarksRequest  =
  sendMsg vnDetectFaceLandmarksRequest (mkSelector "results") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @revision:supportsConstellation:@
revision_supportsConstellationSelector :: Selector
revision_supportsConstellationSelector = mkSelector "revision:supportsConstellation:"

-- | @Selector@ for @constellation@
constellationSelector :: Selector
constellationSelector = mkSelector "constellation"

-- | @Selector@ for @setConstellation:@
setConstellationSelector :: Selector
setConstellationSelector = mkSelector "setConstellation:"

-- | @Selector@ for @results@
resultsSelector :: Selector
resultsSelector = mkSelector "results"

