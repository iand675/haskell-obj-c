{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A request that will detect faces in an image.
--
-- This request will generate VNFaceObservation objects with a defined boundingBox.
--
-- Generated bindings for @VNDetectFaceRectanglesRequest@.
module ObjC.Vision.VNDetectFaceRectanglesRequest
  ( VNDetectFaceRectanglesRequest
  , IsVNDetectFaceRectanglesRequest(..)
  , results
  , resultsSelector


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

-- | VNFaceObservation results.
--
-- ObjC selector: @- results@
results :: IsVNDetectFaceRectanglesRequest vnDetectFaceRectanglesRequest => vnDetectFaceRectanglesRequest -> IO (Id NSArray)
results vnDetectFaceRectanglesRequest  =
  sendMsg vnDetectFaceRectanglesRequest (mkSelector "results") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @results@
resultsSelector :: Selector
resultsSelector = mkSelector "results"

