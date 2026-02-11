{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A request that will detect regions of text in an image.
--
-- This request will generate VNTextObservation objects describing the locations of text detected in an image.
--
-- Generated bindings for @VNDetectTextRectanglesRequest@.
module ObjC.Vision.VNDetectTextRectanglesRequest
  ( VNDetectTextRectanglesRequest
  , IsVNDetectTextRectanglesRequest(..)
  , reportCharacterBoxes
  , setReportCharacterBoxes
  , results
  , reportCharacterBoxesSelector
  , setReportCharacterBoxesSelector
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

-- | Specify whether or not the bounding boxes of individual characters should also be returned in the resultant VNTextObservations. Default is NO.
--
-- ObjC selector: @- reportCharacterBoxes@
reportCharacterBoxes :: IsVNDetectTextRectanglesRequest vnDetectTextRectanglesRequest => vnDetectTextRectanglesRequest -> IO Bool
reportCharacterBoxes vnDetectTextRectanglesRequest  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg vnDetectTextRectanglesRequest (mkSelector "reportCharacterBoxes") retCULong []

-- | Specify whether or not the bounding boxes of individual characters should also be returned in the resultant VNTextObservations. Default is NO.
--
-- ObjC selector: @- setReportCharacterBoxes:@
setReportCharacterBoxes :: IsVNDetectTextRectanglesRequest vnDetectTextRectanglesRequest => vnDetectTextRectanglesRequest -> Bool -> IO ()
setReportCharacterBoxes vnDetectTextRectanglesRequest  value =
  sendMsg vnDetectTextRectanglesRequest (mkSelector "setReportCharacterBoxes:") retVoid [argCULong (if value then 1 else 0)]

-- | VNTextObservation results.
--
-- ObjC selector: @- results@
results :: IsVNDetectTextRectanglesRequest vnDetectTextRectanglesRequest => vnDetectTextRectanglesRequest -> IO (Id NSArray)
results vnDetectTextRectanglesRequest  =
  sendMsg vnDetectTextRectanglesRequest (mkSelector "results") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @reportCharacterBoxes@
reportCharacterBoxesSelector :: Selector
reportCharacterBoxesSelector = mkSelector "reportCharacterBoxes"

-- | @Selector@ for @setReportCharacterBoxes:@
setReportCharacterBoxesSelector :: Selector
setReportCharacterBoxesSelector = mkSelector "setReportCharacterBoxes:"

-- | @Selector@ for @results@
resultsSelector :: Selector
resultsSelector = mkSelector "results"

