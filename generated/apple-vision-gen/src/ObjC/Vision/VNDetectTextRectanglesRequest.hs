{-# LANGUAGE DataKinds #-}
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
  , resultsSelector
  , setReportCharacterBoxesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Specify whether or not the bounding boxes of individual characters should also be returned in the resultant VNTextObservations. Default is NO.
--
-- ObjC selector: @- reportCharacterBoxes@
reportCharacterBoxes :: IsVNDetectTextRectanglesRequest vnDetectTextRectanglesRequest => vnDetectTextRectanglesRequest -> IO Bool
reportCharacterBoxes vnDetectTextRectanglesRequest =
  sendMessage vnDetectTextRectanglesRequest reportCharacterBoxesSelector

-- | Specify whether or not the bounding boxes of individual characters should also be returned in the resultant VNTextObservations. Default is NO.
--
-- ObjC selector: @- setReportCharacterBoxes:@
setReportCharacterBoxes :: IsVNDetectTextRectanglesRequest vnDetectTextRectanglesRequest => vnDetectTextRectanglesRequest -> Bool -> IO ()
setReportCharacterBoxes vnDetectTextRectanglesRequest value =
  sendMessage vnDetectTextRectanglesRequest setReportCharacterBoxesSelector value

-- | VNTextObservation results.
--
-- ObjC selector: @- results@
results :: IsVNDetectTextRectanglesRequest vnDetectTextRectanglesRequest => vnDetectTextRectanglesRequest -> IO (Id NSArray)
results vnDetectTextRectanglesRequest =
  sendMessage vnDetectTextRectanglesRequest resultsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @reportCharacterBoxes@
reportCharacterBoxesSelector :: Selector '[] Bool
reportCharacterBoxesSelector = mkSelector "reportCharacterBoxes"

-- | @Selector@ for @setReportCharacterBoxes:@
setReportCharacterBoxesSelector :: Selector '[Bool] ()
setReportCharacterBoxesSelector = mkSelector "setReportCharacterBoxes:"

-- | @Selector@ for @results@
resultsSelector :: Selector '[] (Id NSArray)
resultsSelector = mkSelector "results"

