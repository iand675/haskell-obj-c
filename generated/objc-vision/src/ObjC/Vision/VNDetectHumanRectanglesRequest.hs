{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A request that will detect human Torsos in an image.
--
-- This request will generate VNHumanObservation objects with defined boundingBox and confidence score.
--
-- Generated bindings for @VNDetectHumanRectanglesRequest@.
module ObjC.Vision.VNDetectHumanRectanglesRequest
  ( VNDetectHumanRectanglesRequest
  , IsVNDetectHumanRectanglesRequest(..)
  , upperBodyOnly
  , setUpperBodyOnly
  , results
  , upperBodyOnlySelector
  , setUpperBodyOnlySelector
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

-- | Boolean property to specify whether the human upper body or full body needs to be detected. The default is YES, meaning the request is setup to detect upper body only
--
-- ObjC selector: @- upperBodyOnly@
upperBodyOnly :: IsVNDetectHumanRectanglesRequest vnDetectHumanRectanglesRequest => vnDetectHumanRectanglesRequest -> IO Bool
upperBodyOnly vnDetectHumanRectanglesRequest  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg vnDetectHumanRectanglesRequest (mkSelector "upperBodyOnly") retCULong []

-- | Boolean property to specify whether the human upper body or full body needs to be detected. The default is YES, meaning the request is setup to detect upper body only
--
-- ObjC selector: @- setUpperBodyOnly:@
setUpperBodyOnly :: IsVNDetectHumanRectanglesRequest vnDetectHumanRectanglesRequest => vnDetectHumanRectanglesRequest -> Bool -> IO ()
setUpperBodyOnly vnDetectHumanRectanglesRequest  value =
  sendMsg vnDetectHumanRectanglesRequest (mkSelector "setUpperBodyOnly:") retVoid [argCULong (if value then 1 else 0)]

-- | VNHumanObservation results.
--
-- ObjC selector: @- results@
results :: IsVNDetectHumanRectanglesRequest vnDetectHumanRectanglesRequest => vnDetectHumanRectanglesRequest -> IO (Id NSArray)
results vnDetectHumanRectanglesRequest  =
  sendMsg vnDetectHumanRectanglesRequest (mkSelector "results") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @upperBodyOnly@
upperBodyOnlySelector :: Selector
upperBodyOnlySelector = mkSelector "upperBodyOnly"

-- | @Selector@ for @setUpperBodyOnly:@
setUpperBodyOnlySelector :: Selector
setUpperBodyOnlySelector = mkSelector "setUpperBodyOnly:"

-- | @Selector@ for @results@
resultsSelector :: Selector
resultsSelector = mkSelector "results"

