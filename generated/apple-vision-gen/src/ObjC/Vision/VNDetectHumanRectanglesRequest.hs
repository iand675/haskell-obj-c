{-# LANGUAGE DataKinds #-}
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
  , resultsSelector
  , setUpperBodyOnlySelector
  , upperBodyOnlySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Boolean property to specify whether the human upper body or full body needs to be detected. The default is YES, meaning the request is setup to detect upper body only
--
-- ObjC selector: @- upperBodyOnly@
upperBodyOnly :: IsVNDetectHumanRectanglesRequest vnDetectHumanRectanglesRequest => vnDetectHumanRectanglesRequest -> IO Bool
upperBodyOnly vnDetectHumanRectanglesRequest =
  sendMessage vnDetectHumanRectanglesRequest upperBodyOnlySelector

-- | Boolean property to specify whether the human upper body or full body needs to be detected. The default is YES, meaning the request is setup to detect upper body only
--
-- ObjC selector: @- setUpperBodyOnly:@
setUpperBodyOnly :: IsVNDetectHumanRectanglesRequest vnDetectHumanRectanglesRequest => vnDetectHumanRectanglesRequest -> Bool -> IO ()
setUpperBodyOnly vnDetectHumanRectanglesRequest value =
  sendMessage vnDetectHumanRectanglesRequest setUpperBodyOnlySelector value

-- | VNHumanObservation results.
--
-- ObjC selector: @- results@
results :: IsVNDetectHumanRectanglesRequest vnDetectHumanRectanglesRequest => vnDetectHumanRectanglesRequest -> IO (Id NSArray)
results vnDetectHumanRectanglesRequest =
  sendMessage vnDetectHumanRectanglesRequest resultsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @upperBodyOnly@
upperBodyOnlySelector :: Selector '[] Bool
upperBodyOnlySelector = mkSelector "upperBodyOnly"

-- | @Selector@ for @setUpperBodyOnly:@
setUpperBodyOnlySelector :: Selector '[Bool] ()
setUpperBodyOnlySelector = mkSelector "setUpperBodyOnly:"

-- | @Selector@ for @results@
resultsSelector :: Selector '[] (Id NSArray)
resultsSelector = mkSelector "results"

