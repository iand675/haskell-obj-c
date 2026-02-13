{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | VNHumanObservation
--
-- VNDetectedObjectObservation
--
-- VNHumanObservation is the result of a Human rectangles detection request
--
-- Generated bindings for @VNHumanObservation@.
module ObjC.Vision.VNHumanObservation
  ( VNHumanObservation
  , IsVNHumanObservation(..)
  , upperBodyOnly
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

-- | Boolean property to specify whether the human upper body or full body detection is recorded in the observation. This setting is propagated from [VNDetectHumanRectanglesRequest -upperBodyOnly]
--
-- ObjC selector: @- upperBodyOnly@
upperBodyOnly :: IsVNHumanObservation vnHumanObservation => vnHumanObservation -> IO Bool
upperBodyOnly vnHumanObservation =
  sendMessage vnHumanObservation upperBodyOnlySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @upperBodyOnly@
upperBodyOnlySelector :: Selector '[] Bool
upperBodyOnlySelector = mkSelector "upperBodyOnly"

