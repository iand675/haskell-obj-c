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

-- | Boolean property to specify whether the human upper body or full body detection is recorded in the observation. This setting is propagated from [VNDetectHumanRectanglesRequest -upperBodyOnly]
--
-- ObjC selector: @- upperBodyOnly@
upperBodyOnly :: IsVNHumanObservation vnHumanObservation => vnHumanObservation -> IO Bool
upperBodyOnly vnHumanObservation  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg vnHumanObservation (mkSelector "upperBodyOnly") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @upperBodyOnly@
upperBodyOnlySelector :: Selector
upperBodyOnlySelector = mkSelector "upperBodyOnly"

