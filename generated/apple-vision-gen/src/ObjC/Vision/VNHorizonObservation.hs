{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | VNHorizonObservation
--
-- VNObservation
--
-- VNHorizonObservation is the result of a VNDetectHorizonRequest
--
-- Use the transform or angle to upright the image and make the detected horizon level.
--
-- Generated bindings for @VNHorizonObservation@.
module ObjC.Vision.VNHorizonObservation
  ( VNHorizonObservation
  , IsVNHorizonObservation(..)
  , angle
  , angleSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Angle of the observed horizon.
--
-- ObjC selector: @- angle@
angle :: IsVNHorizonObservation vnHorizonObservation => vnHorizonObservation -> IO CDouble
angle vnHorizonObservation =
  sendMessage vnHorizonObservation angleSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @angle@
angleSelector :: Selector '[] CDouble
angleSelector = mkSelector "angle"

