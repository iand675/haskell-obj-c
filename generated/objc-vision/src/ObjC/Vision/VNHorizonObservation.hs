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

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Angle of the observed horizon.
--
-- ObjC selector: @- angle@
angle :: IsVNHorizonObservation vnHorizonObservation => vnHorizonObservation -> IO CDouble
angle vnHorizonObservation  =
  sendMsg vnHorizonObservation (mkSelector "angle") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @angle@
angleSelector :: Selector
angleSelector = mkSelector "angle"

