{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | VNObservation
--
-- VNObservation describes the results of performing a VNRequest. The result has a confidence score. The different types of requests will create different subclasses of VNObservation to return their results in properties of those subclasses.
--
-- Generated bindings for @VNObservation@.
module ObjC.Vision.VNObservation
  ( VNObservation
  , IsVNObservation(..)
  , uuid
  , confidence
  , uuidSelector
  , confidenceSelector


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

-- | The unique identifier assigned to an observation.
--
-- ObjC selector: @- uuid@
uuid :: IsVNObservation vnObservation => vnObservation -> IO (Id NSUUID)
uuid vnObservation  =
  sendMsg vnObservation (mkSelector "uuid") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The level of confidence normalized to [0, 1] where 1 is most confident. The only exception is results coming from VNCoreMLRequest, where confidence values are forwarded as is from relevant CoreML models
--
-- Confidence can always be returned as 1.0 if confidence is not supported or has no meaning
--
-- ObjC selector: @- confidence@
confidence :: IsVNObservation vnObservation => vnObservation -> IO CFloat
confidence vnObservation  =
  sendMsg vnObservation (mkSelector "confidence") retCFloat []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @uuid@
uuidSelector :: Selector
uuidSelector = mkSelector "uuid"

-- | @Selector@ for @confidence@
confidenceSelector :: Selector
confidenceSelector = mkSelector "confidence"

