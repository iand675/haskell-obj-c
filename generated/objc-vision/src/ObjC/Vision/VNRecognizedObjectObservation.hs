{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | VNRecognizedObjectObservation
--
-- VNDetectedObjectObservation
--
-- VNRecognizedObjectObservation is a VNDetectedObjectObservation with an array of classifications that classify the recognized object. The confidence of the classifications sum up to 1.0. It is common practice to multiply the classification confidence with the confidence of this observation.
--
-- Generated bindings for @VNRecognizedObjectObservation@.
module ObjC.Vision.VNRecognizedObjectObservation
  ( VNRecognizedObjectObservation
  , IsVNRecognizedObjectObservation(..)
  , labels
  , labelsSelector


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

-- | @- labels@
labels :: IsVNRecognizedObjectObservation vnRecognizedObjectObservation => vnRecognizedObjectObservation -> IO (Id NSArray)
labels vnRecognizedObjectObservation  =
  sendMsg vnRecognizedObjectObservation (mkSelector "labels") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @labels@
labelsSelector :: Selector
labelsSelector = mkSelector "labels"

