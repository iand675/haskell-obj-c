{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | VNRecognizedText
--
-- VNRecognizedText A block of recognized text. There can be multiple VNRecognizedText objects returned in a VNRecognizedTextObservation - one for each candidate.
--
-- Generated bindings for @VNRecognizedText@.
module ObjC.Vision.VNRecognizedText
  ( VNRecognizedText
  , IsVNRecognizedText(..)
  , boundingBoxForRange_error
  , string
  , confidence
  , boundingBoxForRange_errorSelector
  , stringSelector
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
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | Calculate the bounding box around the characters in the range of the string.
--
-- The bounding boxes are not guaranteed to be an exact fit around the characters and are purely meant for UI purposes and not for image processing.
--
-- ObjC selector: @- boundingBoxForRange:error:@
boundingBoxForRange_error :: (IsVNRecognizedText vnRecognizedText, IsNSError error_) => vnRecognizedText -> NSRange -> error_ -> IO (Id VNRectangleObservation)
boundingBoxForRange_error vnRecognizedText  range error_ =
withObjCPtr error_ $ \raw_error_ ->
    sendMsg vnRecognizedText (mkSelector "boundingBoxForRange:error:") (retPtr retVoid) [argNSRange range, argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | Field that contains recognized text.
--
-- This is the top candidate of the recognized text.
--
-- ObjC selector: @- string@
string :: IsVNRecognizedText vnRecognizedText => vnRecognizedText -> IO (Id NSString)
string vnRecognizedText  =
  sendMsg vnRecognizedText (mkSelector "string") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The level of confidence normalized to [0.0, 1.0] where 1.0 is most confident
--
-- ObjC selector: @- confidence@
confidence :: IsVNRecognizedText vnRecognizedText => vnRecognizedText -> IO CFloat
confidence vnRecognizedText  =
  sendMsg vnRecognizedText (mkSelector "confidence") retCFloat []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @boundingBoxForRange:error:@
boundingBoxForRange_errorSelector :: Selector
boundingBoxForRange_errorSelector = mkSelector "boundingBoxForRange:error:"

-- | @Selector@ for @string@
stringSelector :: Selector
stringSelector = mkSelector "string"

-- | @Selector@ for @confidence@
confidenceSelector :: Selector
confidenceSelector = mkSelector "confidence"

