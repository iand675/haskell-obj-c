{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCaptionConversionWarning
--
-- Reports a specific problem encountered during the validation of a caption conversion.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVCaptionConversionWarning@.
module ObjC.AVFoundation.AVCaptionConversionWarning
  ( AVCaptionConversionWarning
  , IsAVCaptionConversionWarning(..)
  , init_
  , new
  , warningType
  , rangeOfCaptions
  , adjustment
  , initSelector
  , newSelector
  , warningTypeSelector
  , rangeOfCaptionsSelector
  , adjustmentSelector


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

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVCaptionConversionWarning avCaptionConversionWarning => avCaptionConversionWarning -> IO (Id AVCaptionConversionWarning)
init_ avCaptionConversionWarning  =
  sendMsg avCaptionConversionWarning (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVCaptionConversionWarning)
new  =
  do
    cls' <- getRequiredClass "AVCaptionConversionWarning"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | warningType
--
-- Indicates the type of warning provided by the receiver.
--
-- ObjC selector: @- warningType@
warningType :: IsAVCaptionConversionWarning avCaptionConversionWarning => avCaptionConversionWarning -> IO (Id NSString)
warningType avCaptionConversionWarning  =
  sendMsg avCaptionConversionWarning (mkSelector "warningType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | rangeOfCaptions
--
-- Indicates the range of captions in the validator's captions array for which the specified warning has been issued.
--
-- Only captions with the same start time and duration will be referenced. If captions with different start times and durations exhibit similar problems, a separate instance of AVCaptionConversionWarning will be used to indicate each problematic case. If the referenced captions have multiple problems, a separate instance of AVCaptionConversionWarning will be issued to indicate each problem.
--
-- ObjC selector: @- rangeOfCaptions@
rangeOfCaptions :: IsAVCaptionConversionWarning avCaptionConversionWarning => avCaptionConversionWarning -> IO NSRange
rangeOfCaptions avCaptionConversionWarning  =
  sendMsgStret avCaptionConversionWarning (mkSelector "rangeOfCaptions") retNSRange []

-- | adjustment
--
-- Indicates an adjustment to the indicated captions that can be applied in order to correct the problem.
--
-- If the value of adjustment is not nil and the conversion operation is performed without correcting the problem, the adjustment will be applied during conversion. If the value of adjustment is nil and the conversion operation is performed without correcting the problem, the indicated captions will be omitted from the output media data.
--
-- ObjC selector: @- adjustment@
adjustment :: IsAVCaptionConversionWarning avCaptionConversionWarning => avCaptionConversionWarning -> IO (Id AVCaptionConversionAdjustment)
adjustment avCaptionConversionWarning  =
  sendMsg avCaptionConversionWarning (mkSelector "adjustment") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @warningType@
warningTypeSelector :: Selector
warningTypeSelector = mkSelector "warningType"

-- | @Selector@ for @rangeOfCaptions@
rangeOfCaptionsSelector :: Selector
rangeOfCaptionsSelector = mkSelector "rangeOfCaptions"

-- | @Selector@ for @adjustment@
adjustmentSelector :: Selector
adjustmentSelector = mkSelector "adjustment"

