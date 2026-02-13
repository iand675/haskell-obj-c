{-# LANGUAGE DataKinds #-}
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
  , adjustmentSelector
  , initSelector
  , newSelector
  , rangeOfCaptionsSelector
  , warningTypeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVCaptionConversionWarning avCaptionConversionWarning => avCaptionConversionWarning -> IO (Id AVCaptionConversionWarning)
init_ avCaptionConversionWarning =
  sendOwnedMessage avCaptionConversionWarning initSelector

-- | @+ new@
new :: IO (Id AVCaptionConversionWarning)
new  =
  do
    cls' <- getRequiredClass "AVCaptionConversionWarning"
    sendOwnedClassMessage cls' newSelector

-- | warningType
--
-- Indicates the type of warning provided by the receiver.
--
-- ObjC selector: @- warningType@
warningType :: IsAVCaptionConversionWarning avCaptionConversionWarning => avCaptionConversionWarning -> IO (Id NSString)
warningType avCaptionConversionWarning =
  sendMessage avCaptionConversionWarning warningTypeSelector

-- | rangeOfCaptions
--
-- Indicates the range of captions in the validator's captions array for which the specified warning has been issued.
--
-- Only captions with the same start time and duration will be referenced. If captions with different start times and durations exhibit similar problems, a separate instance of AVCaptionConversionWarning will be used to indicate each problematic case. If the referenced captions have multiple problems, a separate instance of AVCaptionConversionWarning will be issued to indicate each problem.
--
-- ObjC selector: @- rangeOfCaptions@
rangeOfCaptions :: IsAVCaptionConversionWarning avCaptionConversionWarning => avCaptionConversionWarning -> IO NSRange
rangeOfCaptions avCaptionConversionWarning =
  sendMessage avCaptionConversionWarning rangeOfCaptionsSelector

-- | adjustment
--
-- Indicates an adjustment to the indicated captions that can be applied in order to correct the problem.
--
-- If the value of adjustment is not nil and the conversion operation is performed without correcting the problem, the adjustment will be applied during conversion. If the value of adjustment is nil and the conversion operation is performed without correcting the problem, the indicated captions will be omitted from the output media data.
--
-- ObjC selector: @- adjustment@
adjustment :: IsAVCaptionConversionWarning avCaptionConversionWarning => avCaptionConversionWarning -> IO (Id AVCaptionConversionAdjustment)
adjustment avCaptionConversionWarning =
  sendMessage avCaptionConversionWarning adjustmentSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVCaptionConversionWarning)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVCaptionConversionWarning)
newSelector = mkSelector "new"

-- | @Selector@ for @warningType@
warningTypeSelector :: Selector '[] (Id NSString)
warningTypeSelector = mkSelector "warningType"

-- | @Selector@ for @rangeOfCaptions@
rangeOfCaptionsSelector :: Selector '[] NSRange
rangeOfCaptionsSelector = mkSelector "rangeOfCaptions"

-- | @Selector@ for @adjustment@
adjustmentSelector :: Selector '[] (Id AVCaptionConversionAdjustment)
adjustmentSelector = mkSelector "adjustment"

