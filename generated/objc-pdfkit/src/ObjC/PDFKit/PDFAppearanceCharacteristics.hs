{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PDFAppearanceCharacteristics@.
module ObjC.PDFKit.PDFAppearanceCharacteristics
  ( PDFAppearanceCharacteristics
  , IsPDFAppearanceCharacteristics(..)
  , controlType
  , setControlType
  , backgroundColor
  , setBackgroundColor
  , borderColor
  , setBorderColor
  , rotation
  , setRotation
  , caption
  , setCaption
  , rolloverCaption
  , setRolloverCaption
  , downCaption
  , setDownCaption
  , appearanceCharacteristicsKeyValues
  , controlTypeSelector
  , setControlTypeSelector
  , backgroundColorSelector
  , setBackgroundColorSelector
  , borderColorSelector
  , setBorderColorSelector
  , rotationSelector
  , setRotationSelector
  , captionSelector
  , setCaptionSelector
  , rolloverCaptionSelector
  , setRolloverCaptionSelector
  , downCaptionSelector
  , setDownCaptionSelector
  , appearanceCharacteristicsKeyValuesSelector

  -- * Enum types
  , PDFWidgetControlType(PDFWidgetControlType)
  , pattern KPDFWidgetUnknownControl
  , pattern KPDFWidgetPushButtonControl
  , pattern KPDFWidgetRadioButtonControl
  , pattern KPDFWidgetCheckBoxControl

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

import ObjC.PDFKit.Internal.Classes
import ObjC.PDFKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- controlType@
controlType :: IsPDFAppearanceCharacteristics pdfAppearanceCharacteristics => pdfAppearanceCharacteristics -> IO PDFWidgetControlType
controlType pdfAppearanceCharacteristics  =
  fmap (coerce :: CLong -> PDFWidgetControlType) $ sendMsg pdfAppearanceCharacteristics (mkSelector "controlType") retCLong []

-- | @- setControlType:@
setControlType :: IsPDFAppearanceCharacteristics pdfAppearanceCharacteristics => pdfAppearanceCharacteristics -> PDFWidgetControlType -> IO ()
setControlType pdfAppearanceCharacteristics  value =
  sendMsg pdfAppearanceCharacteristics (mkSelector "setControlType:") retVoid [argCLong (coerce value)]

-- | @- backgroundColor@
backgroundColor :: IsPDFAppearanceCharacteristics pdfAppearanceCharacteristics => pdfAppearanceCharacteristics -> IO (Id NSColor)
backgroundColor pdfAppearanceCharacteristics  =
  sendMsg pdfAppearanceCharacteristics (mkSelector "backgroundColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBackgroundColor:@
setBackgroundColor :: (IsPDFAppearanceCharacteristics pdfAppearanceCharacteristics, IsNSColor value) => pdfAppearanceCharacteristics -> value -> IO ()
setBackgroundColor pdfAppearanceCharacteristics  value =
withObjCPtr value $ \raw_value ->
    sendMsg pdfAppearanceCharacteristics (mkSelector "setBackgroundColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- borderColor@
borderColor :: IsPDFAppearanceCharacteristics pdfAppearanceCharacteristics => pdfAppearanceCharacteristics -> IO (Id NSColor)
borderColor pdfAppearanceCharacteristics  =
  sendMsg pdfAppearanceCharacteristics (mkSelector "borderColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBorderColor:@
setBorderColor :: (IsPDFAppearanceCharacteristics pdfAppearanceCharacteristics, IsNSColor value) => pdfAppearanceCharacteristics -> value -> IO ()
setBorderColor pdfAppearanceCharacteristics  value =
withObjCPtr value $ \raw_value ->
    sendMsg pdfAppearanceCharacteristics (mkSelector "setBorderColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- rotation@
rotation :: IsPDFAppearanceCharacteristics pdfAppearanceCharacteristics => pdfAppearanceCharacteristics -> IO CLong
rotation pdfAppearanceCharacteristics  =
  sendMsg pdfAppearanceCharacteristics (mkSelector "rotation") retCLong []

-- | @- setRotation:@
setRotation :: IsPDFAppearanceCharacteristics pdfAppearanceCharacteristics => pdfAppearanceCharacteristics -> CLong -> IO ()
setRotation pdfAppearanceCharacteristics  value =
  sendMsg pdfAppearanceCharacteristics (mkSelector "setRotation:") retVoid [argCLong (fromIntegral value)]

-- | @- caption@
caption :: IsPDFAppearanceCharacteristics pdfAppearanceCharacteristics => pdfAppearanceCharacteristics -> IO (Id NSString)
caption pdfAppearanceCharacteristics  =
  sendMsg pdfAppearanceCharacteristics (mkSelector "caption") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCaption:@
setCaption :: (IsPDFAppearanceCharacteristics pdfAppearanceCharacteristics, IsNSString value) => pdfAppearanceCharacteristics -> value -> IO ()
setCaption pdfAppearanceCharacteristics  value =
withObjCPtr value $ \raw_value ->
    sendMsg pdfAppearanceCharacteristics (mkSelector "setCaption:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- rolloverCaption@
rolloverCaption :: IsPDFAppearanceCharacteristics pdfAppearanceCharacteristics => pdfAppearanceCharacteristics -> IO (Id NSString)
rolloverCaption pdfAppearanceCharacteristics  =
  sendMsg pdfAppearanceCharacteristics (mkSelector "rolloverCaption") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRolloverCaption:@
setRolloverCaption :: (IsPDFAppearanceCharacteristics pdfAppearanceCharacteristics, IsNSString value) => pdfAppearanceCharacteristics -> value -> IO ()
setRolloverCaption pdfAppearanceCharacteristics  value =
withObjCPtr value $ \raw_value ->
    sendMsg pdfAppearanceCharacteristics (mkSelector "setRolloverCaption:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- downCaption@
downCaption :: IsPDFAppearanceCharacteristics pdfAppearanceCharacteristics => pdfAppearanceCharacteristics -> IO (Id NSString)
downCaption pdfAppearanceCharacteristics  =
  sendMsg pdfAppearanceCharacteristics (mkSelector "downCaption") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDownCaption:@
setDownCaption :: (IsPDFAppearanceCharacteristics pdfAppearanceCharacteristics, IsNSString value) => pdfAppearanceCharacteristics -> value -> IO ()
setDownCaption pdfAppearanceCharacteristics  value =
withObjCPtr value $ \raw_value ->
    sendMsg pdfAppearanceCharacteristics (mkSelector "setDownCaption:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- appearanceCharacteristicsKeyValues@
appearanceCharacteristicsKeyValues :: IsPDFAppearanceCharacteristics pdfAppearanceCharacteristics => pdfAppearanceCharacteristics -> IO (Id NSDictionary)
appearanceCharacteristicsKeyValues pdfAppearanceCharacteristics  =
  sendMsg pdfAppearanceCharacteristics (mkSelector "appearanceCharacteristicsKeyValues") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @controlType@
controlTypeSelector :: Selector
controlTypeSelector = mkSelector "controlType"

-- | @Selector@ for @setControlType:@
setControlTypeSelector :: Selector
setControlTypeSelector = mkSelector "setControlType:"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

-- | @Selector@ for @borderColor@
borderColorSelector :: Selector
borderColorSelector = mkSelector "borderColor"

-- | @Selector@ for @setBorderColor:@
setBorderColorSelector :: Selector
setBorderColorSelector = mkSelector "setBorderColor:"

-- | @Selector@ for @rotation@
rotationSelector :: Selector
rotationSelector = mkSelector "rotation"

-- | @Selector@ for @setRotation:@
setRotationSelector :: Selector
setRotationSelector = mkSelector "setRotation:"

-- | @Selector@ for @caption@
captionSelector :: Selector
captionSelector = mkSelector "caption"

-- | @Selector@ for @setCaption:@
setCaptionSelector :: Selector
setCaptionSelector = mkSelector "setCaption:"

-- | @Selector@ for @rolloverCaption@
rolloverCaptionSelector :: Selector
rolloverCaptionSelector = mkSelector "rolloverCaption"

-- | @Selector@ for @setRolloverCaption:@
setRolloverCaptionSelector :: Selector
setRolloverCaptionSelector = mkSelector "setRolloverCaption:"

-- | @Selector@ for @downCaption@
downCaptionSelector :: Selector
downCaptionSelector = mkSelector "downCaption"

-- | @Selector@ for @setDownCaption:@
setDownCaptionSelector :: Selector
setDownCaptionSelector = mkSelector "setDownCaption:"

-- | @Selector@ for @appearanceCharacteristicsKeyValues@
appearanceCharacteristicsKeyValuesSelector :: Selector
appearanceCharacteristicsKeyValuesSelector = mkSelector "appearanceCharacteristicsKeyValues"

