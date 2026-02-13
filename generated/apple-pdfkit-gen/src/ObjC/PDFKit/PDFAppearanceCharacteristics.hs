{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , appearanceCharacteristicsKeyValuesSelector
  , backgroundColorSelector
  , borderColorSelector
  , captionSelector
  , controlTypeSelector
  , downCaptionSelector
  , rolloverCaptionSelector
  , rotationSelector
  , setBackgroundColorSelector
  , setBorderColorSelector
  , setCaptionSelector
  , setControlTypeSelector
  , setDownCaptionSelector
  , setRolloverCaptionSelector
  , setRotationSelector

  -- * Enum types
  , PDFWidgetControlType(PDFWidgetControlType)
  , pattern KPDFWidgetUnknownControl
  , pattern KPDFWidgetPushButtonControl
  , pattern KPDFWidgetRadioButtonControl
  , pattern KPDFWidgetCheckBoxControl

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PDFKit.Internal.Classes
import ObjC.PDFKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- controlType@
controlType :: IsPDFAppearanceCharacteristics pdfAppearanceCharacteristics => pdfAppearanceCharacteristics -> IO PDFWidgetControlType
controlType pdfAppearanceCharacteristics =
  sendMessage pdfAppearanceCharacteristics controlTypeSelector

-- | @- setControlType:@
setControlType :: IsPDFAppearanceCharacteristics pdfAppearanceCharacteristics => pdfAppearanceCharacteristics -> PDFWidgetControlType -> IO ()
setControlType pdfAppearanceCharacteristics value =
  sendMessage pdfAppearanceCharacteristics setControlTypeSelector value

-- | @- backgroundColor@
backgroundColor :: IsPDFAppearanceCharacteristics pdfAppearanceCharacteristics => pdfAppearanceCharacteristics -> IO (Id NSColor)
backgroundColor pdfAppearanceCharacteristics =
  sendMessage pdfAppearanceCharacteristics backgroundColorSelector

-- | @- setBackgroundColor:@
setBackgroundColor :: (IsPDFAppearanceCharacteristics pdfAppearanceCharacteristics, IsNSColor value) => pdfAppearanceCharacteristics -> value -> IO ()
setBackgroundColor pdfAppearanceCharacteristics value =
  sendMessage pdfAppearanceCharacteristics setBackgroundColorSelector (toNSColor value)

-- | @- borderColor@
borderColor :: IsPDFAppearanceCharacteristics pdfAppearanceCharacteristics => pdfAppearanceCharacteristics -> IO (Id NSColor)
borderColor pdfAppearanceCharacteristics =
  sendMessage pdfAppearanceCharacteristics borderColorSelector

-- | @- setBorderColor:@
setBorderColor :: (IsPDFAppearanceCharacteristics pdfAppearanceCharacteristics, IsNSColor value) => pdfAppearanceCharacteristics -> value -> IO ()
setBorderColor pdfAppearanceCharacteristics value =
  sendMessage pdfAppearanceCharacteristics setBorderColorSelector (toNSColor value)

-- | @- rotation@
rotation :: IsPDFAppearanceCharacteristics pdfAppearanceCharacteristics => pdfAppearanceCharacteristics -> IO CLong
rotation pdfAppearanceCharacteristics =
  sendMessage pdfAppearanceCharacteristics rotationSelector

-- | @- setRotation:@
setRotation :: IsPDFAppearanceCharacteristics pdfAppearanceCharacteristics => pdfAppearanceCharacteristics -> CLong -> IO ()
setRotation pdfAppearanceCharacteristics value =
  sendMessage pdfAppearanceCharacteristics setRotationSelector value

-- | @- caption@
caption :: IsPDFAppearanceCharacteristics pdfAppearanceCharacteristics => pdfAppearanceCharacteristics -> IO (Id NSString)
caption pdfAppearanceCharacteristics =
  sendMessage pdfAppearanceCharacteristics captionSelector

-- | @- setCaption:@
setCaption :: (IsPDFAppearanceCharacteristics pdfAppearanceCharacteristics, IsNSString value) => pdfAppearanceCharacteristics -> value -> IO ()
setCaption pdfAppearanceCharacteristics value =
  sendMessage pdfAppearanceCharacteristics setCaptionSelector (toNSString value)

-- | @- rolloverCaption@
rolloverCaption :: IsPDFAppearanceCharacteristics pdfAppearanceCharacteristics => pdfAppearanceCharacteristics -> IO (Id NSString)
rolloverCaption pdfAppearanceCharacteristics =
  sendMessage pdfAppearanceCharacteristics rolloverCaptionSelector

-- | @- setRolloverCaption:@
setRolloverCaption :: (IsPDFAppearanceCharacteristics pdfAppearanceCharacteristics, IsNSString value) => pdfAppearanceCharacteristics -> value -> IO ()
setRolloverCaption pdfAppearanceCharacteristics value =
  sendMessage pdfAppearanceCharacteristics setRolloverCaptionSelector (toNSString value)

-- | @- downCaption@
downCaption :: IsPDFAppearanceCharacteristics pdfAppearanceCharacteristics => pdfAppearanceCharacteristics -> IO (Id NSString)
downCaption pdfAppearanceCharacteristics =
  sendMessage pdfAppearanceCharacteristics downCaptionSelector

-- | @- setDownCaption:@
setDownCaption :: (IsPDFAppearanceCharacteristics pdfAppearanceCharacteristics, IsNSString value) => pdfAppearanceCharacteristics -> value -> IO ()
setDownCaption pdfAppearanceCharacteristics value =
  sendMessage pdfAppearanceCharacteristics setDownCaptionSelector (toNSString value)

-- | @- appearanceCharacteristicsKeyValues@
appearanceCharacteristicsKeyValues :: IsPDFAppearanceCharacteristics pdfAppearanceCharacteristics => pdfAppearanceCharacteristics -> IO (Id NSDictionary)
appearanceCharacteristicsKeyValues pdfAppearanceCharacteristics =
  sendMessage pdfAppearanceCharacteristics appearanceCharacteristicsKeyValuesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @controlType@
controlTypeSelector :: Selector '[] PDFWidgetControlType
controlTypeSelector = mkSelector "controlType"

-- | @Selector@ for @setControlType:@
setControlTypeSelector :: Selector '[PDFWidgetControlType] ()
setControlTypeSelector = mkSelector "setControlType:"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector '[] (Id NSColor)
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector '[Id NSColor] ()
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

-- | @Selector@ for @borderColor@
borderColorSelector :: Selector '[] (Id NSColor)
borderColorSelector = mkSelector "borderColor"

-- | @Selector@ for @setBorderColor:@
setBorderColorSelector :: Selector '[Id NSColor] ()
setBorderColorSelector = mkSelector "setBorderColor:"

-- | @Selector@ for @rotation@
rotationSelector :: Selector '[] CLong
rotationSelector = mkSelector "rotation"

-- | @Selector@ for @setRotation:@
setRotationSelector :: Selector '[CLong] ()
setRotationSelector = mkSelector "setRotation:"

-- | @Selector@ for @caption@
captionSelector :: Selector '[] (Id NSString)
captionSelector = mkSelector "caption"

-- | @Selector@ for @setCaption:@
setCaptionSelector :: Selector '[Id NSString] ()
setCaptionSelector = mkSelector "setCaption:"

-- | @Selector@ for @rolloverCaption@
rolloverCaptionSelector :: Selector '[] (Id NSString)
rolloverCaptionSelector = mkSelector "rolloverCaption"

-- | @Selector@ for @setRolloverCaption:@
setRolloverCaptionSelector :: Selector '[Id NSString] ()
setRolloverCaptionSelector = mkSelector "setRolloverCaption:"

-- | @Selector@ for @downCaption@
downCaptionSelector :: Selector '[] (Id NSString)
downCaptionSelector = mkSelector "downCaption"

-- | @Selector@ for @setDownCaption:@
setDownCaptionSelector :: Selector '[Id NSString] ()
setDownCaptionSelector = mkSelector "setDownCaption:"

-- | @Selector@ for @appearanceCharacteristicsKeyValues@
appearanceCharacteristicsKeyValuesSelector :: Selector '[] (Id NSDictionary)
appearanceCharacteristicsKeyValuesSelector = mkSelector "appearanceCharacteristicsKeyValues"

