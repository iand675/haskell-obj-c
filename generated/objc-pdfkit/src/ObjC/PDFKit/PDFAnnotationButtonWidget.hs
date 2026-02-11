{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PDFAnnotationButtonWidget@.
module ObjC.PDFKit.PDFAnnotationButtonWidget
  ( PDFAnnotationButtonWidget
  , IsPDFAnnotationButtonWidget(..)
  , controlType
  , setControlType
  , state
  , setState
  , backgroundColor
  , setBackgroundColor
  , allowsToggleToOff
  , setAllowsToggleToOff
  , font
  , setFont
  , fontColor
  , setFontColor
  , caption
  , setCaption
  , fieldName
  , setFieldName
  , onStateValue
  , setOnStateValue
  , controlTypeSelector
  , setControlTypeSelector
  , stateSelector
  , setStateSelector
  , backgroundColorSelector
  , setBackgroundColorSelector
  , allowsToggleToOffSelector
  , setAllowsToggleToOffSelector
  , fontSelector
  , setFontSelector
  , fontColorSelector
  , setFontColorSelector
  , captionSelector
  , setCaptionSelector
  , fieldNameSelector
  , setFieldNameSelector
  , onStateValueSelector
  , setOnStateValueSelector

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
controlType :: IsPDFAnnotationButtonWidget pdfAnnotationButtonWidget => pdfAnnotationButtonWidget -> IO PDFWidgetControlType
controlType pdfAnnotationButtonWidget  =
  fmap (coerce :: CLong -> PDFWidgetControlType) $ sendMsg pdfAnnotationButtonWidget (mkSelector "controlType") retCLong []

-- | @- setControlType:@
setControlType :: IsPDFAnnotationButtonWidget pdfAnnotationButtonWidget => pdfAnnotationButtonWidget -> PDFWidgetControlType -> IO ()
setControlType pdfAnnotationButtonWidget  type_ =
  sendMsg pdfAnnotationButtonWidget (mkSelector "setControlType:") retVoid [argCLong (coerce type_)]

-- | @- state@
state :: IsPDFAnnotationButtonWidget pdfAnnotationButtonWidget => pdfAnnotationButtonWidget -> IO CLong
state pdfAnnotationButtonWidget  =
  sendMsg pdfAnnotationButtonWidget (mkSelector "state") retCLong []

-- | @- setState:@
setState :: IsPDFAnnotationButtonWidget pdfAnnotationButtonWidget => pdfAnnotationButtonWidget -> CLong -> IO ()
setState pdfAnnotationButtonWidget  value =
  sendMsg pdfAnnotationButtonWidget (mkSelector "setState:") retVoid [argCLong (fromIntegral value)]

-- | @- backgroundColor@
backgroundColor :: IsPDFAnnotationButtonWidget pdfAnnotationButtonWidget => pdfAnnotationButtonWidget -> IO (Id NSColor)
backgroundColor pdfAnnotationButtonWidget  =
  sendMsg pdfAnnotationButtonWidget (mkSelector "backgroundColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBackgroundColor:@
setBackgroundColor :: (IsPDFAnnotationButtonWidget pdfAnnotationButtonWidget, IsNSColor color) => pdfAnnotationButtonWidget -> color -> IO ()
setBackgroundColor pdfAnnotationButtonWidget  color =
withObjCPtr color $ \raw_color ->
    sendMsg pdfAnnotationButtonWidget (mkSelector "setBackgroundColor:") retVoid [argPtr (castPtr raw_color :: Ptr ())]

-- | @- allowsToggleToOff@
allowsToggleToOff :: IsPDFAnnotationButtonWidget pdfAnnotationButtonWidget => pdfAnnotationButtonWidget -> IO Bool
allowsToggleToOff pdfAnnotationButtonWidget  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg pdfAnnotationButtonWidget (mkSelector "allowsToggleToOff") retCULong []

-- | @- setAllowsToggleToOff:@
setAllowsToggleToOff :: IsPDFAnnotationButtonWidget pdfAnnotationButtonWidget => pdfAnnotationButtonWidget -> Bool -> IO ()
setAllowsToggleToOff pdfAnnotationButtonWidget  allowOff =
  sendMsg pdfAnnotationButtonWidget (mkSelector "setAllowsToggleToOff:") retVoid [argCULong (if allowOff then 1 else 0)]

-- | @- font@
font :: IsPDFAnnotationButtonWidget pdfAnnotationButtonWidget => pdfAnnotationButtonWidget -> IO (Id NSFont)
font pdfAnnotationButtonWidget  =
  sendMsg pdfAnnotationButtonWidget (mkSelector "font") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFont:@
setFont :: (IsPDFAnnotationButtonWidget pdfAnnotationButtonWidget, IsNSFont font) => pdfAnnotationButtonWidget -> font -> IO ()
setFont pdfAnnotationButtonWidget  font =
withObjCPtr font $ \raw_font ->
    sendMsg pdfAnnotationButtonWidget (mkSelector "setFont:") retVoid [argPtr (castPtr raw_font :: Ptr ())]

-- | @- fontColor@
fontColor :: IsPDFAnnotationButtonWidget pdfAnnotationButtonWidget => pdfAnnotationButtonWidget -> IO (Id NSColor)
fontColor pdfAnnotationButtonWidget  =
  sendMsg pdfAnnotationButtonWidget (mkSelector "fontColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFontColor:@
setFontColor :: (IsPDFAnnotationButtonWidget pdfAnnotationButtonWidget, IsNSColor color) => pdfAnnotationButtonWidget -> color -> IO ()
setFontColor pdfAnnotationButtonWidget  color =
withObjCPtr color $ \raw_color ->
    sendMsg pdfAnnotationButtonWidget (mkSelector "setFontColor:") retVoid [argPtr (castPtr raw_color :: Ptr ())]

-- | @- caption@
caption :: IsPDFAnnotationButtonWidget pdfAnnotationButtonWidget => pdfAnnotationButtonWidget -> IO (Id NSString)
caption pdfAnnotationButtonWidget  =
  sendMsg pdfAnnotationButtonWidget (mkSelector "caption") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCaption:@
setCaption :: (IsPDFAnnotationButtonWidget pdfAnnotationButtonWidget, IsNSString name) => pdfAnnotationButtonWidget -> name -> IO ()
setCaption pdfAnnotationButtonWidget  name =
withObjCPtr name $ \raw_name ->
    sendMsg pdfAnnotationButtonWidget (mkSelector "setCaption:") retVoid [argPtr (castPtr raw_name :: Ptr ())]

-- | @- fieldName@
fieldName :: IsPDFAnnotationButtonWidget pdfAnnotationButtonWidget => pdfAnnotationButtonWidget -> IO (Id NSString)
fieldName pdfAnnotationButtonWidget  =
  sendMsg pdfAnnotationButtonWidget (mkSelector "fieldName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFieldName:@
setFieldName :: (IsPDFAnnotationButtonWidget pdfAnnotationButtonWidget, IsNSString name) => pdfAnnotationButtonWidget -> name -> IO ()
setFieldName pdfAnnotationButtonWidget  name =
withObjCPtr name $ \raw_name ->
    sendMsg pdfAnnotationButtonWidget (mkSelector "setFieldName:") retVoid [argPtr (castPtr raw_name :: Ptr ())]

-- | @- onStateValue@
onStateValue :: IsPDFAnnotationButtonWidget pdfAnnotationButtonWidget => pdfAnnotationButtonWidget -> IO (Id NSString)
onStateValue pdfAnnotationButtonWidget  =
  sendMsg pdfAnnotationButtonWidget (mkSelector "onStateValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOnStateValue:@
setOnStateValue :: (IsPDFAnnotationButtonWidget pdfAnnotationButtonWidget, IsNSString name) => pdfAnnotationButtonWidget -> name -> IO ()
setOnStateValue pdfAnnotationButtonWidget  name =
withObjCPtr name $ \raw_name ->
    sendMsg pdfAnnotationButtonWidget (mkSelector "setOnStateValue:") retVoid [argPtr (castPtr raw_name :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @controlType@
controlTypeSelector :: Selector
controlTypeSelector = mkSelector "controlType"

-- | @Selector@ for @setControlType:@
setControlTypeSelector :: Selector
setControlTypeSelector = mkSelector "setControlType:"

-- | @Selector@ for @state@
stateSelector :: Selector
stateSelector = mkSelector "state"

-- | @Selector@ for @setState:@
setStateSelector :: Selector
setStateSelector = mkSelector "setState:"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

-- | @Selector@ for @allowsToggleToOff@
allowsToggleToOffSelector :: Selector
allowsToggleToOffSelector = mkSelector "allowsToggleToOff"

-- | @Selector@ for @setAllowsToggleToOff:@
setAllowsToggleToOffSelector :: Selector
setAllowsToggleToOffSelector = mkSelector "setAllowsToggleToOff:"

-- | @Selector@ for @font@
fontSelector :: Selector
fontSelector = mkSelector "font"

-- | @Selector@ for @setFont:@
setFontSelector :: Selector
setFontSelector = mkSelector "setFont:"

-- | @Selector@ for @fontColor@
fontColorSelector :: Selector
fontColorSelector = mkSelector "fontColor"

-- | @Selector@ for @setFontColor:@
setFontColorSelector :: Selector
setFontColorSelector = mkSelector "setFontColor:"

-- | @Selector@ for @caption@
captionSelector :: Selector
captionSelector = mkSelector "caption"

-- | @Selector@ for @setCaption:@
setCaptionSelector :: Selector
setCaptionSelector = mkSelector "setCaption:"

-- | @Selector@ for @fieldName@
fieldNameSelector :: Selector
fieldNameSelector = mkSelector "fieldName"

-- | @Selector@ for @setFieldName:@
setFieldNameSelector :: Selector
setFieldNameSelector = mkSelector "setFieldName:"

-- | @Selector@ for @onStateValue@
onStateValueSelector :: Selector
onStateValueSelector = mkSelector "onStateValue"

-- | @Selector@ for @setOnStateValue:@
setOnStateValueSelector :: Selector
setOnStateValueSelector = mkSelector "setOnStateValue:"

