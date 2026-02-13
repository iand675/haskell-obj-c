{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , allowsToggleToOffSelector
  , backgroundColorSelector
  , captionSelector
  , controlTypeSelector
  , fieldNameSelector
  , fontColorSelector
  , fontSelector
  , onStateValueSelector
  , setAllowsToggleToOffSelector
  , setBackgroundColorSelector
  , setCaptionSelector
  , setControlTypeSelector
  , setFieldNameSelector
  , setFontColorSelector
  , setFontSelector
  , setOnStateValueSelector
  , setStateSelector
  , stateSelector

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
controlType :: IsPDFAnnotationButtonWidget pdfAnnotationButtonWidget => pdfAnnotationButtonWidget -> IO PDFWidgetControlType
controlType pdfAnnotationButtonWidget =
  sendMessage pdfAnnotationButtonWidget controlTypeSelector

-- | @- setControlType:@
setControlType :: IsPDFAnnotationButtonWidget pdfAnnotationButtonWidget => pdfAnnotationButtonWidget -> PDFWidgetControlType -> IO ()
setControlType pdfAnnotationButtonWidget type_ =
  sendMessage pdfAnnotationButtonWidget setControlTypeSelector type_

-- | @- state@
state :: IsPDFAnnotationButtonWidget pdfAnnotationButtonWidget => pdfAnnotationButtonWidget -> IO CLong
state pdfAnnotationButtonWidget =
  sendMessage pdfAnnotationButtonWidget stateSelector

-- | @- setState:@
setState :: IsPDFAnnotationButtonWidget pdfAnnotationButtonWidget => pdfAnnotationButtonWidget -> CLong -> IO ()
setState pdfAnnotationButtonWidget value =
  sendMessage pdfAnnotationButtonWidget setStateSelector value

-- | @- backgroundColor@
backgroundColor :: IsPDFAnnotationButtonWidget pdfAnnotationButtonWidget => pdfAnnotationButtonWidget -> IO (Id NSColor)
backgroundColor pdfAnnotationButtonWidget =
  sendMessage pdfAnnotationButtonWidget backgroundColorSelector

-- | @- setBackgroundColor:@
setBackgroundColor :: (IsPDFAnnotationButtonWidget pdfAnnotationButtonWidget, IsNSColor color) => pdfAnnotationButtonWidget -> color -> IO ()
setBackgroundColor pdfAnnotationButtonWidget color =
  sendMessage pdfAnnotationButtonWidget setBackgroundColorSelector (toNSColor color)

-- | @- allowsToggleToOff@
allowsToggleToOff :: IsPDFAnnotationButtonWidget pdfAnnotationButtonWidget => pdfAnnotationButtonWidget -> IO Bool
allowsToggleToOff pdfAnnotationButtonWidget =
  sendMessage pdfAnnotationButtonWidget allowsToggleToOffSelector

-- | @- setAllowsToggleToOff:@
setAllowsToggleToOff :: IsPDFAnnotationButtonWidget pdfAnnotationButtonWidget => pdfAnnotationButtonWidget -> Bool -> IO ()
setAllowsToggleToOff pdfAnnotationButtonWidget allowOff =
  sendMessage pdfAnnotationButtonWidget setAllowsToggleToOffSelector allowOff

-- | @- font@
font :: IsPDFAnnotationButtonWidget pdfAnnotationButtonWidget => pdfAnnotationButtonWidget -> IO (Id NSFont)
font pdfAnnotationButtonWidget =
  sendMessage pdfAnnotationButtonWidget fontSelector

-- | @- setFont:@
setFont :: (IsPDFAnnotationButtonWidget pdfAnnotationButtonWidget, IsNSFont font) => pdfAnnotationButtonWidget -> font -> IO ()
setFont pdfAnnotationButtonWidget font =
  sendMessage pdfAnnotationButtonWidget setFontSelector (toNSFont font)

-- | @- fontColor@
fontColor :: IsPDFAnnotationButtonWidget pdfAnnotationButtonWidget => pdfAnnotationButtonWidget -> IO (Id NSColor)
fontColor pdfAnnotationButtonWidget =
  sendMessage pdfAnnotationButtonWidget fontColorSelector

-- | @- setFontColor:@
setFontColor :: (IsPDFAnnotationButtonWidget pdfAnnotationButtonWidget, IsNSColor color) => pdfAnnotationButtonWidget -> color -> IO ()
setFontColor pdfAnnotationButtonWidget color =
  sendMessage pdfAnnotationButtonWidget setFontColorSelector (toNSColor color)

-- | @- caption@
caption :: IsPDFAnnotationButtonWidget pdfAnnotationButtonWidget => pdfAnnotationButtonWidget -> IO (Id NSString)
caption pdfAnnotationButtonWidget =
  sendMessage pdfAnnotationButtonWidget captionSelector

-- | @- setCaption:@
setCaption :: (IsPDFAnnotationButtonWidget pdfAnnotationButtonWidget, IsNSString name) => pdfAnnotationButtonWidget -> name -> IO ()
setCaption pdfAnnotationButtonWidget name =
  sendMessage pdfAnnotationButtonWidget setCaptionSelector (toNSString name)

-- | @- fieldName@
fieldName :: IsPDFAnnotationButtonWidget pdfAnnotationButtonWidget => pdfAnnotationButtonWidget -> IO (Id NSString)
fieldName pdfAnnotationButtonWidget =
  sendMessage pdfAnnotationButtonWidget fieldNameSelector

-- | @- setFieldName:@
setFieldName :: (IsPDFAnnotationButtonWidget pdfAnnotationButtonWidget, IsNSString name) => pdfAnnotationButtonWidget -> name -> IO ()
setFieldName pdfAnnotationButtonWidget name =
  sendMessage pdfAnnotationButtonWidget setFieldNameSelector (toNSString name)

-- | @- onStateValue@
onStateValue :: IsPDFAnnotationButtonWidget pdfAnnotationButtonWidget => pdfAnnotationButtonWidget -> IO (Id NSString)
onStateValue pdfAnnotationButtonWidget =
  sendMessage pdfAnnotationButtonWidget onStateValueSelector

-- | @- setOnStateValue:@
setOnStateValue :: (IsPDFAnnotationButtonWidget pdfAnnotationButtonWidget, IsNSString name) => pdfAnnotationButtonWidget -> name -> IO ()
setOnStateValue pdfAnnotationButtonWidget name =
  sendMessage pdfAnnotationButtonWidget setOnStateValueSelector (toNSString name)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @controlType@
controlTypeSelector :: Selector '[] PDFWidgetControlType
controlTypeSelector = mkSelector "controlType"

-- | @Selector@ for @setControlType:@
setControlTypeSelector :: Selector '[PDFWidgetControlType] ()
setControlTypeSelector = mkSelector "setControlType:"

-- | @Selector@ for @state@
stateSelector :: Selector '[] CLong
stateSelector = mkSelector "state"

-- | @Selector@ for @setState:@
setStateSelector :: Selector '[CLong] ()
setStateSelector = mkSelector "setState:"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector '[] (Id NSColor)
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector '[Id NSColor] ()
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

-- | @Selector@ for @allowsToggleToOff@
allowsToggleToOffSelector :: Selector '[] Bool
allowsToggleToOffSelector = mkSelector "allowsToggleToOff"

-- | @Selector@ for @setAllowsToggleToOff:@
setAllowsToggleToOffSelector :: Selector '[Bool] ()
setAllowsToggleToOffSelector = mkSelector "setAllowsToggleToOff:"

-- | @Selector@ for @font@
fontSelector :: Selector '[] (Id NSFont)
fontSelector = mkSelector "font"

-- | @Selector@ for @setFont:@
setFontSelector :: Selector '[Id NSFont] ()
setFontSelector = mkSelector "setFont:"

-- | @Selector@ for @fontColor@
fontColorSelector :: Selector '[] (Id NSColor)
fontColorSelector = mkSelector "fontColor"

-- | @Selector@ for @setFontColor:@
setFontColorSelector :: Selector '[Id NSColor] ()
setFontColorSelector = mkSelector "setFontColor:"

-- | @Selector@ for @caption@
captionSelector :: Selector '[] (Id NSString)
captionSelector = mkSelector "caption"

-- | @Selector@ for @setCaption:@
setCaptionSelector :: Selector '[Id NSString] ()
setCaptionSelector = mkSelector "setCaption:"

-- | @Selector@ for @fieldName@
fieldNameSelector :: Selector '[] (Id NSString)
fieldNameSelector = mkSelector "fieldName"

-- | @Selector@ for @setFieldName:@
setFieldNameSelector :: Selector '[Id NSString] ()
setFieldNameSelector = mkSelector "setFieldName:"

-- | @Selector@ for @onStateValue@
onStateValueSelector :: Selector '[] (Id NSString)
onStateValueSelector = mkSelector "onStateValue"

-- | @Selector@ for @setOnStateValue:@
setOnStateValueSelector :: Selector '[Id NSString] ()
setOnStateValueSelector = mkSelector "setOnStateValue:"

