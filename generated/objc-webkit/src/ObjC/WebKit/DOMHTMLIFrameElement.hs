{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLIFrameElement@.
module ObjC.WebKit.DOMHTMLIFrameElement
  ( DOMHTMLIFrameElement
  , IsDOMHTMLIFrameElement(..)
  , align
  , setAlign
  , frameBorder
  , setFrameBorder
  , height
  , setHeight
  , longDesc
  , setLongDesc
  , marginHeight
  , setMarginHeight
  , marginWidth
  , setMarginWidth
  , name
  , setName
  , scrolling
  , setScrolling
  , src
  , setSrc
  , width
  , setWidth
  , contentDocument
  , contentWindow
  , contentFrame
  , alignSelector
  , setAlignSelector
  , frameBorderSelector
  , setFrameBorderSelector
  , heightSelector
  , setHeightSelector
  , longDescSelector
  , setLongDescSelector
  , marginHeightSelector
  , setMarginHeightSelector
  , marginWidthSelector
  , setMarginWidthSelector
  , nameSelector
  , setNameSelector
  , scrollingSelector
  , setScrollingSelector
  , srcSelector
  , setSrcSelector
  , widthSelector
  , setWidthSelector
  , contentDocumentSelector
  , contentWindowSelector
  , contentFrameSelector


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

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- align@
align :: IsDOMHTMLIFrameElement domhtmliFrameElement => domhtmliFrameElement -> IO (Id NSString)
align domhtmliFrameElement  =
  sendMsg domhtmliFrameElement (mkSelector "align") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAlign:@
setAlign :: (IsDOMHTMLIFrameElement domhtmliFrameElement, IsNSString value) => domhtmliFrameElement -> value -> IO ()
setAlign domhtmliFrameElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmliFrameElement (mkSelector "setAlign:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- frameBorder@
frameBorder :: IsDOMHTMLIFrameElement domhtmliFrameElement => domhtmliFrameElement -> IO (Id NSString)
frameBorder domhtmliFrameElement  =
  sendMsg domhtmliFrameElement (mkSelector "frameBorder") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFrameBorder:@
setFrameBorder :: (IsDOMHTMLIFrameElement domhtmliFrameElement, IsNSString value) => domhtmliFrameElement -> value -> IO ()
setFrameBorder domhtmliFrameElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmliFrameElement (mkSelector "setFrameBorder:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- height@
height :: IsDOMHTMLIFrameElement domhtmliFrameElement => domhtmliFrameElement -> IO (Id NSString)
height domhtmliFrameElement  =
  sendMsg domhtmliFrameElement (mkSelector "height") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setHeight:@
setHeight :: (IsDOMHTMLIFrameElement domhtmliFrameElement, IsNSString value) => domhtmliFrameElement -> value -> IO ()
setHeight domhtmliFrameElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmliFrameElement (mkSelector "setHeight:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- longDesc@
longDesc :: IsDOMHTMLIFrameElement domhtmliFrameElement => domhtmliFrameElement -> IO (Id NSString)
longDesc domhtmliFrameElement  =
  sendMsg domhtmliFrameElement (mkSelector "longDesc") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLongDesc:@
setLongDesc :: (IsDOMHTMLIFrameElement domhtmliFrameElement, IsNSString value) => domhtmliFrameElement -> value -> IO ()
setLongDesc domhtmliFrameElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmliFrameElement (mkSelector "setLongDesc:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- marginHeight@
marginHeight :: IsDOMHTMLIFrameElement domhtmliFrameElement => domhtmliFrameElement -> IO (Id NSString)
marginHeight domhtmliFrameElement  =
  sendMsg domhtmliFrameElement (mkSelector "marginHeight") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMarginHeight:@
setMarginHeight :: (IsDOMHTMLIFrameElement domhtmliFrameElement, IsNSString value) => domhtmliFrameElement -> value -> IO ()
setMarginHeight domhtmliFrameElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmliFrameElement (mkSelector "setMarginHeight:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- marginWidth@
marginWidth :: IsDOMHTMLIFrameElement domhtmliFrameElement => domhtmliFrameElement -> IO (Id NSString)
marginWidth domhtmliFrameElement  =
  sendMsg domhtmliFrameElement (mkSelector "marginWidth") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMarginWidth:@
setMarginWidth :: (IsDOMHTMLIFrameElement domhtmliFrameElement, IsNSString value) => domhtmliFrameElement -> value -> IO ()
setMarginWidth domhtmliFrameElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmliFrameElement (mkSelector "setMarginWidth:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- name@
name :: IsDOMHTMLIFrameElement domhtmliFrameElement => domhtmliFrameElement -> IO (Id NSString)
name domhtmliFrameElement  =
  sendMsg domhtmliFrameElement (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsDOMHTMLIFrameElement domhtmliFrameElement, IsNSString value) => domhtmliFrameElement -> value -> IO ()
setName domhtmliFrameElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmliFrameElement (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- scrolling@
scrolling :: IsDOMHTMLIFrameElement domhtmliFrameElement => domhtmliFrameElement -> IO (Id NSString)
scrolling domhtmliFrameElement  =
  sendMsg domhtmliFrameElement (mkSelector "scrolling") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setScrolling:@
setScrolling :: (IsDOMHTMLIFrameElement domhtmliFrameElement, IsNSString value) => domhtmliFrameElement -> value -> IO ()
setScrolling domhtmliFrameElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmliFrameElement (mkSelector "setScrolling:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- src@
src :: IsDOMHTMLIFrameElement domhtmliFrameElement => domhtmliFrameElement -> IO (Id NSString)
src domhtmliFrameElement  =
  sendMsg domhtmliFrameElement (mkSelector "src") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSrc:@
setSrc :: (IsDOMHTMLIFrameElement domhtmliFrameElement, IsNSString value) => domhtmliFrameElement -> value -> IO ()
setSrc domhtmliFrameElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmliFrameElement (mkSelector "setSrc:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- width@
width :: IsDOMHTMLIFrameElement domhtmliFrameElement => domhtmliFrameElement -> IO (Id NSString)
width domhtmliFrameElement  =
  sendMsg domhtmliFrameElement (mkSelector "width") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setWidth:@
setWidth :: (IsDOMHTMLIFrameElement domhtmliFrameElement, IsNSString value) => domhtmliFrameElement -> value -> IO ()
setWidth domhtmliFrameElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmliFrameElement (mkSelector "setWidth:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- contentDocument@
contentDocument :: IsDOMHTMLIFrameElement domhtmliFrameElement => domhtmliFrameElement -> IO (Id DOMDocument)
contentDocument domhtmliFrameElement  =
  sendMsg domhtmliFrameElement (mkSelector "contentDocument") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- contentWindow@
contentWindow :: IsDOMHTMLIFrameElement domhtmliFrameElement => domhtmliFrameElement -> IO (Id DOMAbstractView)
contentWindow domhtmliFrameElement  =
  sendMsg domhtmliFrameElement (mkSelector "contentWindow") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | contentFrame
--
-- Returns the content frame of the element.
--
-- ObjC selector: @- contentFrame@
contentFrame :: IsDOMHTMLIFrameElement domhtmliFrameElement => domhtmliFrameElement -> IO (Id WebFrame)
contentFrame domhtmliFrameElement  =
  sendMsg domhtmliFrameElement (mkSelector "contentFrame") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @align@
alignSelector :: Selector
alignSelector = mkSelector "align"

-- | @Selector@ for @setAlign:@
setAlignSelector :: Selector
setAlignSelector = mkSelector "setAlign:"

-- | @Selector@ for @frameBorder@
frameBorderSelector :: Selector
frameBorderSelector = mkSelector "frameBorder"

-- | @Selector@ for @setFrameBorder:@
setFrameBorderSelector :: Selector
setFrameBorderSelector = mkSelector "setFrameBorder:"

-- | @Selector@ for @height@
heightSelector :: Selector
heightSelector = mkSelector "height"

-- | @Selector@ for @setHeight:@
setHeightSelector :: Selector
setHeightSelector = mkSelector "setHeight:"

-- | @Selector@ for @longDesc@
longDescSelector :: Selector
longDescSelector = mkSelector "longDesc"

-- | @Selector@ for @setLongDesc:@
setLongDescSelector :: Selector
setLongDescSelector = mkSelector "setLongDesc:"

-- | @Selector@ for @marginHeight@
marginHeightSelector :: Selector
marginHeightSelector = mkSelector "marginHeight"

-- | @Selector@ for @setMarginHeight:@
setMarginHeightSelector :: Selector
setMarginHeightSelector = mkSelector "setMarginHeight:"

-- | @Selector@ for @marginWidth@
marginWidthSelector :: Selector
marginWidthSelector = mkSelector "marginWidth"

-- | @Selector@ for @setMarginWidth:@
setMarginWidthSelector :: Selector
setMarginWidthSelector = mkSelector "setMarginWidth:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @scrolling@
scrollingSelector :: Selector
scrollingSelector = mkSelector "scrolling"

-- | @Selector@ for @setScrolling:@
setScrollingSelector :: Selector
setScrollingSelector = mkSelector "setScrolling:"

-- | @Selector@ for @src@
srcSelector :: Selector
srcSelector = mkSelector "src"

-- | @Selector@ for @setSrc:@
setSrcSelector :: Selector
setSrcSelector = mkSelector "setSrc:"

-- | @Selector@ for @width@
widthSelector :: Selector
widthSelector = mkSelector "width"

-- | @Selector@ for @setWidth:@
setWidthSelector :: Selector
setWidthSelector = mkSelector "setWidth:"

-- | @Selector@ for @contentDocument@
contentDocumentSelector :: Selector
contentDocumentSelector = mkSelector "contentDocument"

-- | @Selector@ for @contentWindow@
contentWindowSelector :: Selector
contentWindowSelector = mkSelector "contentWindow"

-- | @Selector@ for @contentFrame@
contentFrameSelector :: Selector
contentFrameSelector = mkSelector "contentFrame"

