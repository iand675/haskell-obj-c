{-# LANGUAGE DataKinds #-}
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
  , contentDocumentSelector
  , contentFrameSelector
  , contentWindowSelector
  , frameBorderSelector
  , heightSelector
  , longDescSelector
  , marginHeightSelector
  , marginWidthSelector
  , nameSelector
  , scrollingSelector
  , setAlignSelector
  , setFrameBorderSelector
  , setHeightSelector
  , setLongDescSelector
  , setMarginHeightSelector
  , setMarginWidthSelector
  , setNameSelector
  , setScrollingSelector
  , setSrcSelector
  , setWidthSelector
  , srcSelector
  , widthSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- align@
align :: IsDOMHTMLIFrameElement domhtmliFrameElement => domhtmliFrameElement -> IO (Id NSString)
align domhtmliFrameElement =
  sendMessage domhtmliFrameElement alignSelector

-- | @- setAlign:@
setAlign :: (IsDOMHTMLIFrameElement domhtmliFrameElement, IsNSString value) => domhtmliFrameElement -> value -> IO ()
setAlign domhtmliFrameElement value =
  sendMessage domhtmliFrameElement setAlignSelector (toNSString value)

-- | @- frameBorder@
frameBorder :: IsDOMHTMLIFrameElement domhtmliFrameElement => domhtmliFrameElement -> IO (Id NSString)
frameBorder domhtmliFrameElement =
  sendMessage domhtmliFrameElement frameBorderSelector

-- | @- setFrameBorder:@
setFrameBorder :: (IsDOMHTMLIFrameElement domhtmliFrameElement, IsNSString value) => domhtmliFrameElement -> value -> IO ()
setFrameBorder domhtmliFrameElement value =
  sendMessage domhtmliFrameElement setFrameBorderSelector (toNSString value)

-- | @- height@
height :: IsDOMHTMLIFrameElement domhtmliFrameElement => domhtmliFrameElement -> IO (Id NSString)
height domhtmliFrameElement =
  sendMessage domhtmliFrameElement heightSelector

-- | @- setHeight:@
setHeight :: (IsDOMHTMLIFrameElement domhtmliFrameElement, IsNSString value) => domhtmliFrameElement -> value -> IO ()
setHeight domhtmliFrameElement value =
  sendMessage domhtmliFrameElement setHeightSelector (toNSString value)

-- | @- longDesc@
longDesc :: IsDOMHTMLIFrameElement domhtmliFrameElement => domhtmliFrameElement -> IO (Id NSString)
longDesc domhtmliFrameElement =
  sendMessage domhtmliFrameElement longDescSelector

-- | @- setLongDesc:@
setLongDesc :: (IsDOMHTMLIFrameElement domhtmliFrameElement, IsNSString value) => domhtmliFrameElement -> value -> IO ()
setLongDesc domhtmliFrameElement value =
  sendMessage domhtmliFrameElement setLongDescSelector (toNSString value)

-- | @- marginHeight@
marginHeight :: IsDOMHTMLIFrameElement domhtmliFrameElement => domhtmliFrameElement -> IO (Id NSString)
marginHeight domhtmliFrameElement =
  sendMessage domhtmliFrameElement marginHeightSelector

-- | @- setMarginHeight:@
setMarginHeight :: (IsDOMHTMLIFrameElement domhtmliFrameElement, IsNSString value) => domhtmliFrameElement -> value -> IO ()
setMarginHeight domhtmliFrameElement value =
  sendMessage domhtmliFrameElement setMarginHeightSelector (toNSString value)

-- | @- marginWidth@
marginWidth :: IsDOMHTMLIFrameElement domhtmliFrameElement => domhtmliFrameElement -> IO (Id NSString)
marginWidth domhtmliFrameElement =
  sendMessage domhtmliFrameElement marginWidthSelector

-- | @- setMarginWidth:@
setMarginWidth :: (IsDOMHTMLIFrameElement domhtmliFrameElement, IsNSString value) => domhtmliFrameElement -> value -> IO ()
setMarginWidth domhtmliFrameElement value =
  sendMessage domhtmliFrameElement setMarginWidthSelector (toNSString value)

-- | @- name@
name :: IsDOMHTMLIFrameElement domhtmliFrameElement => domhtmliFrameElement -> IO (Id NSString)
name domhtmliFrameElement =
  sendMessage domhtmliFrameElement nameSelector

-- | @- setName:@
setName :: (IsDOMHTMLIFrameElement domhtmliFrameElement, IsNSString value) => domhtmliFrameElement -> value -> IO ()
setName domhtmliFrameElement value =
  sendMessage domhtmliFrameElement setNameSelector (toNSString value)

-- | @- scrolling@
scrolling :: IsDOMHTMLIFrameElement domhtmliFrameElement => domhtmliFrameElement -> IO (Id NSString)
scrolling domhtmliFrameElement =
  sendMessage domhtmliFrameElement scrollingSelector

-- | @- setScrolling:@
setScrolling :: (IsDOMHTMLIFrameElement domhtmliFrameElement, IsNSString value) => domhtmliFrameElement -> value -> IO ()
setScrolling domhtmliFrameElement value =
  sendMessage domhtmliFrameElement setScrollingSelector (toNSString value)

-- | @- src@
src :: IsDOMHTMLIFrameElement domhtmliFrameElement => domhtmliFrameElement -> IO (Id NSString)
src domhtmliFrameElement =
  sendMessage domhtmliFrameElement srcSelector

-- | @- setSrc:@
setSrc :: (IsDOMHTMLIFrameElement domhtmliFrameElement, IsNSString value) => domhtmliFrameElement -> value -> IO ()
setSrc domhtmliFrameElement value =
  sendMessage domhtmliFrameElement setSrcSelector (toNSString value)

-- | @- width@
width :: IsDOMHTMLIFrameElement domhtmliFrameElement => domhtmliFrameElement -> IO (Id NSString)
width domhtmliFrameElement =
  sendMessage domhtmliFrameElement widthSelector

-- | @- setWidth:@
setWidth :: (IsDOMHTMLIFrameElement domhtmliFrameElement, IsNSString value) => domhtmliFrameElement -> value -> IO ()
setWidth domhtmliFrameElement value =
  sendMessage domhtmliFrameElement setWidthSelector (toNSString value)

-- | @- contentDocument@
contentDocument :: IsDOMHTMLIFrameElement domhtmliFrameElement => domhtmliFrameElement -> IO (Id DOMDocument)
contentDocument domhtmliFrameElement =
  sendMessage domhtmliFrameElement contentDocumentSelector

-- | @- contentWindow@
contentWindow :: IsDOMHTMLIFrameElement domhtmliFrameElement => domhtmliFrameElement -> IO (Id DOMAbstractView)
contentWindow domhtmliFrameElement =
  sendMessage domhtmliFrameElement contentWindowSelector

-- | contentFrame
--
-- Returns the content frame of the element.
--
-- ObjC selector: @- contentFrame@
contentFrame :: IsDOMHTMLIFrameElement domhtmliFrameElement => domhtmliFrameElement -> IO (Id WebFrame)
contentFrame domhtmliFrameElement =
  sendMessage domhtmliFrameElement contentFrameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @align@
alignSelector :: Selector '[] (Id NSString)
alignSelector = mkSelector "align"

-- | @Selector@ for @setAlign:@
setAlignSelector :: Selector '[Id NSString] ()
setAlignSelector = mkSelector "setAlign:"

-- | @Selector@ for @frameBorder@
frameBorderSelector :: Selector '[] (Id NSString)
frameBorderSelector = mkSelector "frameBorder"

-- | @Selector@ for @setFrameBorder:@
setFrameBorderSelector :: Selector '[Id NSString] ()
setFrameBorderSelector = mkSelector "setFrameBorder:"

-- | @Selector@ for @height@
heightSelector :: Selector '[] (Id NSString)
heightSelector = mkSelector "height"

-- | @Selector@ for @setHeight:@
setHeightSelector :: Selector '[Id NSString] ()
setHeightSelector = mkSelector "setHeight:"

-- | @Selector@ for @longDesc@
longDescSelector :: Selector '[] (Id NSString)
longDescSelector = mkSelector "longDesc"

-- | @Selector@ for @setLongDesc:@
setLongDescSelector :: Selector '[Id NSString] ()
setLongDescSelector = mkSelector "setLongDesc:"

-- | @Selector@ for @marginHeight@
marginHeightSelector :: Selector '[] (Id NSString)
marginHeightSelector = mkSelector "marginHeight"

-- | @Selector@ for @setMarginHeight:@
setMarginHeightSelector :: Selector '[Id NSString] ()
setMarginHeightSelector = mkSelector "setMarginHeight:"

-- | @Selector@ for @marginWidth@
marginWidthSelector :: Selector '[] (Id NSString)
marginWidthSelector = mkSelector "marginWidth"

-- | @Selector@ for @setMarginWidth:@
setMarginWidthSelector :: Selector '[Id NSString] ()
setMarginWidthSelector = mkSelector "setMarginWidth:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @scrolling@
scrollingSelector :: Selector '[] (Id NSString)
scrollingSelector = mkSelector "scrolling"

-- | @Selector@ for @setScrolling:@
setScrollingSelector :: Selector '[Id NSString] ()
setScrollingSelector = mkSelector "setScrolling:"

-- | @Selector@ for @src@
srcSelector :: Selector '[] (Id NSString)
srcSelector = mkSelector "src"

-- | @Selector@ for @setSrc:@
setSrcSelector :: Selector '[Id NSString] ()
setSrcSelector = mkSelector "setSrc:"

-- | @Selector@ for @width@
widthSelector :: Selector '[] (Id NSString)
widthSelector = mkSelector "width"

-- | @Selector@ for @setWidth:@
setWidthSelector :: Selector '[Id NSString] ()
setWidthSelector = mkSelector "setWidth:"

-- | @Selector@ for @contentDocument@
contentDocumentSelector :: Selector '[] (Id DOMDocument)
contentDocumentSelector = mkSelector "contentDocument"

-- | @Selector@ for @contentWindow@
contentWindowSelector :: Selector '[] (Id DOMAbstractView)
contentWindowSelector = mkSelector "contentWindow"

-- | @Selector@ for @contentFrame@
contentFrameSelector :: Selector '[] (Id WebFrame)
contentFrameSelector = mkSelector "contentFrame"

