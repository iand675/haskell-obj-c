{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLFrameElement@.
module ObjC.WebKit.DOMHTMLFrameElement
  ( DOMHTMLFrameElement
  , IsDOMHTMLFrameElement(..)
  , frameBorder
  , setFrameBorder
  , longDesc
  , setLongDesc
  , marginHeight
  , setMarginHeight
  , marginWidth
  , setMarginWidth
  , name
  , setName
  , noResize
  , setNoResize
  , scrolling
  , setScrolling
  , src
  , setSrc
  , contentDocument
  , contentWindow
  , location
  , setLocation
  , width
  , height
  , contentFrame
  , contentDocumentSelector
  , contentFrameSelector
  , contentWindowSelector
  , frameBorderSelector
  , heightSelector
  , locationSelector
  , longDescSelector
  , marginHeightSelector
  , marginWidthSelector
  , nameSelector
  , noResizeSelector
  , scrollingSelector
  , setFrameBorderSelector
  , setLocationSelector
  , setLongDescSelector
  , setMarginHeightSelector
  , setMarginWidthSelector
  , setNameSelector
  , setNoResizeSelector
  , setScrollingSelector
  , setSrcSelector
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

-- | @- frameBorder@
frameBorder :: IsDOMHTMLFrameElement domhtmlFrameElement => domhtmlFrameElement -> IO (Id NSString)
frameBorder domhtmlFrameElement =
  sendMessage domhtmlFrameElement frameBorderSelector

-- | @- setFrameBorder:@
setFrameBorder :: (IsDOMHTMLFrameElement domhtmlFrameElement, IsNSString value) => domhtmlFrameElement -> value -> IO ()
setFrameBorder domhtmlFrameElement value =
  sendMessage domhtmlFrameElement setFrameBorderSelector (toNSString value)

-- | @- longDesc@
longDesc :: IsDOMHTMLFrameElement domhtmlFrameElement => domhtmlFrameElement -> IO (Id NSString)
longDesc domhtmlFrameElement =
  sendMessage domhtmlFrameElement longDescSelector

-- | @- setLongDesc:@
setLongDesc :: (IsDOMHTMLFrameElement domhtmlFrameElement, IsNSString value) => domhtmlFrameElement -> value -> IO ()
setLongDesc domhtmlFrameElement value =
  sendMessage domhtmlFrameElement setLongDescSelector (toNSString value)

-- | @- marginHeight@
marginHeight :: IsDOMHTMLFrameElement domhtmlFrameElement => domhtmlFrameElement -> IO (Id NSString)
marginHeight domhtmlFrameElement =
  sendMessage domhtmlFrameElement marginHeightSelector

-- | @- setMarginHeight:@
setMarginHeight :: (IsDOMHTMLFrameElement domhtmlFrameElement, IsNSString value) => domhtmlFrameElement -> value -> IO ()
setMarginHeight domhtmlFrameElement value =
  sendMessage domhtmlFrameElement setMarginHeightSelector (toNSString value)

-- | @- marginWidth@
marginWidth :: IsDOMHTMLFrameElement domhtmlFrameElement => domhtmlFrameElement -> IO (Id NSString)
marginWidth domhtmlFrameElement =
  sendMessage domhtmlFrameElement marginWidthSelector

-- | @- setMarginWidth:@
setMarginWidth :: (IsDOMHTMLFrameElement domhtmlFrameElement, IsNSString value) => domhtmlFrameElement -> value -> IO ()
setMarginWidth domhtmlFrameElement value =
  sendMessage domhtmlFrameElement setMarginWidthSelector (toNSString value)

-- | @- name@
name :: IsDOMHTMLFrameElement domhtmlFrameElement => domhtmlFrameElement -> IO (Id NSString)
name domhtmlFrameElement =
  sendMessage domhtmlFrameElement nameSelector

-- | @- setName:@
setName :: (IsDOMHTMLFrameElement domhtmlFrameElement, IsNSString value) => domhtmlFrameElement -> value -> IO ()
setName domhtmlFrameElement value =
  sendMessage domhtmlFrameElement setNameSelector (toNSString value)

-- | @- noResize@
noResize :: IsDOMHTMLFrameElement domhtmlFrameElement => domhtmlFrameElement -> IO Bool
noResize domhtmlFrameElement =
  sendMessage domhtmlFrameElement noResizeSelector

-- | @- setNoResize:@
setNoResize :: IsDOMHTMLFrameElement domhtmlFrameElement => domhtmlFrameElement -> Bool -> IO ()
setNoResize domhtmlFrameElement value =
  sendMessage domhtmlFrameElement setNoResizeSelector value

-- | @- scrolling@
scrolling :: IsDOMHTMLFrameElement domhtmlFrameElement => domhtmlFrameElement -> IO (Id NSString)
scrolling domhtmlFrameElement =
  sendMessage domhtmlFrameElement scrollingSelector

-- | @- setScrolling:@
setScrolling :: (IsDOMHTMLFrameElement domhtmlFrameElement, IsNSString value) => domhtmlFrameElement -> value -> IO ()
setScrolling domhtmlFrameElement value =
  sendMessage domhtmlFrameElement setScrollingSelector (toNSString value)

-- | @- src@
src :: IsDOMHTMLFrameElement domhtmlFrameElement => domhtmlFrameElement -> IO (Id NSString)
src domhtmlFrameElement =
  sendMessage domhtmlFrameElement srcSelector

-- | @- setSrc:@
setSrc :: (IsDOMHTMLFrameElement domhtmlFrameElement, IsNSString value) => domhtmlFrameElement -> value -> IO ()
setSrc domhtmlFrameElement value =
  sendMessage domhtmlFrameElement setSrcSelector (toNSString value)

-- | @- contentDocument@
contentDocument :: IsDOMHTMLFrameElement domhtmlFrameElement => domhtmlFrameElement -> IO (Id DOMDocument)
contentDocument domhtmlFrameElement =
  sendMessage domhtmlFrameElement contentDocumentSelector

-- | @- contentWindow@
contentWindow :: IsDOMHTMLFrameElement domhtmlFrameElement => domhtmlFrameElement -> IO (Id DOMAbstractView)
contentWindow domhtmlFrameElement =
  sendMessage domhtmlFrameElement contentWindowSelector

-- | @- location@
location :: IsDOMHTMLFrameElement domhtmlFrameElement => domhtmlFrameElement -> IO (Id NSString)
location domhtmlFrameElement =
  sendMessage domhtmlFrameElement locationSelector

-- | @- setLocation:@
setLocation :: (IsDOMHTMLFrameElement domhtmlFrameElement, IsNSString value) => domhtmlFrameElement -> value -> IO ()
setLocation domhtmlFrameElement value =
  sendMessage domhtmlFrameElement setLocationSelector (toNSString value)

-- | @- width@
width :: IsDOMHTMLFrameElement domhtmlFrameElement => domhtmlFrameElement -> IO CInt
width domhtmlFrameElement =
  sendMessage domhtmlFrameElement widthSelector

-- | @- height@
height :: IsDOMHTMLFrameElement domhtmlFrameElement => domhtmlFrameElement -> IO CInt
height domhtmlFrameElement =
  sendMessage domhtmlFrameElement heightSelector

-- | contentFrame
--
-- The content frame of the element.
--
-- ObjC selector: @- contentFrame@
contentFrame :: IsDOMHTMLFrameElement domhtmlFrameElement => domhtmlFrameElement -> IO (Id WebFrame)
contentFrame domhtmlFrameElement =
  sendMessage domhtmlFrameElement contentFrameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @frameBorder@
frameBorderSelector :: Selector '[] (Id NSString)
frameBorderSelector = mkSelector "frameBorder"

-- | @Selector@ for @setFrameBorder:@
setFrameBorderSelector :: Selector '[Id NSString] ()
setFrameBorderSelector = mkSelector "setFrameBorder:"

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

-- | @Selector@ for @noResize@
noResizeSelector :: Selector '[] Bool
noResizeSelector = mkSelector "noResize"

-- | @Selector@ for @setNoResize:@
setNoResizeSelector :: Selector '[Bool] ()
setNoResizeSelector = mkSelector "setNoResize:"

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

-- | @Selector@ for @contentDocument@
contentDocumentSelector :: Selector '[] (Id DOMDocument)
contentDocumentSelector = mkSelector "contentDocument"

-- | @Selector@ for @contentWindow@
contentWindowSelector :: Selector '[] (Id DOMAbstractView)
contentWindowSelector = mkSelector "contentWindow"

-- | @Selector@ for @location@
locationSelector :: Selector '[] (Id NSString)
locationSelector = mkSelector "location"

-- | @Selector@ for @setLocation:@
setLocationSelector :: Selector '[Id NSString] ()
setLocationSelector = mkSelector "setLocation:"

-- | @Selector@ for @width@
widthSelector :: Selector '[] CInt
widthSelector = mkSelector "width"

-- | @Selector@ for @height@
heightSelector :: Selector '[] CInt
heightSelector = mkSelector "height"

-- | @Selector@ for @contentFrame@
contentFrameSelector :: Selector '[] (Id WebFrame)
contentFrameSelector = mkSelector "contentFrame"

