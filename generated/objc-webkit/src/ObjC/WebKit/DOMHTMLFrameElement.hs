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
  , frameBorderSelector
  , setFrameBorderSelector
  , longDescSelector
  , setLongDescSelector
  , marginHeightSelector
  , setMarginHeightSelector
  , marginWidthSelector
  , setMarginWidthSelector
  , nameSelector
  , setNameSelector
  , noResizeSelector
  , setNoResizeSelector
  , scrollingSelector
  , setScrollingSelector
  , srcSelector
  , setSrcSelector
  , contentDocumentSelector
  , contentWindowSelector
  , locationSelector
  , setLocationSelector
  , widthSelector
  , heightSelector
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

-- | @- frameBorder@
frameBorder :: IsDOMHTMLFrameElement domhtmlFrameElement => domhtmlFrameElement -> IO (Id NSString)
frameBorder domhtmlFrameElement  =
  sendMsg domhtmlFrameElement (mkSelector "frameBorder") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFrameBorder:@
setFrameBorder :: (IsDOMHTMLFrameElement domhtmlFrameElement, IsNSString value) => domhtmlFrameElement -> value -> IO ()
setFrameBorder domhtmlFrameElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlFrameElement (mkSelector "setFrameBorder:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- longDesc@
longDesc :: IsDOMHTMLFrameElement domhtmlFrameElement => domhtmlFrameElement -> IO (Id NSString)
longDesc domhtmlFrameElement  =
  sendMsg domhtmlFrameElement (mkSelector "longDesc") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLongDesc:@
setLongDesc :: (IsDOMHTMLFrameElement domhtmlFrameElement, IsNSString value) => domhtmlFrameElement -> value -> IO ()
setLongDesc domhtmlFrameElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlFrameElement (mkSelector "setLongDesc:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- marginHeight@
marginHeight :: IsDOMHTMLFrameElement domhtmlFrameElement => domhtmlFrameElement -> IO (Id NSString)
marginHeight domhtmlFrameElement  =
  sendMsg domhtmlFrameElement (mkSelector "marginHeight") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMarginHeight:@
setMarginHeight :: (IsDOMHTMLFrameElement domhtmlFrameElement, IsNSString value) => domhtmlFrameElement -> value -> IO ()
setMarginHeight domhtmlFrameElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlFrameElement (mkSelector "setMarginHeight:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- marginWidth@
marginWidth :: IsDOMHTMLFrameElement domhtmlFrameElement => domhtmlFrameElement -> IO (Id NSString)
marginWidth domhtmlFrameElement  =
  sendMsg domhtmlFrameElement (mkSelector "marginWidth") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMarginWidth:@
setMarginWidth :: (IsDOMHTMLFrameElement domhtmlFrameElement, IsNSString value) => domhtmlFrameElement -> value -> IO ()
setMarginWidth domhtmlFrameElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlFrameElement (mkSelector "setMarginWidth:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- name@
name :: IsDOMHTMLFrameElement domhtmlFrameElement => domhtmlFrameElement -> IO (Id NSString)
name domhtmlFrameElement  =
  sendMsg domhtmlFrameElement (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsDOMHTMLFrameElement domhtmlFrameElement, IsNSString value) => domhtmlFrameElement -> value -> IO ()
setName domhtmlFrameElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlFrameElement (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- noResize@
noResize :: IsDOMHTMLFrameElement domhtmlFrameElement => domhtmlFrameElement -> IO Bool
noResize domhtmlFrameElement  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domhtmlFrameElement (mkSelector "noResize") retCULong []

-- | @- setNoResize:@
setNoResize :: IsDOMHTMLFrameElement domhtmlFrameElement => domhtmlFrameElement -> Bool -> IO ()
setNoResize domhtmlFrameElement  value =
  sendMsg domhtmlFrameElement (mkSelector "setNoResize:") retVoid [argCULong (if value then 1 else 0)]

-- | @- scrolling@
scrolling :: IsDOMHTMLFrameElement domhtmlFrameElement => domhtmlFrameElement -> IO (Id NSString)
scrolling domhtmlFrameElement  =
  sendMsg domhtmlFrameElement (mkSelector "scrolling") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setScrolling:@
setScrolling :: (IsDOMHTMLFrameElement domhtmlFrameElement, IsNSString value) => domhtmlFrameElement -> value -> IO ()
setScrolling domhtmlFrameElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlFrameElement (mkSelector "setScrolling:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- src@
src :: IsDOMHTMLFrameElement domhtmlFrameElement => domhtmlFrameElement -> IO (Id NSString)
src domhtmlFrameElement  =
  sendMsg domhtmlFrameElement (mkSelector "src") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSrc:@
setSrc :: (IsDOMHTMLFrameElement domhtmlFrameElement, IsNSString value) => domhtmlFrameElement -> value -> IO ()
setSrc domhtmlFrameElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlFrameElement (mkSelector "setSrc:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- contentDocument@
contentDocument :: IsDOMHTMLFrameElement domhtmlFrameElement => domhtmlFrameElement -> IO (Id DOMDocument)
contentDocument domhtmlFrameElement  =
  sendMsg domhtmlFrameElement (mkSelector "contentDocument") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- contentWindow@
contentWindow :: IsDOMHTMLFrameElement domhtmlFrameElement => domhtmlFrameElement -> IO (Id DOMAbstractView)
contentWindow domhtmlFrameElement  =
  sendMsg domhtmlFrameElement (mkSelector "contentWindow") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- location@
location :: IsDOMHTMLFrameElement domhtmlFrameElement => domhtmlFrameElement -> IO (Id NSString)
location domhtmlFrameElement  =
  sendMsg domhtmlFrameElement (mkSelector "location") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLocation:@
setLocation :: (IsDOMHTMLFrameElement domhtmlFrameElement, IsNSString value) => domhtmlFrameElement -> value -> IO ()
setLocation domhtmlFrameElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlFrameElement (mkSelector "setLocation:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- width@
width :: IsDOMHTMLFrameElement domhtmlFrameElement => domhtmlFrameElement -> IO CInt
width domhtmlFrameElement  =
  sendMsg domhtmlFrameElement (mkSelector "width") retCInt []

-- | @- height@
height :: IsDOMHTMLFrameElement domhtmlFrameElement => domhtmlFrameElement -> IO CInt
height domhtmlFrameElement  =
  sendMsg domhtmlFrameElement (mkSelector "height") retCInt []

-- | contentFrame
--
-- The content frame of the element.
--
-- ObjC selector: @- contentFrame@
contentFrame :: IsDOMHTMLFrameElement domhtmlFrameElement => domhtmlFrameElement -> IO (Id WebFrame)
contentFrame domhtmlFrameElement  =
  sendMsg domhtmlFrameElement (mkSelector "contentFrame") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @frameBorder@
frameBorderSelector :: Selector
frameBorderSelector = mkSelector "frameBorder"

-- | @Selector@ for @setFrameBorder:@
setFrameBorderSelector :: Selector
setFrameBorderSelector = mkSelector "setFrameBorder:"

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

-- | @Selector@ for @noResize@
noResizeSelector :: Selector
noResizeSelector = mkSelector "noResize"

-- | @Selector@ for @setNoResize:@
setNoResizeSelector :: Selector
setNoResizeSelector = mkSelector "setNoResize:"

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

-- | @Selector@ for @contentDocument@
contentDocumentSelector :: Selector
contentDocumentSelector = mkSelector "contentDocument"

-- | @Selector@ for @contentWindow@
contentWindowSelector :: Selector
contentWindowSelector = mkSelector "contentWindow"

-- | @Selector@ for @location@
locationSelector :: Selector
locationSelector = mkSelector "location"

-- | @Selector@ for @setLocation:@
setLocationSelector :: Selector
setLocationSelector = mkSelector "setLocation:"

-- | @Selector@ for @width@
widthSelector :: Selector
widthSelector = mkSelector "width"

-- | @Selector@ for @height@
heightSelector :: Selector
heightSelector = mkSelector "height"

-- | @Selector@ for @contentFrame@
contentFrameSelector :: Selector
contentFrameSelector = mkSelector "contentFrame"

