{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLTableColElement@.
module ObjC.WebKit.DOMHTMLTableColElement
  ( DOMHTMLTableColElement
  , IsDOMHTMLTableColElement(..)
  , align
  , setAlign
  , ch
  , setCh
  , chOff
  , setChOff
  , span
  , setSpan
  , vAlign
  , setVAlign
  , width
  , setWidth
  , alignSelector
  , setAlignSelector
  , chSelector
  , setChSelector
  , chOffSelector
  , setChOffSelector
  , spanSelector
  , setSpanSelector
  , vAlignSelector
  , setVAlignSelector
  , widthSelector
  , setWidthSelector


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
align :: IsDOMHTMLTableColElement domhtmlTableColElement => domhtmlTableColElement -> IO (Id NSString)
align domhtmlTableColElement  =
  sendMsg domhtmlTableColElement (mkSelector "align") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAlign:@
setAlign :: (IsDOMHTMLTableColElement domhtmlTableColElement, IsNSString value) => domhtmlTableColElement -> value -> IO ()
setAlign domhtmlTableColElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlTableColElement (mkSelector "setAlign:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- ch@
ch :: IsDOMHTMLTableColElement domhtmlTableColElement => domhtmlTableColElement -> IO (Id NSString)
ch domhtmlTableColElement  =
  sendMsg domhtmlTableColElement (mkSelector "ch") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCh:@
setCh :: (IsDOMHTMLTableColElement domhtmlTableColElement, IsNSString value) => domhtmlTableColElement -> value -> IO ()
setCh domhtmlTableColElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlTableColElement (mkSelector "setCh:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- chOff@
chOff :: IsDOMHTMLTableColElement domhtmlTableColElement => domhtmlTableColElement -> IO (Id NSString)
chOff domhtmlTableColElement  =
  sendMsg domhtmlTableColElement (mkSelector "chOff") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setChOff:@
setChOff :: (IsDOMHTMLTableColElement domhtmlTableColElement, IsNSString value) => domhtmlTableColElement -> value -> IO ()
setChOff domhtmlTableColElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlTableColElement (mkSelector "setChOff:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- span@
span :: IsDOMHTMLTableColElement domhtmlTableColElement => domhtmlTableColElement -> IO CInt
span domhtmlTableColElement  =
  sendMsg domhtmlTableColElement (mkSelector "span") retCInt []

-- | @- setSpan:@
setSpan :: IsDOMHTMLTableColElement domhtmlTableColElement => domhtmlTableColElement -> CInt -> IO ()
setSpan domhtmlTableColElement  value =
  sendMsg domhtmlTableColElement (mkSelector "setSpan:") retVoid [argCInt (fromIntegral value)]

-- | @- vAlign@
vAlign :: IsDOMHTMLTableColElement domhtmlTableColElement => domhtmlTableColElement -> IO (Id NSString)
vAlign domhtmlTableColElement  =
  sendMsg domhtmlTableColElement (mkSelector "vAlign") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVAlign:@
setVAlign :: (IsDOMHTMLTableColElement domhtmlTableColElement, IsNSString value) => domhtmlTableColElement -> value -> IO ()
setVAlign domhtmlTableColElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlTableColElement (mkSelector "setVAlign:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- width@
width :: IsDOMHTMLTableColElement domhtmlTableColElement => domhtmlTableColElement -> IO (Id NSString)
width domhtmlTableColElement  =
  sendMsg domhtmlTableColElement (mkSelector "width") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setWidth:@
setWidth :: (IsDOMHTMLTableColElement domhtmlTableColElement, IsNSString value) => domhtmlTableColElement -> value -> IO ()
setWidth domhtmlTableColElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlTableColElement (mkSelector "setWidth:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @align@
alignSelector :: Selector
alignSelector = mkSelector "align"

-- | @Selector@ for @setAlign:@
setAlignSelector :: Selector
setAlignSelector = mkSelector "setAlign:"

-- | @Selector@ for @ch@
chSelector :: Selector
chSelector = mkSelector "ch"

-- | @Selector@ for @setCh:@
setChSelector :: Selector
setChSelector = mkSelector "setCh:"

-- | @Selector@ for @chOff@
chOffSelector :: Selector
chOffSelector = mkSelector "chOff"

-- | @Selector@ for @setChOff:@
setChOffSelector :: Selector
setChOffSelector = mkSelector "setChOff:"

-- | @Selector@ for @span@
spanSelector :: Selector
spanSelector = mkSelector "span"

-- | @Selector@ for @setSpan:@
setSpanSelector :: Selector
setSpanSelector = mkSelector "setSpan:"

-- | @Selector@ for @vAlign@
vAlignSelector :: Selector
vAlignSelector = mkSelector "vAlign"

-- | @Selector@ for @setVAlign:@
setVAlignSelector :: Selector
setVAlignSelector = mkSelector "setVAlign:"

-- | @Selector@ for @width@
widthSelector :: Selector
widthSelector = mkSelector "width"

-- | @Selector@ for @setWidth:@
setWidthSelector :: Selector
setWidthSelector = mkSelector "setWidth:"

