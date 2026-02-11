{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLTableCellElement@.
module ObjC.WebKit.DOMHTMLTableCellElement
  ( DOMHTMLTableCellElement
  , IsDOMHTMLTableCellElement(..)
  , cellIndex
  , align
  , setAlign
  , axis
  , setAxis
  , bgColor
  , setBgColor
  , ch
  , setCh
  , chOff
  , setChOff
  , colSpan
  , setColSpan
  , rowSpan
  , setRowSpan
  , headers
  , setHeaders
  , height
  , setHeight
  , noWrap
  , setNoWrap
  , vAlign
  , setVAlign
  , width
  , setWidth
  , abbr
  , setAbbr
  , scope
  , setScope
  , cellIndexSelector
  , alignSelector
  , setAlignSelector
  , axisSelector
  , setAxisSelector
  , bgColorSelector
  , setBgColorSelector
  , chSelector
  , setChSelector
  , chOffSelector
  , setChOffSelector
  , colSpanSelector
  , setColSpanSelector
  , rowSpanSelector
  , setRowSpanSelector
  , headersSelector
  , setHeadersSelector
  , heightSelector
  , setHeightSelector
  , noWrapSelector
  , setNoWrapSelector
  , vAlignSelector
  , setVAlignSelector
  , widthSelector
  , setWidthSelector
  , abbrSelector
  , setAbbrSelector
  , scopeSelector
  , setScopeSelector


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

-- | @- cellIndex@
cellIndex :: IsDOMHTMLTableCellElement domhtmlTableCellElement => domhtmlTableCellElement -> IO CInt
cellIndex domhtmlTableCellElement  =
  sendMsg domhtmlTableCellElement (mkSelector "cellIndex") retCInt []

-- | @- align@
align :: IsDOMHTMLTableCellElement domhtmlTableCellElement => domhtmlTableCellElement -> IO (Id NSString)
align domhtmlTableCellElement  =
  sendMsg domhtmlTableCellElement (mkSelector "align") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAlign:@
setAlign :: (IsDOMHTMLTableCellElement domhtmlTableCellElement, IsNSString value) => domhtmlTableCellElement -> value -> IO ()
setAlign domhtmlTableCellElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlTableCellElement (mkSelector "setAlign:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- axis@
axis :: IsDOMHTMLTableCellElement domhtmlTableCellElement => domhtmlTableCellElement -> IO (Id NSString)
axis domhtmlTableCellElement  =
  sendMsg domhtmlTableCellElement (mkSelector "axis") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAxis:@
setAxis :: (IsDOMHTMLTableCellElement domhtmlTableCellElement, IsNSString value) => domhtmlTableCellElement -> value -> IO ()
setAxis domhtmlTableCellElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlTableCellElement (mkSelector "setAxis:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- bgColor@
bgColor :: IsDOMHTMLTableCellElement domhtmlTableCellElement => domhtmlTableCellElement -> IO (Id NSString)
bgColor domhtmlTableCellElement  =
  sendMsg domhtmlTableCellElement (mkSelector "bgColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBgColor:@
setBgColor :: (IsDOMHTMLTableCellElement domhtmlTableCellElement, IsNSString value) => domhtmlTableCellElement -> value -> IO ()
setBgColor domhtmlTableCellElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlTableCellElement (mkSelector "setBgColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- ch@
ch :: IsDOMHTMLTableCellElement domhtmlTableCellElement => domhtmlTableCellElement -> IO (Id NSString)
ch domhtmlTableCellElement  =
  sendMsg domhtmlTableCellElement (mkSelector "ch") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCh:@
setCh :: (IsDOMHTMLTableCellElement domhtmlTableCellElement, IsNSString value) => domhtmlTableCellElement -> value -> IO ()
setCh domhtmlTableCellElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlTableCellElement (mkSelector "setCh:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- chOff@
chOff :: IsDOMHTMLTableCellElement domhtmlTableCellElement => domhtmlTableCellElement -> IO (Id NSString)
chOff domhtmlTableCellElement  =
  sendMsg domhtmlTableCellElement (mkSelector "chOff") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setChOff:@
setChOff :: (IsDOMHTMLTableCellElement domhtmlTableCellElement, IsNSString value) => domhtmlTableCellElement -> value -> IO ()
setChOff domhtmlTableCellElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlTableCellElement (mkSelector "setChOff:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- colSpan@
colSpan :: IsDOMHTMLTableCellElement domhtmlTableCellElement => domhtmlTableCellElement -> IO CInt
colSpan domhtmlTableCellElement  =
  sendMsg domhtmlTableCellElement (mkSelector "colSpan") retCInt []

-- | @- setColSpan:@
setColSpan :: IsDOMHTMLTableCellElement domhtmlTableCellElement => domhtmlTableCellElement -> CInt -> IO ()
setColSpan domhtmlTableCellElement  value =
  sendMsg domhtmlTableCellElement (mkSelector "setColSpan:") retVoid [argCInt (fromIntegral value)]

-- | @- rowSpan@
rowSpan :: IsDOMHTMLTableCellElement domhtmlTableCellElement => domhtmlTableCellElement -> IO CInt
rowSpan domhtmlTableCellElement  =
  sendMsg domhtmlTableCellElement (mkSelector "rowSpan") retCInt []

-- | @- setRowSpan:@
setRowSpan :: IsDOMHTMLTableCellElement domhtmlTableCellElement => domhtmlTableCellElement -> CInt -> IO ()
setRowSpan domhtmlTableCellElement  value =
  sendMsg domhtmlTableCellElement (mkSelector "setRowSpan:") retVoid [argCInt (fromIntegral value)]

-- | @- headers@
headers :: IsDOMHTMLTableCellElement domhtmlTableCellElement => domhtmlTableCellElement -> IO (Id NSString)
headers domhtmlTableCellElement  =
  sendMsg domhtmlTableCellElement (mkSelector "headers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setHeaders:@
setHeaders :: (IsDOMHTMLTableCellElement domhtmlTableCellElement, IsNSString value) => domhtmlTableCellElement -> value -> IO ()
setHeaders domhtmlTableCellElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlTableCellElement (mkSelector "setHeaders:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- height@
height :: IsDOMHTMLTableCellElement domhtmlTableCellElement => domhtmlTableCellElement -> IO (Id NSString)
height domhtmlTableCellElement  =
  sendMsg domhtmlTableCellElement (mkSelector "height") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setHeight:@
setHeight :: (IsDOMHTMLTableCellElement domhtmlTableCellElement, IsNSString value) => domhtmlTableCellElement -> value -> IO ()
setHeight domhtmlTableCellElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlTableCellElement (mkSelector "setHeight:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- noWrap@
noWrap :: IsDOMHTMLTableCellElement domhtmlTableCellElement => domhtmlTableCellElement -> IO Bool
noWrap domhtmlTableCellElement  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domhtmlTableCellElement (mkSelector "noWrap") retCULong []

-- | @- setNoWrap:@
setNoWrap :: IsDOMHTMLTableCellElement domhtmlTableCellElement => domhtmlTableCellElement -> Bool -> IO ()
setNoWrap domhtmlTableCellElement  value =
  sendMsg domhtmlTableCellElement (mkSelector "setNoWrap:") retVoid [argCULong (if value then 1 else 0)]

-- | @- vAlign@
vAlign :: IsDOMHTMLTableCellElement domhtmlTableCellElement => domhtmlTableCellElement -> IO (Id NSString)
vAlign domhtmlTableCellElement  =
  sendMsg domhtmlTableCellElement (mkSelector "vAlign") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVAlign:@
setVAlign :: (IsDOMHTMLTableCellElement domhtmlTableCellElement, IsNSString value) => domhtmlTableCellElement -> value -> IO ()
setVAlign domhtmlTableCellElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlTableCellElement (mkSelector "setVAlign:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- width@
width :: IsDOMHTMLTableCellElement domhtmlTableCellElement => domhtmlTableCellElement -> IO (Id NSString)
width domhtmlTableCellElement  =
  sendMsg domhtmlTableCellElement (mkSelector "width") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setWidth:@
setWidth :: (IsDOMHTMLTableCellElement domhtmlTableCellElement, IsNSString value) => domhtmlTableCellElement -> value -> IO ()
setWidth domhtmlTableCellElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlTableCellElement (mkSelector "setWidth:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- abbr@
abbr :: IsDOMHTMLTableCellElement domhtmlTableCellElement => domhtmlTableCellElement -> IO (Id NSString)
abbr domhtmlTableCellElement  =
  sendMsg domhtmlTableCellElement (mkSelector "abbr") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAbbr:@
setAbbr :: (IsDOMHTMLTableCellElement domhtmlTableCellElement, IsNSString value) => domhtmlTableCellElement -> value -> IO ()
setAbbr domhtmlTableCellElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlTableCellElement (mkSelector "setAbbr:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- scope@
scope :: IsDOMHTMLTableCellElement domhtmlTableCellElement => domhtmlTableCellElement -> IO (Id NSString)
scope domhtmlTableCellElement  =
  sendMsg domhtmlTableCellElement (mkSelector "scope") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setScope:@
setScope :: (IsDOMHTMLTableCellElement domhtmlTableCellElement, IsNSString value) => domhtmlTableCellElement -> value -> IO ()
setScope domhtmlTableCellElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlTableCellElement (mkSelector "setScope:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cellIndex@
cellIndexSelector :: Selector
cellIndexSelector = mkSelector "cellIndex"

-- | @Selector@ for @align@
alignSelector :: Selector
alignSelector = mkSelector "align"

-- | @Selector@ for @setAlign:@
setAlignSelector :: Selector
setAlignSelector = mkSelector "setAlign:"

-- | @Selector@ for @axis@
axisSelector :: Selector
axisSelector = mkSelector "axis"

-- | @Selector@ for @setAxis:@
setAxisSelector :: Selector
setAxisSelector = mkSelector "setAxis:"

-- | @Selector@ for @bgColor@
bgColorSelector :: Selector
bgColorSelector = mkSelector "bgColor"

-- | @Selector@ for @setBgColor:@
setBgColorSelector :: Selector
setBgColorSelector = mkSelector "setBgColor:"

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

-- | @Selector@ for @colSpan@
colSpanSelector :: Selector
colSpanSelector = mkSelector "colSpan"

-- | @Selector@ for @setColSpan:@
setColSpanSelector :: Selector
setColSpanSelector = mkSelector "setColSpan:"

-- | @Selector@ for @rowSpan@
rowSpanSelector :: Selector
rowSpanSelector = mkSelector "rowSpan"

-- | @Selector@ for @setRowSpan:@
setRowSpanSelector :: Selector
setRowSpanSelector = mkSelector "setRowSpan:"

-- | @Selector@ for @headers@
headersSelector :: Selector
headersSelector = mkSelector "headers"

-- | @Selector@ for @setHeaders:@
setHeadersSelector :: Selector
setHeadersSelector = mkSelector "setHeaders:"

-- | @Selector@ for @height@
heightSelector :: Selector
heightSelector = mkSelector "height"

-- | @Selector@ for @setHeight:@
setHeightSelector :: Selector
setHeightSelector = mkSelector "setHeight:"

-- | @Selector@ for @noWrap@
noWrapSelector :: Selector
noWrapSelector = mkSelector "noWrap"

-- | @Selector@ for @setNoWrap:@
setNoWrapSelector :: Selector
setNoWrapSelector = mkSelector "setNoWrap:"

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

-- | @Selector@ for @abbr@
abbrSelector :: Selector
abbrSelector = mkSelector "abbr"

-- | @Selector@ for @setAbbr:@
setAbbrSelector :: Selector
setAbbrSelector = mkSelector "setAbbr:"

-- | @Selector@ for @scope@
scopeSelector :: Selector
scopeSelector = mkSelector "scope"

-- | @Selector@ for @setScope:@
setScopeSelector :: Selector
setScopeSelector = mkSelector "setScope:"

