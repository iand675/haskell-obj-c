{-# LANGUAGE DataKinds #-}
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
  , abbrSelector
  , alignSelector
  , axisSelector
  , bgColorSelector
  , cellIndexSelector
  , chOffSelector
  , chSelector
  , colSpanSelector
  , headersSelector
  , heightSelector
  , noWrapSelector
  , rowSpanSelector
  , scopeSelector
  , setAbbrSelector
  , setAlignSelector
  , setAxisSelector
  , setBgColorSelector
  , setChOffSelector
  , setChSelector
  , setColSpanSelector
  , setHeadersSelector
  , setHeightSelector
  , setNoWrapSelector
  , setRowSpanSelector
  , setScopeSelector
  , setVAlignSelector
  , setWidthSelector
  , vAlignSelector
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

-- | @- cellIndex@
cellIndex :: IsDOMHTMLTableCellElement domhtmlTableCellElement => domhtmlTableCellElement -> IO CInt
cellIndex domhtmlTableCellElement =
  sendMessage domhtmlTableCellElement cellIndexSelector

-- | @- align@
align :: IsDOMHTMLTableCellElement domhtmlTableCellElement => domhtmlTableCellElement -> IO (Id NSString)
align domhtmlTableCellElement =
  sendMessage domhtmlTableCellElement alignSelector

-- | @- setAlign:@
setAlign :: (IsDOMHTMLTableCellElement domhtmlTableCellElement, IsNSString value) => domhtmlTableCellElement -> value -> IO ()
setAlign domhtmlTableCellElement value =
  sendMessage domhtmlTableCellElement setAlignSelector (toNSString value)

-- | @- axis@
axis :: IsDOMHTMLTableCellElement domhtmlTableCellElement => domhtmlTableCellElement -> IO (Id NSString)
axis domhtmlTableCellElement =
  sendMessage domhtmlTableCellElement axisSelector

-- | @- setAxis:@
setAxis :: (IsDOMHTMLTableCellElement domhtmlTableCellElement, IsNSString value) => domhtmlTableCellElement -> value -> IO ()
setAxis domhtmlTableCellElement value =
  sendMessage domhtmlTableCellElement setAxisSelector (toNSString value)

-- | @- bgColor@
bgColor :: IsDOMHTMLTableCellElement domhtmlTableCellElement => domhtmlTableCellElement -> IO (Id NSString)
bgColor domhtmlTableCellElement =
  sendMessage domhtmlTableCellElement bgColorSelector

-- | @- setBgColor:@
setBgColor :: (IsDOMHTMLTableCellElement domhtmlTableCellElement, IsNSString value) => domhtmlTableCellElement -> value -> IO ()
setBgColor domhtmlTableCellElement value =
  sendMessage domhtmlTableCellElement setBgColorSelector (toNSString value)

-- | @- ch@
ch :: IsDOMHTMLTableCellElement domhtmlTableCellElement => domhtmlTableCellElement -> IO (Id NSString)
ch domhtmlTableCellElement =
  sendMessage domhtmlTableCellElement chSelector

-- | @- setCh:@
setCh :: (IsDOMHTMLTableCellElement domhtmlTableCellElement, IsNSString value) => domhtmlTableCellElement -> value -> IO ()
setCh domhtmlTableCellElement value =
  sendMessage domhtmlTableCellElement setChSelector (toNSString value)

-- | @- chOff@
chOff :: IsDOMHTMLTableCellElement domhtmlTableCellElement => domhtmlTableCellElement -> IO (Id NSString)
chOff domhtmlTableCellElement =
  sendMessage domhtmlTableCellElement chOffSelector

-- | @- setChOff:@
setChOff :: (IsDOMHTMLTableCellElement domhtmlTableCellElement, IsNSString value) => domhtmlTableCellElement -> value -> IO ()
setChOff domhtmlTableCellElement value =
  sendMessage domhtmlTableCellElement setChOffSelector (toNSString value)

-- | @- colSpan@
colSpan :: IsDOMHTMLTableCellElement domhtmlTableCellElement => domhtmlTableCellElement -> IO CInt
colSpan domhtmlTableCellElement =
  sendMessage domhtmlTableCellElement colSpanSelector

-- | @- setColSpan:@
setColSpan :: IsDOMHTMLTableCellElement domhtmlTableCellElement => domhtmlTableCellElement -> CInt -> IO ()
setColSpan domhtmlTableCellElement value =
  sendMessage domhtmlTableCellElement setColSpanSelector value

-- | @- rowSpan@
rowSpan :: IsDOMHTMLTableCellElement domhtmlTableCellElement => domhtmlTableCellElement -> IO CInt
rowSpan domhtmlTableCellElement =
  sendMessage domhtmlTableCellElement rowSpanSelector

-- | @- setRowSpan:@
setRowSpan :: IsDOMHTMLTableCellElement domhtmlTableCellElement => domhtmlTableCellElement -> CInt -> IO ()
setRowSpan domhtmlTableCellElement value =
  sendMessage domhtmlTableCellElement setRowSpanSelector value

-- | @- headers@
headers :: IsDOMHTMLTableCellElement domhtmlTableCellElement => domhtmlTableCellElement -> IO (Id NSString)
headers domhtmlTableCellElement =
  sendMessage domhtmlTableCellElement headersSelector

-- | @- setHeaders:@
setHeaders :: (IsDOMHTMLTableCellElement domhtmlTableCellElement, IsNSString value) => domhtmlTableCellElement -> value -> IO ()
setHeaders domhtmlTableCellElement value =
  sendMessage domhtmlTableCellElement setHeadersSelector (toNSString value)

-- | @- height@
height :: IsDOMHTMLTableCellElement domhtmlTableCellElement => domhtmlTableCellElement -> IO (Id NSString)
height domhtmlTableCellElement =
  sendMessage domhtmlTableCellElement heightSelector

-- | @- setHeight:@
setHeight :: (IsDOMHTMLTableCellElement domhtmlTableCellElement, IsNSString value) => domhtmlTableCellElement -> value -> IO ()
setHeight domhtmlTableCellElement value =
  sendMessage domhtmlTableCellElement setHeightSelector (toNSString value)

-- | @- noWrap@
noWrap :: IsDOMHTMLTableCellElement domhtmlTableCellElement => domhtmlTableCellElement -> IO Bool
noWrap domhtmlTableCellElement =
  sendMessage domhtmlTableCellElement noWrapSelector

-- | @- setNoWrap:@
setNoWrap :: IsDOMHTMLTableCellElement domhtmlTableCellElement => domhtmlTableCellElement -> Bool -> IO ()
setNoWrap domhtmlTableCellElement value =
  sendMessage domhtmlTableCellElement setNoWrapSelector value

-- | @- vAlign@
vAlign :: IsDOMHTMLTableCellElement domhtmlTableCellElement => domhtmlTableCellElement -> IO (Id NSString)
vAlign domhtmlTableCellElement =
  sendMessage domhtmlTableCellElement vAlignSelector

-- | @- setVAlign:@
setVAlign :: (IsDOMHTMLTableCellElement domhtmlTableCellElement, IsNSString value) => domhtmlTableCellElement -> value -> IO ()
setVAlign domhtmlTableCellElement value =
  sendMessage domhtmlTableCellElement setVAlignSelector (toNSString value)

-- | @- width@
width :: IsDOMHTMLTableCellElement domhtmlTableCellElement => domhtmlTableCellElement -> IO (Id NSString)
width domhtmlTableCellElement =
  sendMessage domhtmlTableCellElement widthSelector

-- | @- setWidth:@
setWidth :: (IsDOMHTMLTableCellElement domhtmlTableCellElement, IsNSString value) => domhtmlTableCellElement -> value -> IO ()
setWidth domhtmlTableCellElement value =
  sendMessage domhtmlTableCellElement setWidthSelector (toNSString value)

-- | @- abbr@
abbr :: IsDOMHTMLTableCellElement domhtmlTableCellElement => domhtmlTableCellElement -> IO (Id NSString)
abbr domhtmlTableCellElement =
  sendMessage domhtmlTableCellElement abbrSelector

-- | @- setAbbr:@
setAbbr :: (IsDOMHTMLTableCellElement domhtmlTableCellElement, IsNSString value) => domhtmlTableCellElement -> value -> IO ()
setAbbr domhtmlTableCellElement value =
  sendMessage domhtmlTableCellElement setAbbrSelector (toNSString value)

-- | @- scope@
scope :: IsDOMHTMLTableCellElement domhtmlTableCellElement => domhtmlTableCellElement -> IO (Id NSString)
scope domhtmlTableCellElement =
  sendMessage domhtmlTableCellElement scopeSelector

-- | @- setScope:@
setScope :: (IsDOMHTMLTableCellElement domhtmlTableCellElement, IsNSString value) => domhtmlTableCellElement -> value -> IO ()
setScope domhtmlTableCellElement value =
  sendMessage domhtmlTableCellElement setScopeSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cellIndex@
cellIndexSelector :: Selector '[] CInt
cellIndexSelector = mkSelector "cellIndex"

-- | @Selector@ for @align@
alignSelector :: Selector '[] (Id NSString)
alignSelector = mkSelector "align"

-- | @Selector@ for @setAlign:@
setAlignSelector :: Selector '[Id NSString] ()
setAlignSelector = mkSelector "setAlign:"

-- | @Selector@ for @axis@
axisSelector :: Selector '[] (Id NSString)
axisSelector = mkSelector "axis"

-- | @Selector@ for @setAxis:@
setAxisSelector :: Selector '[Id NSString] ()
setAxisSelector = mkSelector "setAxis:"

-- | @Selector@ for @bgColor@
bgColorSelector :: Selector '[] (Id NSString)
bgColorSelector = mkSelector "bgColor"

-- | @Selector@ for @setBgColor:@
setBgColorSelector :: Selector '[Id NSString] ()
setBgColorSelector = mkSelector "setBgColor:"

-- | @Selector@ for @ch@
chSelector :: Selector '[] (Id NSString)
chSelector = mkSelector "ch"

-- | @Selector@ for @setCh:@
setChSelector :: Selector '[Id NSString] ()
setChSelector = mkSelector "setCh:"

-- | @Selector@ for @chOff@
chOffSelector :: Selector '[] (Id NSString)
chOffSelector = mkSelector "chOff"

-- | @Selector@ for @setChOff:@
setChOffSelector :: Selector '[Id NSString] ()
setChOffSelector = mkSelector "setChOff:"

-- | @Selector@ for @colSpan@
colSpanSelector :: Selector '[] CInt
colSpanSelector = mkSelector "colSpan"

-- | @Selector@ for @setColSpan:@
setColSpanSelector :: Selector '[CInt] ()
setColSpanSelector = mkSelector "setColSpan:"

-- | @Selector@ for @rowSpan@
rowSpanSelector :: Selector '[] CInt
rowSpanSelector = mkSelector "rowSpan"

-- | @Selector@ for @setRowSpan:@
setRowSpanSelector :: Selector '[CInt] ()
setRowSpanSelector = mkSelector "setRowSpan:"

-- | @Selector@ for @headers@
headersSelector :: Selector '[] (Id NSString)
headersSelector = mkSelector "headers"

-- | @Selector@ for @setHeaders:@
setHeadersSelector :: Selector '[Id NSString] ()
setHeadersSelector = mkSelector "setHeaders:"

-- | @Selector@ for @height@
heightSelector :: Selector '[] (Id NSString)
heightSelector = mkSelector "height"

-- | @Selector@ for @setHeight:@
setHeightSelector :: Selector '[Id NSString] ()
setHeightSelector = mkSelector "setHeight:"

-- | @Selector@ for @noWrap@
noWrapSelector :: Selector '[] Bool
noWrapSelector = mkSelector "noWrap"

-- | @Selector@ for @setNoWrap:@
setNoWrapSelector :: Selector '[Bool] ()
setNoWrapSelector = mkSelector "setNoWrap:"

-- | @Selector@ for @vAlign@
vAlignSelector :: Selector '[] (Id NSString)
vAlignSelector = mkSelector "vAlign"

-- | @Selector@ for @setVAlign:@
setVAlignSelector :: Selector '[Id NSString] ()
setVAlignSelector = mkSelector "setVAlign:"

-- | @Selector@ for @width@
widthSelector :: Selector '[] (Id NSString)
widthSelector = mkSelector "width"

-- | @Selector@ for @setWidth:@
setWidthSelector :: Selector '[Id NSString] ()
setWidthSelector = mkSelector "setWidth:"

-- | @Selector@ for @abbr@
abbrSelector :: Selector '[] (Id NSString)
abbrSelector = mkSelector "abbr"

-- | @Selector@ for @setAbbr:@
setAbbrSelector :: Selector '[Id NSString] ()
setAbbrSelector = mkSelector "setAbbr:"

-- | @Selector@ for @scope@
scopeSelector :: Selector '[] (Id NSString)
scopeSelector = mkSelector "scope"

-- | @Selector@ for @setScope:@
setScopeSelector :: Selector '[Id NSString] ()
setScopeSelector = mkSelector "setScope:"

