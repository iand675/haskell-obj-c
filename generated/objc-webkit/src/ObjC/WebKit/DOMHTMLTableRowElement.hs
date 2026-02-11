{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLTableRowElement@.
module ObjC.WebKit.DOMHTMLTableRowElement
  ( DOMHTMLTableRowElement
  , IsDOMHTMLTableRowElement(..)
  , insertCell
  , deleteCell
  , rowIndex
  , sectionRowIndex
  , cells
  , align
  , setAlign
  , bgColor
  , setBgColor
  , ch
  , setCh
  , chOff
  , setChOff
  , vAlign
  , setVAlign
  , insertCellSelector
  , deleteCellSelector
  , rowIndexSelector
  , sectionRowIndexSelector
  , cellsSelector
  , alignSelector
  , setAlignSelector
  , bgColorSelector
  , setBgColorSelector
  , chSelector
  , setChSelector
  , chOffSelector
  , setChOffSelector
  , vAlignSelector
  , setVAlignSelector


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

-- | @- insertCell:@
insertCell :: IsDOMHTMLTableRowElement domhtmlTableRowElement => domhtmlTableRowElement -> CInt -> IO (Id DOMHTMLElement)
insertCell domhtmlTableRowElement  index =
  sendMsg domhtmlTableRowElement (mkSelector "insertCell:") (retPtr retVoid) [argCInt (fromIntegral index)] >>= retainedObject . castPtr

-- | @- deleteCell:@
deleteCell :: IsDOMHTMLTableRowElement domhtmlTableRowElement => domhtmlTableRowElement -> CInt -> IO ()
deleteCell domhtmlTableRowElement  index =
  sendMsg domhtmlTableRowElement (mkSelector "deleteCell:") retVoid [argCInt (fromIntegral index)]

-- | @- rowIndex@
rowIndex :: IsDOMHTMLTableRowElement domhtmlTableRowElement => domhtmlTableRowElement -> IO CInt
rowIndex domhtmlTableRowElement  =
  sendMsg domhtmlTableRowElement (mkSelector "rowIndex") retCInt []

-- | @- sectionRowIndex@
sectionRowIndex :: IsDOMHTMLTableRowElement domhtmlTableRowElement => domhtmlTableRowElement -> IO CInt
sectionRowIndex domhtmlTableRowElement  =
  sendMsg domhtmlTableRowElement (mkSelector "sectionRowIndex") retCInt []

-- | @- cells@
cells :: IsDOMHTMLTableRowElement domhtmlTableRowElement => domhtmlTableRowElement -> IO (Id DOMHTMLCollection)
cells domhtmlTableRowElement  =
  sendMsg domhtmlTableRowElement (mkSelector "cells") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- align@
align :: IsDOMHTMLTableRowElement domhtmlTableRowElement => domhtmlTableRowElement -> IO (Id NSString)
align domhtmlTableRowElement  =
  sendMsg domhtmlTableRowElement (mkSelector "align") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAlign:@
setAlign :: (IsDOMHTMLTableRowElement domhtmlTableRowElement, IsNSString value) => domhtmlTableRowElement -> value -> IO ()
setAlign domhtmlTableRowElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlTableRowElement (mkSelector "setAlign:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- bgColor@
bgColor :: IsDOMHTMLTableRowElement domhtmlTableRowElement => domhtmlTableRowElement -> IO (Id NSString)
bgColor domhtmlTableRowElement  =
  sendMsg domhtmlTableRowElement (mkSelector "bgColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBgColor:@
setBgColor :: (IsDOMHTMLTableRowElement domhtmlTableRowElement, IsNSString value) => domhtmlTableRowElement -> value -> IO ()
setBgColor domhtmlTableRowElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlTableRowElement (mkSelector "setBgColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- ch@
ch :: IsDOMHTMLTableRowElement domhtmlTableRowElement => domhtmlTableRowElement -> IO (Id NSString)
ch domhtmlTableRowElement  =
  sendMsg domhtmlTableRowElement (mkSelector "ch") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCh:@
setCh :: (IsDOMHTMLTableRowElement domhtmlTableRowElement, IsNSString value) => domhtmlTableRowElement -> value -> IO ()
setCh domhtmlTableRowElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlTableRowElement (mkSelector "setCh:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- chOff@
chOff :: IsDOMHTMLTableRowElement domhtmlTableRowElement => domhtmlTableRowElement -> IO (Id NSString)
chOff domhtmlTableRowElement  =
  sendMsg domhtmlTableRowElement (mkSelector "chOff") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setChOff:@
setChOff :: (IsDOMHTMLTableRowElement domhtmlTableRowElement, IsNSString value) => domhtmlTableRowElement -> value -> IO ()
setChOff domhtmlTableRowElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlTableRowElement (mkSelector "setChOff:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- vAlign@
vAlign :: IsDOMHTMLTableRowElement domhtmlTableRowElement => domhtmlTableRowElement -> IO (Id NSString)
vAlign domhtmlTableRowElement  =
  sendMsg domhtmlTableRowElement (mkSelector "vAlign") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVAlign:@
setVAlign :: (IsDOMHTMLTableRowElement domhtmlTableRowElement, IsNSString value) => domhtmlTableRowElement -> value -> IO ()
setVAlign domhtmlTableRowElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlTableRowElement (mkSelector "setVAlign:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @insertCell:@
insertCellSelector :: Selector
insertCellSelector = mkSelector "insertCell:"

-- | @Selector@ for @deleteCell:@
deleteCellSelector :: Selector
deleteCellSelector = mkSelector "deleteCell:"

-- | @Selector@ for @rowIndex@
rowIndexSelector :: Selector
rowIndexSelector = mkSelector "rowIndex"

-- | @Selector@ for @sectionRowIndex@
sectionRowIndexSelector :: Selector
sectionRowIndexSelector = mkSelector "sectionRowIndex"

-- | @Selector@ for @cells@
cellsSelector :: Selector
cellsSelector = mkSelector "cells"

-- | @Selector@ for @align@
alignSelector :: Selector
alignSelector = mkSelector "align"

-- | @Selector@ for @setAlign:@
setAlignSelector :: Selector
setAlignSelector = mkSelector "setAlign:"

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

-- | @Selector@ for @vAlign@
vAlignSelector :: Selector
vAlignSelector = mkSelector "vAlign"

-- | @Selector@ for @setVAlign:@
setVAlignSelector :: Selector
setVAlignSelector = mkSelector "setVAlign:"

