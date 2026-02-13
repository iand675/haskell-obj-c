{-# LANGUAGE DataKinds #-}
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
  , alignSelector
  , bgColorSelector
  , cellsSelector
  , chOffSelector
  , chSelector
  , deleteCellSelector
  , insertCellSelector
  , rowIndexSelector
  , sectionRowIndexSelector
  , setAlignSelector
  , setBgColorSelector
  , setChOffSelector
  , setChSelector
  , setVAlignSelector
  , vAlignSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- insertCell:@
insertCell :: IsDOMHTMLTableRowElement domhtmlTableRowElement => domhtmlTableRowElement -> CInt -> IO (Id DOMHTMLElement)
insertCell domhtmlTableRowElement index =
  sendMessage domhtmlTableRowElement insertCellSelector index

-- | @- deleteCell:@
deleteCell :: IsDOMHTMLTableRowElement domhtmlTableRowElement => domhtmlTableRowElement -> CInt -> IO ()
deleteCell domhtmlTableRowElement index =
  sendMessage domhtmlTableRowElement deleteCellSelector index

-- | @- rowIndex@
rowIndex :: IsDOMHTMLTableRowElement domhtmlTableRowElement => domhtmlTableRowElement -> IO CInt
rowIndex domhtmlTableRowElement =
  sendMessage domhtmlTableRowElement rowIndexSelector

-- | @- sectionRowIndex@
sectionRowIndex :: IsDOMHTMLTableRowElement domhtmlTableRowElement => domhtmlTableRowElement -> IO CInt
sectionRowIndex domhtmlTableRowElement =
  sendMessage domhtmlTableRowElement sectionRowIndexSelector

-- | @- cells@
cells :: IsDOMHTMLTableRowElement domhtmlTableRowElement => domhtmlTableRowElement -> IO (Id DOMHTMLCollection)
cells domhtmlTableRowElement =
  sendMessage domhtmlTableRowElement cellsSelector

-- | @- align@
align :: IsDOMHTMLTableRowElement domhtmlTableRowElement => domhtmlTableRowElement -> IO (Id NSString)
align domhtmlTableRowElement =
  sendMessage domhtmlTableRowElement alignSelector

-- | @- setAlign:@
setAlign :: (IsDOMHTMLTableRowElement domhtmlTableRowElement, IsNSString value) => domhtmlTableRowElement -> value -> IO ()
setAlign domhtmlTableRowElement value =
  sendMessage domhtmlTableRowElement setAlignSelector (toNSString value)

-- | @- bgColor@
bgColor :: IsDOMHTMLTableRowElement domhtmlTableRowElement => domhtmlTableRowElement -> IO (Id NSString)
bgColor domhtmlTableRowElement =
  sendMessage domhtmlTableRowElement bgColorSelector

-- | @- setBgColor:@
setBgColor :: (IsDOMHTMLTableRowElement domhtmlTableRowElement, IsNSString value) => domhtmlTableRowElement -> value -> IO ()
setBgColor domhtmlTableRowElement value =
  sendMessage domhtmlTableRowElement setBgColorSelector (toNSString value)

-- | @- ch@
ch :: IsDOMHTMLTableRowElement domhtmlTableRowElement => domhtmlTableRowElement -> IO (Id NSString)
ch domhtmlTableRowElement =
  sendMessage domhtmlTableRowElement chSelector

-- | @- setCh:@
setCh :: (IsDOMHTMLTableRowElement domhtmlTableRowElement, IsNSString value) => domhtmlTableRowElement -> value -> IO ()
setCh domhtmlTableRowElement value =
  sendMessage domhtmlTableRowElement setChSelector (toNSString value)

-- | @- chOff@
chOff :: IsDOMHTMLTableRowElement domhtmlTableRowElement => domhtmlTableRowElement -> IO (Id NSString)
chOff domhtmlTableRowElement =
  sendMessage domhtmlTableRowElement chOffSelector

-- | @- setChOff:@
setChOff :: (IsDOMHTMLTableRowElement domhtmlTableRowElement, IsNSString value) => domhtmlTableRowElement -> value -> IO ()
setChOff domhtmlTableRowElement value =
  sendMessage domhtmlTableRowElement setChOffSelector (toNSString value)

-- | @- vAlign@
vAlign :: IsDOMHTMLTableRowElement domhtmlTableRowElement => domhtmlTableRowElement -> IO (Id NSString)
vAlign domhtmlTableRowElement =
  sendMessage domhtmlTableRowElement vAlignSelector

-- | @- setVAlign:@
setVAlign :: (IsDOMHTMLTableRowElement domhtmlTableRowElement, IsNSString value) => domhtmlTableRowElement -> value -> IO ()
setVAlign domhtmlTableRowElement value =
  sendMessage domhtmlTableRowElement setVAlignSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @insertCell:@
insertCellSelector :: Selector '[CInt] (Id DOMHTMLElement)
insertCellSelector = mkSelector "insertCell:"

-- | @Selector@ for @deleteCell:@
deleteCellSelector :: Selector '[CInt] ()
deleteCellSelector = mkSelector "deleteCell:"

-- | @Selector@ for @rowIndex@
rowIndexSelector :: Selector '[] CInt
rowIndexSelector = mkSelector "rowIndex"

-- | @Selector@ for @sectionRowIndex@
sectionRowIndexSelector :: Selector '[] CInt
sectionRowIndexSelector = mkSelector "sectionRowIndex"

-- | @Selector@ for @cells@
cellsSelector :: Selector '[] (Id DOMHTMLCollection)
cellsSelector = mkSelector "cells"

-- | @Selector@ for @align@
alignSelector :: Selector '[] (Id NSString)
alignSelector = mkSelector "align"

-- | @Selector@ for @setAlign:@
setAlignSelector :: Selector '[Id NSString] ()
setAlignSelector = mkSelector "setAlign:"

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

-- | @Selector@ for @vAlign@
vAlignSelector :: Selector '[] (Id NSString)
vAlignSelector = mkSelector "vAlign"

-- | @Selector@ for @setVAlign:@
setVAlignSelector :: Selector '[Id NSString] ()
setVAlignSelector = mkSelector "setVAlign:"

