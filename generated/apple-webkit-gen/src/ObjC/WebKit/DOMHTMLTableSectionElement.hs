{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLTableSectionElement@.
module ObjC.WebKit.DOMHTMLTableSectionElement
  ( DOMHTMLTableSectionElement
  , IsDOMHTMLTableSectionElement(..)
  , insertRow
  , deleteRow
  , align
  , setAlign
  , ch
  , setCh
  , chOff
  , setChOff
  , vAlign
  , setVAlign
  , rows
  , alignSelector
  , chOffSelector
  , chSelector
  , deleteRowSelector
  , insertRowSelector
  , rowsSelector
  , setAlignSelector
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

-- | @- insertRow:@
insertRow :: IsDOMHTMLTableSectionElement domhtmlTableSectionElement => domhtmlTableSectionElement -> CInt -> IO (Id DOMHTMLElement)
insertRow domhtmlTableSectionElement index =
  sendMessage domhtmlTableSectionElement insertRowSelector index

-- | @- deleteRow:@
deleteRow :: IsDOMHTMLTableSectionElement domhtmlTableSectionElement => domhtmlTableSectionElement -> CInt -> IO ()
deleteRow domhtmlTableSectionElement index =
  sendMessage domhtmlTableSectionElement deleteRowSelector index

-- | @- align@
align :: IsDOMHTMLTableSectionElement domhtmlTableSectionElement => domhtmlTableSectionElement -> IO (Id NSString)
align domhtmlTableSectionElement =
  sendMessage domhtmlTableSectionElement alignSelector

-- | @- setAlign:@
setAlign :: (IsDOMHTMLTableSectionElement domhtmlTableSectionElement, IsNSString value) => domhtmlTableSectionElement -> value -> IO ()
setAlign domhtmlTableSectionElement value =
  sendMessage domhtmlTableSectionElement setAlignSelector (toNSString value)

-- | @- ch@
ch :: IsDOMHTMLTableSectionElement domhtmlTableSectionElement => domhtmlTableSectionElement -> IO (Id NSString)
ch domhtmlTableSectionElement =
  sendMessage domhtmlTableSectionElement chSelector

-- | @- setCh:@
setCh :: (IsDOMHTMLTableSectionElement domhtmlTableSectionElement, IsNSString value) => domhtmlTableSectionElement -> value -> IO ()
setCh domhtmlTableSectionElement value =
  sendMessage domhtmlTableSectionElement setChSelector (toNSString value)

-- | @- chOff@
chOff :: IsDOMHTMLTableSectionElement domhtmlTableSectionElement => domhtmlTableSectionElement -> IO (Id NSString)
chOff domhtmlTableSectionElement =
  sendMessage domhtmlTableSectionElement chOffSelector

-- | @- setChOff:@
setChOff :: (IsDOMHTMLTableSectionElement domhtmlTableSectionElement, IsNSString value) => domhtmlTableSectionElement -> value -> IO ()
setChOff domhtmlTableSectionElement value =
  sendMessage domhtmlTableSectionElement setChOffSelector (toNSString value)

-- | @- vAlign@
vAlign :: IsDOMHTMLTableSectionElement domhtmlTableSectionElement => domhtmlTableSectionElement -> IO (Id NSString)
vAlign domhtmlTableSectionElement =
  sendMessage domhtmlTableSectionElement vAlignSelector

-- | @- setVAlign:@
setVAlign :: (IsDOMHTMLTableSectionElement domhtmlTableSectionElement, IsNSString value) => domhtmlTableSectionElement -> value -> IO ()
setVAlign domhtmlTableSectionElement value =
  sendMessage domhtmlTableSectionElement setVAlignSelector (toNSString value)

-- | @- rows@
rows :: IsDOMHTMLTableSectionElement domhtmlTableSectionElement => domhtmlTableSectionElement -> IO (Id DOMHTMLCollection)
rows domhtmlTableSectionElement =
  sendMessage domhtmlTableSectionElement rowsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @insertRow:@
insertRowSelector :: Selector '[CInt] (Id DOMHTMLElement)
insertRowSelector = mkSelector "insertRow:"

-- | @Selector@ for @deleteRow:@
deleteRowSelector :: Selector '[CInt] ()
deleteRowSelector = mkSelector "deleteRow:"

-- | @Selector@ for @align@
alignSelector :: Selector '[] (Id NSString)
alignSelector = mkSelector "align"

-- | @Selector@ for @setAlign:@
setAlignSelector :: Selector '[Id NSString] ()
setAlignSelector = mkSelector "setAlign:"

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

-- | @Selector@ for @rows@
rowsSelector :: Selector '[] (Id DOMHTMLCollection)
rowsSelector = mkSelector "rows"

