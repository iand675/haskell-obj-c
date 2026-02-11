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
  , insertRowSelector
  , deleteRowSelector
  , alignSelector
  , setAlignSelector
  , chSelector
  , setChSelector
  , chOffSelector
  , setChOffSelector
  , vAlignSelector
  , setVAlignSelector
  , rowsSelector


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

-- | @- insertRow:@
insertRow :: IsDOMHTMLTableSectionElement domhtmlTableSectionElement => domhtmlTableSectionElement -> CInt -> IO (Id DOMHTMLElement)
insertRow domhtmlTableSectionElement  index =
  sendMsg domhtmlTableSectionElement (mkSelector "insertRow:") (retPtr retVoid) [argCInt (fromIntegral index)] >>= retainedObject . castPtr

-- | @- deleteRow:@
deleteRow :: IsDOMHTMLTableSectionElement domhtmlTableSectionElement => domhtmlTableSectionElement -> CInt -> IO ()
deleteRow domhtmlTableSectionElement  index =
  sendMsg domhtmlTableSectionElement (mkSelector "deleteRow:") retVoid [argCInt (fromIntegral index)]

-- | @- align@
align :: IsDOMHTMLTableSectionElement domhtmlTableSectionElement => domhtmlTableSectionElement -> IO (Id NSString)
align domhtmlTableSectionElement  =
  sendMsg domhtmlTableSectionElement (mkSelector "align") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAlign:@
setAlign :: (IsDOMHTMLTableSectionElement domhtmlTableSectionElement, IsNSString value) => domhtmlTableSectionElement -> value -> IO ()
setAlign domhtmlTableSectionElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlTableSectionElement (mkSelector "setAlign:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- ch@
ch :: IsDOMHTMLTableSectionElement domhtmlTableSectionElement => domhtmlTableSectionElement -> IO (Id NSString)
ch domhtmlTableSectionElement  =
  sendMsg domhtmlTableSectionElement (mkSelector "ch") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCh:@
setCh :: (IsDOMHTMLTableSectionElement domhtmlTableSectionElement, IsNSString value) => domhtmlTableSectionElement -> value -> IO ()
setCh domhtmlTableSectionElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlTableSectionElement (mkSelector "setCh:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- chOff@
chOff :: IsDOMHTMLTableSectionElement domhtmlTableSectionElement => domhtmlTableSectionElement -> IO (Id NSString)
chOff domhtmlTableSectionElement  =
  sendMsg domhtmlTableSectionElement (mkSelector "chOff") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setChOff:@
setChOff :: (IsDOMHTMLTableSectionElement domhtmlTableSectionElement, IsNSString value) => domhtmlTableSectionElement -> value -> IO ()
setChOff domhtmlTableSectionElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlTableSectionElement (mkSelector "setChOff:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- vAlign@
vAlign :: IsDOMHTMLTableSectionElement domhtmlTableSectionElement => domhtmlTableSectionElement -> IO (Id NSString)
vAlign domhtmlTableSectionElement  =
  sendMsg domhtmlTableSectionElement (mkSelector "vAlign") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVAlign:@
setVAlign :: (IsDOMHTMLTableSectionElement domhtmlTableSectionElement, IsNSString value) => domhtmlTableSectionElement -> value -> IO ()
setVAlign domhtmlTableSectionElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlTableSectionElement (mkSelector "setVAlign:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- rows@
rows :: IsDOMHTMLTableSectionElement domhtmlTableSectionElement => domhtmlTableSectionElement -> IO (Id DOMHTMLCollection)
rows domhtmlTableSectionElement  =
  sendMsg domhtmlTableSectionElement (mkSelector "rows") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @insertRow:@
insertRowSelector :: Selector
insertRowSelector = mkSelector "insertRow:"

-- | @Selector@ for @deleteRow:@
deleteRowSelector :: Selector
deleteRowSelector = mkSelector "deleteRow:"

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

-- | @Selector@ for @vAlign@
vAlignSelector :: Selector
vAlignSelector = mkSelector "vAlign"

-- | @Selector@ for @setVAlign:@
setVAlignSelector :: Selector
setVAlignSelector = mkSelector "setVAlign:"

-- | @Selector@ for @rows@
rowsSelector :: Selector
rowsSelector = mkSelector "rows"

