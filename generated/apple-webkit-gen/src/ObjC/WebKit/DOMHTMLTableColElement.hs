{-# LANGUAGE DataKinds #-}
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
  , chOffSelector
  , chSelector
  , setAlignSelector
  , setChOffSelector
  , setChSelector
  , setSpanSelector
  , setVAlignSelector
  , setWidthSelector
  , spanSelector
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

-- | @- align@
align :: IsDOMHTMLTableColElement domhtmlTableColElement => domhtmlTableColElement -> IO (Id NSString)
align domhtmlTableColElement =
  sendMessage domhtmlTableColElement alignSelector

-- | @- setAlign:@
setAlign :: (IsDOMHTMLTableColElement domhtmlTableColElement, IsNSString value) => domhtmlTableColElement -> value -> IO ()
setAlign domhtmlTableColElement value =
  sendMessage domhtmlTableColElement setAlignSelector (toNSString value)

-- | @- ch@
ch :: IsDOMHTMLTableColElement domhtmlTableColElement => domhtmlTableColElement -> IO (Id NSString)
ch domhtmlTableColElement =
  sendMessage domhtmlTableColElement chSelector

-- | @- setCh:@
setCh :: (IsDOMHTMLTableColElement domhtmlTableColElement, IsNSString value) => domhtmlTableColElement -> value -> IO ()
setCh domhtmlTableColElement value =
  sendMessage domhtmlTableColElement setChSelector (toNSString value)

-- | @- chOff@
chOff :: IsDOMHTMLTableColElement domhtmlTableColElement => domhtmlTableColElement -> IO (Id NSString)
chOff domhtmlTableColElement =
  sendMessage domhtmlTableColElement chOffSelector

-- | @- setChOff:@
setChOff :: (IsDOMHTMLTableColElement domhtmlTableColElement, IsNSString value) => domhtmlTableColElement -> value -> IO ()
setChOff domhtmlTableColElement value =
  sendMessage domhtmlTableColElement setChOffSelector (toNSString value)

-- | @- span@
span :: IsDOMHTMLTableColElement domhtmlTableColElement => domhtmlTableColElement -> IO CInt
span domhtmlTableColElement =
  sendMessage domhtmlTableColElement spanSelector

-- | @- setSpan:@
setSpan :: IsDOMHTMLTableColElement domhtmlTableColElement => domhtmlTableColElement -> CInt -> IO ()
setSpan domhtmlTableColElement value =
  sendMessage domhtmlTableColElement setSpanSelector value

-- | @- vAlign@
vAlign :: IsDOMHTMLTableColElement domhtmlTableColElement => domhtmlTableColElement -> IO (Id NSString)
vAlign domhtmlTableColElement =
  sendMessage domhtmlTableColElement vAlignSelector

-- | @- setVAlign:@
setVAlign :: (IsDOMHTMLTableColElement domhtmlTableColElement, IsNSString value) => domhtmlTableColElement -> value -> IO ()
setVAlign domhtmlTableColElement value =
  sendMessage domhtmlTableColElement setVAlignSelector (toNSString value)

-- | @- width@
width :: IsDOMHTMLTableColElement domhtmlTableColElement => domhtmlTableColElement -> IO (Id NSString)
width domhtmlTableColElement =
  sendMessage domhtmlTableColElement widthSelector

-- | @- setWidth:@
setWidth :: (IsDOMHTMLTableColElement domhtmlTableColElement, IsNSString value) => domhtmlTableColElement -> value -> IO ()
setWidth domhtmlTableColElement value =
  sendMessage domhtmlTableColElement setWidthSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

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

-- | @Selector@ for @span@
spanSelector :: Selector '[] CInt
spanSelector = mkSelector "span"

-- | @Selector@ for @setSpan:@
setSpanSelector :: Selector '[CInt] ()
setSpanSelector = mkSelector "setSpan:"

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

