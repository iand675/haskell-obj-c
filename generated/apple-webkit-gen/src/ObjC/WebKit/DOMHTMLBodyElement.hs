{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLBodyElement@.
module ObjC.WebKit.DOMHTMLBodyElement
  ( DOMHTMLBodyElement
  , IsDOMHTMLBodyElement(..)
  , aLink
  , setALink
  , background
  , setBackground
  , bgColor
  , setBgColor
  , link
  , setLink
  , text
  , setText
  , vLink
  , setVLink
  , aLinkSelector
  , backgroundSelector
  , bgColorSelector
  , linkSelector
  , setALinkSelector
  , setBackgroundSelector
  , setBgColorSelector
  , setLinkSelector
  , setTextSelector
  , setVLinkSelector
  , textSelector
  , vLinkSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- aLink@
aLink :: IsDOMHTMLBodyElement domhtmlBodyElement => domhtmlBodyElement -> IO (Id NSString)
aLink domhtmlBodyElement =
  sendMessage domhtmlBodyElement aLinkSelector

-- | @- setALink:@
setALink :: (IsDOMHTMLBodyElement domhtmlBodyElement, IsNSString value) => domhtmlBodyElement -> value -> IO ()
setALink domhtmlBodyElement value =
  sendMessage domhtmlBodyElement setALinkSelector (toNSString value)

-- | @- background@
background :: IsDOMHTMLBodyElement domhtmlBodyElement => domhtmlBodyElement -> IO (Id NSString)
background domhtmlBodyElement =
  sendMessage domhtmlBodyElement backgroundSelector

-- | @- setBackground:@
setBackground :: (IsDOMHTMLBodyElement domhtmlBodyElement, IsNSString value) => domhtmlBodyElement -> value -> IO ()
setBackground domhtmlBodyElement value =
  sendMessage domhtmlBodyElement setBackgroundSelector (toNSString value)

-- | @- bgColor@
bgColor :: IsDOMHTMLBodyElement domhtmlBodyElement => domhtmlBodyElement -> IO (Id NSString)
bgColor domhtmlBodyElement =
  sendMessage domhtmlBodyElement bgColorSelector

-- | @- setBgColor:@
setBgColor :: (IsDOMHTMLBodyElement domhtmlBodyElement, IsNSString value) => domhtmlBodyElement -> value -> IO ()
setBgColor domhtmlBodyElement value =
  sendMessage domhtmlBodyElement setBgColorSelector (toNSString value)

-- | @- link@
link :: IsDOMHTMLBodyElement domhtmlBodyElement => domhtmlBodyElement -> IO (Id NSString)
link domhtmlBodyElement =
  sendMessage domhtmlBodyElement linkSelector

-- | @- setLink:@
setLink :: (IsDOMHTMLBodyElement domhtmlBodyElement, IsNSString value) => domhtmlBodyElement -> value -> IO ()
setLink domhtmlBodyElement value =
  sendMessage domhtmlBodyElement setLinkSelector (toNSString value)

-- | @- text@
text :: IsDOMHTMLBodyElement domhtmlBodyElement => domhtmlBodyElement -> IO (Id NSString)
text domhtmlBodyElement =
  sendMessage domhtmlBodyElement textSelector

-- | @- setText:@
setText :: (IsDOMHTMLBodyElement domhtmlBodyElement, IsNSString value) => domhtmlBodyElement -> value -> IO ()
setText domhtmlBodyElement value =
  sendMessage domhtmlBodyElement setTextSelector (toNSString value)

-- | @- vLink@
vLink :: IsDOMHTMLBodyElement domhtmlBodyElement => domhtmlBodyElement -> IO (Id NSString)
vLink domhtmlBodyElement =
  sendMessage domhtmlBodyElement vLinkSelector

-- | @- setVLink:@
setVLink :: (IsDOMHTMLBodyElement domhtmlBodyElement, IsNSString value) => domhtmlBodyElement -> value -> IO ()
setVLink domhtmlBodyElement value =
  sendMessage domhtmlBodyElement setVLinkSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @aLink@
aLinkSelector :: Selector '[] (Id NSString)
aLinkSelector = mkSelector "aLink"

-- | @Selector@ for @setALink:@
setALinkSelector :: Selector '[Id NSString] ()
setALinkSelector = mkSelector "setALink:"

-- | @Selector@ for @background@
backgroundSelector :: Selector '[] (Id NSString)
backgroundSelector = mkSelector "background"

-- | @Selector@ for @setBackground:@
setBackgroundSelector :: Selector '[Id NSString] ()
setBackgroundSelector = mkSelector "setBackground:"

-- | @Selector@ for @bgColor@
bgColorSelector :: Selector '[] (Id NSString)
bgColorSelector = mkSelector "bgColor"

-- | @Selector@ for @setBgColor:@
setBgColorSelector :: Selector '[Id NSString] ()
setBgColorSelector = mkSelector "setBgColor:"

-- | @Selector@ for @link@
linkSelector :: Selector '[] (Id NSString)
linkSelector = mkSelector "link"

-- | @Selector@ for @setLink:@
setLinkSelector :: Selector '[Id NSString] ()
setLinkSelector = mkSelector "setLink:"

-- | @Selector@ for @text@
textSelector :: Selector '[] (Id NSString)
textSelector = mkSelector "text"

-- | @Selector@ for @setText:@
setTextSelector :: Selector '[Id NSString] ()
setTextSelector = mkSelector "setText:"

-- | @Selector@ for @vLink@
vLinkSelector :: Selector '[] (Id NSString)
vLinkSelector = mkSelector "vLink"

-- | @Selector@ for @setVLink:@
setVLinkSelector :: Selector '[Id NSString] ()
setVLinkSelector = mkSelector "setVLink:"

