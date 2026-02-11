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
  , setALinkSelector
  , backgroundSelector
  , setBackgroundSelector
  , bgColorSelector
  , setBgColorSelector
  , linkSelector
  , setLinkSelector
  , textSelector
  , setTextSelector
  , vLinkSelector
  , setVLinkSelector


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

-- | @- aLink@
aLink :: IsDOMHTMLBodyElement domhtmlBodyElement => domhtmlBodyElement -> IO (Id NSString)
aLink domhtmlBodyElement  =
  sendMsg domhtmlBodyElement (mkSelector "aLink") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setALink:@
setALink :: (IsDOMHTMLBodyElement domhtmlBodyElement, IsNSString value) => domhtmlBodyElement -> value -> IO ()
setALink domhtmlBodyElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlBodyElement (mkSelector "setALink:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- background@
background :: IsDOMHTMLBodyElement domhtmlBodyElement => domhtmlBodyElement -> IO (Id NSString)
background domhtmlBodyElement  =
  sendMsg domhtmlBodyElement (mkSelector "background") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBackground:@
setBackground :: (IsDOMHTMLBodyElement domhtmlBodyElement, IsNSString value) => domhtmlBodyElement -> value -> IO ()
setBackground domhtmlBodyElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlBodyElement (mkSelector "setBackground:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- bgColor@
bgColor :: IsDOMHTMLBodyElement domhtmlBodyElement => domhtmlBodyElement -> IO (Id NSString)
bgColor domhtmlBodyElement  =
  sendMsg domhtmlBodyElement (mkSelector "bgColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBgColor:@
setBgColor :: (IsDOMHTMLBodyElement domhtmlBodyElement, IsNSString value) => domhtmlBodyElement -> value -> IO ()
setBgColor domhtmlBodyElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlBodyElement (mkSelector "setBgColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- link@
link :: IsDOMHTMLBodyElement domhtmlBodyElement => domhtmlBodyElement -> IO (Id NSString)
link domhtmlBodyElement  =
  sendMsg domhtmlBodyElement (mkSelector "link") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLink:@
setLink :: (IsDOMHTMLBodyElement domhtmlBodyElement, IsNSString value) => domhtmlBodyElement -> value -> IO ()
setLink domhtmlBodyElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlBodyElement (mkSelector "setLink:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- text@
text :: IsDOMHTMLBodyElement domhtmlBodyElement => domhtmlBodyElement -> IO (Id NSString)
text domhtmlBodyElement  =
  sendMsg domhtmlBodyElement (mkSelector "text") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setText:@
setText :: (IsDOMHTMLBodyElement domhtmlBodyElement, IsNSString value) => domhtmlBodyElement -> value -> IO ()
setText domhtmlBodyElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlBodyElement (mkSelector "setText:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- vLink@
vLink :: IsDOMHTMLBodyElement domhtmlBodyElement => domhtmlBodyElement -> IO (Id NSString)
vLink domhtmlBodyElement  =
  sendMsg domhtmlBodyElement (mkSelector "vLink") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVLink:@
setVLink :: (IsDOMHTMLBodyElement domhtmlBodyElement, IsNSString value) => domhtmlBodyElement -> value -> IO ()
setVLink domhtmlBodyElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlBodyElement (mkSelector "setVLink:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @aLink@
aLinkSelector :: Selector
aLinkSelector = mkSelector "aLink"

-- | @Selector@ for @setALink:@
setALinkSelector :: Selector
setALinkSelector = mkSelector "setALink:"

-- | @Selector@ for @background@
backgroundSelector :: Selector
backgroundSelector = mkSelector "background"

-- | @Selector@ for @setBackground:@
setBackgroundSelector :: Selector
setBackgroundSelector = mkSelector "setBackground:"

-- | @Selector@ for @bgColor@
bgColorSelector :: Selector
bgColorSelector = mkSelector "bgColor"

-- | @Selector@ for @setBgColor:@
setBgColorSelector :: Selector
setBgColorSelector = mkSelector "setBgColor:"

-- | @Selector@ for @link@
linkSelector :: Selector
linkSelector = mkSelector "link"

-- | @Selector@ for @setLink:@
setLinkSelector :: Selector
setLinkSelector = mkSelector "setLink:"

-- | @Selector@ for @text@
textSelector :: Selector
textSelector = mkSelector "text"

-- | @Selector@ for @setText:@
setTextSelector :: Selector
setTextSelector = mkSelector "setText:"

-- | @Selector@ for @vLink@
vLinkSelector :: Selector
vLinkSelector = mkSelector "vLink"

-- | @Selector@ for @setVLink:@
setVLinkSelector :: Selector
setVLinkSelector = mkSelector "setVLink:"

