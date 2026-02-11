{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLLinkElement@.
module ObjC.WebKit.DOMHTMLLinkElement
  ( DOMHTMLLinkElement
  , IsDOMHTMLLinkElement(..)
  , disabled
  , setDisabled
  , charset
  , setCharset
  , href
  , setHref
  , hreflang
  , setHreflang
  , media
  , setMedia
  , rel
  , setRel
  , rev
  , setRev
  , target
  , setTarget
  , type_
  , setType
  , sheet
  , absoluteLinkURL
  , disabledSelector
  , setDisabledSelector
  , charsetSelector
  , setCharsetSelector
  , hrefSelector
  , setHrefSelector
  , hreflangSelector
  , setHreflangSelector
  , mediaSelector
  , setMediaSelector
  , relSelector
  , setRelSelector
  , revSelector
  , setRevSelector
  , targetSelector
  , setTargetSelector
  , typeSelector
  , setTypeSelector
  , sheetSelector
  , absoluteLinkURLSelector


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

-- | @- disabled@
disabled :: IsDOMHTMLLinkElement domhtmlLinkElement => domhtmlLinkElement -> IO Bool
disabled domhtmlLinkElement  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domhtmlLinkElement (mkSelector "disabled") retCULong []

-- | @- setDisabled:@
setDisabled :: IsDOMHTMLLinkElement domhtmlLinkElement => domhtmlLinkElement -> Bool -> IO ()
setDisabled domhtmlLinkElement  value =
  sendMsg domhtmlLinkElement (mkSelector "setDisabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- charset@
charset :: IsDOMHTMLLinkElement domhtmlLinkElement => domhtmlLinkElement -> IO (Id NSString)
charset domhtmlLinkElement  =
  sendMsg domhtmlLinkElement (mkSelector "charset") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCharset:@
setCharset :: (IsDOMHTMLLinkElement domhtmlLinkElement, IsNSString value) => domhtmlLinkElement -> value -> IO ()
setCharset domhtmlLinkElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlLinkElement (mkSelector "setCharset:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- href@
href :: IsDOMHTMLLinkElement domhtmlLinkElement => domhtmlLinkElement -> IO (Id NSString)
href domhtmlLinkElement  =
  sendMsg domhtmlLinkElement (mkSelector "href") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setHref:@
setHref :: (IsDOMHTMLLinkElement domhtmlLinkElement, IsNSString value) => domhtmlLinkElement -> value -> IO ()
setHref domhtmlLinkElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlLinkElement (mkSelector "setHref:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- hreflang@
hreflang :: IsDOMHTMLLinkElement domhtmlLinkElement => domhtmlLinkElement -> IO (Id NSString)
hreflang domhtmlLinkElement  =
  sendMsg domhtmlLinkElement (mkSelector "hreflang") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setHreflang:@
setHreflang :: (IsDOMHTMLLinkElement domhtmlLinkElement, IsNSString value) => domhtmlLinkElement -> value -> IO ()
setHreflang domhtmlLinkElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlLinkElement (mkSelector "setHreflang:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- media@
media :: IsDOMHTMLLinkElement domhtmlLinkElement => domhtmlLinkElement -> IO (Id NSString)
media domhtmlLinkElement  =
  sendMsg domhtmlLinkElement (mkSelector "media") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMedia:@
setMedia :: (IsDOMHTMLLinkElement domhtmlLinkElement, IsNSString value) => domhtmlLinkElement -> value -> IO ()
setMedia domhtmlLinkElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlLinkElement (mkSelector "setMedia:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- rel@
rel :: IsDOMHTMLLinkElement domhtmlLinkElement => domhtmlLinkElement -> IO (Id NSString)
rel domhtmlLinkElement  =
  sendMsg domhtmlLinkElement (mkSelector "rel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRel:@
setRel :: (IsDOMHTMLLinkElement domhtmlLinkElement, IsNSString value) => domhtmlLinkElement -> value -> IO ()
setRel domhtmlLinkElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlLinkElement (mkSelector "setRel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- rev@
rev :: IsDOMHTMLLinkElement domhtmlLinkElement => domhtmlLinkElement -> IO (Id NSString)
rev domhtmlLinkElement  =
  sendMsg domhtmlLinkElement (mkSelector "rev") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRev:@
setRev :: (IsDOMHTMLLinkElement domhtmlLinkElement, IsNSString value) => domhtmlLinkElement -> value -> IO ()
setRev domhtmlLinkElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlLinkElement (mkSelector "setRev:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- target@
target :: IsDOMHTMLLinkElement domhtmlLinkElement => domhtmlLinkElement -> IO (Id NSString)
target domhtmlLinkElement  =
  sendMsg domhtmlLinkElement (mkSelector "target") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTarget:@
setTarget :: (IsDOMHTMLLinkElement domhtmlLinkElement, IsNSString value) => domhtmlLinkElement -> value -> IO ()
setTarget domhtmlLinkElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlLinkElement (mkSelector "setTarget:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- type@
type_ :: IsDOMHTMLLinkElement domhtmlLinkElement => domhtmlLinkElement -> IO (Id NSString)
type_ domhtmlLinkElement  =
  sendMsg domhtmlLinkElement (mkSelector "type") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setType:@
setType :: (IsDOMHTMLLinkElement domhtmlLinkElement, IsNSString value) => domhtmlLinkElement -> value -> IO ()
setType domhtmlLinkElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlLinkElement (mkSelector "setType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- sheet@
sheet :: IsDOMHTMLLinkElement domhtmlLinkElement => domhtmlLinkElement -> IO (Id DOMStyleSheet)
sheet domhtmlLinkElement  =
  sendMsg domhtmlLinkElement (mkSelector "sheet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- absoluteLinkURL@
absoluteLinkURL :: IsDOMHTMLLinkElement domhtmlLinkElement => domhtmlLinkElement -> IO (Id NSURL)
absoluteLinkURL domhtmlLinkElement  =
  sendMsg domhtmlLinkElement (mkSelector "absoluteLinkURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @disabled@
disabledSelector :: Selector
disabledSelector = mkSelector "disabled"

-- | @Selector@ for @setDisabled:@
setDisabledSelector :: Selector
setDisabledSelector = mkSelector "setDisabled:"

-- | @Selector@ for @charset@
charsetSelector :: Selector
charsetSelector = mkSelector "charset"

-- | @Selector@ for @setCharset:@
setCharsetSelector :: Selector
setCharsetSelector = mkSelector "setCharset:"

-- | @Selector@ for @href@
hrefSelector :: Selector
hrefSelector = mkSelector "href"

-- | @Selector@ for @setHref:@
setHrefSelector :: Selector
setHrefSelector = mkSelector "setHref:"

-- | @Selector@ for @hreflang@
hreflangSelector :: Selector
hreflangSelector = mkSelector "hreflang"

-- | @Selector@ for @setHreflang:@
setHreflangSelector :: Selector
setHreflangSelector = mkSelector "setHreflang:"

-- | @Selector@ for @media@
mediaSelector :: Selector
mediaSelector = mkSelector "media"

-- | @Selector@ for @setMedia:@
setMediaSelector :: Selector
setMediaSelector = mkSelector "setMedia:"

-- | @Selector@ for @rel@
relSelector :: Selector
relSelector = mkSelector "rel"

-- | @Selector@ for @setRel:@
setRelSelector :: Selector
setRelSelector = mkSelector "setRel:"

-- | @Selector@ for @rev@
revSelector :: Selector
revSelector = mkSelector "rev"

-- | @Selector@ for @setRev:@
setRevSelector :: Selector
setRevSelector = mkSelector "setRev:"

-- | @Selector@ for @target@
targetSelector :: Selector
targetSelector = mkSelector "target"

-- | @Selector@ for @setTarget:@
setTargetSelector :: Selector
setTargetSelector = mkSelector "setTarget:"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector
setTypeSelector = mkSelector "setType:"

-- | @Selector@ for @sheet@
sheetSelector :: Selector
sheetSelector = mkSelector "sheet"

-- | @Selector@ for @absoluteLinkURL@
absoluteLinkURLSelector :: Selector
absoluteLinkURLSelector = mkSelector "absoluteLinkURL"

