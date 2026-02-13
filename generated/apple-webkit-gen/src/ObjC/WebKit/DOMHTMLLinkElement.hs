{-# LANGUAGE DataKinds #-}
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
  , absoluteLinkURLSelector
  , charsetSelector
  , disabledSelector
  , hrefSelector
  , hreflangSelector
  , mediaSelector
  , relSelector
  , revSelector
  , setCharsetSelector
  , setDisabledSelector
  , setHrefSelector
  , setHreflangSelector
  , setMediaSelector
  , setRelSelector
  , setRevSelector
  , setTargetSelector
  , setTypeSelector
  , sheetSelector
  , targetSelector
  , typeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- disabled@
disabled :: IsDOMHTMLLinkElement domhtmlLinkElement => domhtmlLinkElement -> IO Bool
disabled domhtmlLinkElement =
  sendMessage domhtmlLinkElement disabledSelector

-- | @- setDisabled:@
setDisabled :: IsDOMHTMLLinkElement domhtmlLinkElement => domhtmlLinkElement -> Bool -> IO ()
setDisabled domhtmlLinkElement value =
  sendMessage domhtmlLinkElement setDisabledSelector value

-- | @- charset@
charset :: IsDOMHTMLLinkElement domhtmlLinkElement => domhtmlLinkElement -> IO (Id NSString)
charset domhtmlLinkElement =
  sendMessage domhtmlLinkElement charsetSelector

-- | @- setCharset:@
setCharset :: (IsDOMHTMLLinkElement domhtmlLinkElement, IsNSString value) => domhtmlLinkElement -> value -> IO ()
setCharset domhtmlLinkElement value =
  sendMessage domhtmlLinkElement setCharsetSelector (toNSString value)

-- | @- href@
href :: IsDOMHTMLLinkElement domhtmlLinkElement => domhtmlLinkElement -> IO (Id NSString)
href domhtmlLinkElement =
  sendMessage domhtmlLinkElement hrefSelector

-- | @- setHref:@
setHref :: (IsDOMHTMLLinkElement domhtmlLinkElement, IsNSString value) => domhtmlLinkElement -> value -> IO ()
setHref domhtmlLinkElement value =
  sendMessage domhtmlLinkElement setHrefSelector (toNSString value)

-- | @- hreflang@
hreflang :: IsDOMHTMLLinkElement domhtmlLinkElement => domhtmlLinkElement -> IO (Id NSString)
hreflang domhtmlLinkElement =
  sendMessage domhtmlLinkElement hreflangSelector

-- | @- setHreflang:@
setHreflang :: (IsDOMHTMLLinkElement domhtmlLinkElement, IsNSString value) => domhtmlLinkElement -> value -> IO ()
setHreflang domhtmlLinkElement value =
  sendMessage domhtmlLinkElement setHreflangSelector (toNSString value)

-- | @- media@
media :: IsDOMHTMLLinkElement domhtmlLinkElement => domhtmlLinkElement -> IO (Id NSString)
media domhtmlLinkElement =
  sendMessage domhtmlLinkElement mediaSelector

-- | @- setMedia:@
setMedia :: (IsDOMHTMLLinkElement domhtmlLinkElement, IsNSString value) => domhtmlLinkElement -> value -> IO ()
setMedia domhtmlLinkElement value =
  sendMessage domhtmlLinkElement setMediaSelector (toNSString value)

-- | @- rel@
rel :: IsDOMHTMLLinkElement domhtmlLinkElement => domhtmlLinkElement -> IO (Id NSString)
rel domhtmlLinkElement =
  sendMessage domhtmlLinkElement relSelector

-- | @- setRel:@
setRel :: (IsDOMHTMLLinkElement domhtmlLinkElement, IsNSString value) => domhtmlLinkElement -> value -> IO ()
setRel domhtmlLinkElement value =
  sendMessage domhtmlLinkElement setRelSelector (toNSString value)

-- | @- rev@
rev :: IsDOMHTMLLinkElement domhtmlLinkElement => domhtmlLinkElement -> IO (Id NSString)
rev domhtmlLinkElement =
  sendMessage domhtmlLinkElement revSelector

-- | @- setRev:@
setRev :: (IsDOMHTMLLinkElement domhtmlLinkElement, IsNSString value) => domhtmlLinkElement -> value -> IO ()
setRev domhtmlLinkElement value =
  sendMessage domhtmlLinkElement setRevSelector (toNSString value)

-- | @- target@
target :: IsDOMHTMLLinkElement domhtmlLinkElement => domhtmlLinkElement -> IO (Id NSString)
target domhtmlLinkElement =
  sendMessage domhtmlLinkElement targetSelector

-- | @- setTarget:@
setTarget :: (IsDOMHTMLLinkElement domhtmlLinkElement, IsNSString value) => domhtmlLinkElement -> value -> IO ()
setTarget domhtmlLinkElement value =
  sendMessage domhtmlLinkElement setTargetSelector (toNSString value)

-- | @- type@
type_ :: IsDOMHTMLLinkElement domhtmlLinkElement => domhtmlLinkElement -> IO (Id NSString)
type_ domhtmlLinkElement =
  sendMessage domhtmlLinkElement typeSelector

-- | @- setType:@
setType :: (IsDOMHTMLLinkElement domhtmlLinkElement, IsNSString value) => domhtmlLinkElement -> value -> IO ()
setType domhtmlLinkElement value =
  sendMessage domhtmlLinkElement setTypeSelector (toNSString value)

-- | @- sheet@
sheet :: IsDOMHTMLLinkElement domhtmlLinkElement => domhtmlLinkElement -> IO (Id DOMStyleSheet)
sheet domhtmlLinkElement =
  sendMessage domhtmlLinkElement sheetSelector

-- | @- absoluteLinkURL@
absoluteLinkURL :: IsDOMHTMLLinkElement domhtmlLinkElement => domhtmlLinkElement -> IO (Id NSURL)
absoluteLinkURL domhtmlLinkElement =
  sendMessage domhtmlLinkElement absoluteLinkURLSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @disabled@
disabledSelector :: Selector '[] Bool
disabledSelector = mkSelector "disabled"

-- | @Selector@ for @setDisabled:@
setDisabledSelector :: Selector '[Bool] ()
setDisabledSelector = mkSelector "setDisabled:"

-- | @Selector@ for @charset@
charsetSelector :: Selector '[] (Id NSString)
charsetSelector = mkSelector "charset"

-- | @Selector@ for @setCharset:@
setCharsetSelector :: Selector '[Id NSString] ()
setCharsetSelector = mkSelector "setCharset:"

-- | @Selector@ for @href@
hrefSelector :: Selector '[] (Id NSString)
hrefSelector = mkSelector "href"

-- | @Selector@ for @setHref:@
setHrefSelector :: Selector '[Id NSString] ()
setHrefSelector = mkSelector "setHref:"

-- | @Selector@ for @hreflang@
hreflangSelector :: Selector '[] (Id NSString)
hreflangSelector = mkSelector "hreflang"

-- | @Selector@ for @setHreflang:@
setHreflangSelector :: Selector '[Id NSString] ()
setHreflangSelector = mkSelector "setHreflang:"

-- | @Selector@ for @media@
mediaSelector :: Selector '[] (Id NSString)
mediaSelector = mkSelector "media"

-- | @Selector@ for @setMedia:@
setMediaSelector :: Selector '[Id NSString] ()
setMediaSelector = mkSelector "setMedia:"

-- | @Selector@ for @rel@
relSelector :: Selector '[] (Id NSString)
relSelector = mkSelector "rel"

-- | @Selector@ for @setRel:@
setRelSelector :: Selector '[Id NSString] ()
setRelSelector = mkSelector "setRel:"

-- | @Selector@ for @rev@
revSelector :: Selector '[] (Id NSString)
revSelector = mkSelector "rev"

-- | @Selector@ for @setRev:@
setRevSelector :: Selector '[Id NSString] ()
setRevSelector = mkSelector "setRev:"

-- | @Selector@ for @target@
targetSelector :: Selector '[] (Id NSString)
targetSelector = mkSelector "target"

-- | @Selector@ for @setTarget:@
setTargetSelector :: Selector '[Id NSString] ()
setTargetSelector = mkSelector "setTarget:"

-- | @Selector@ for @type@
typeSelector :: Selector '[] (Id NSString)
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector '[Id NSString] ()
setTypeSelector = mkSelector "setType:"

-- | @Selector@ for @sheet@
sheetSelector :: Selector '[] (Id DOMStyleSheet)
sheetSelector = mkSelector "sheet"

-- | @Selector@ for @absoluteLinkURL@
absoluteLinkURLSelector :: Selector '[] (Id NSURL)
absoluteLinkURLSelector = mkSelector "absoluteLinkURL"

