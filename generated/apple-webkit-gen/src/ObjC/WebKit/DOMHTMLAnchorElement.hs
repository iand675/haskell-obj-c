{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLAnchorElement@.
module ObjC.WebKit.DOMHTMLAnchorElement
  ( DOMHTMLAnchorElement
  , IsDOMHTMLAnchorElement(..)
  , charset
  , setCharset
  , coords
  , setCoords
  , hreflang
  , setHreflang
  , name
  , setName
  , rel
  , setRel
  , rev
  , setRev
  , shape
  , setShape
  , target
  , setTarget
  , type_
  , setType
  , accessKey
  , setAccessKey
  , text
  , absoluteLinkURL
  , href
  , setHref
  , protocol
  , host
  , hostname
  , port
  , pathname
  , search
  , hashName
  , absoluteLinkURLSelector
  , accessKeySelector
  , charsetSelector
  , coordsSelector
  , hashNameSelector
  , hostSelector
  , hostnameSelector
  , hrefSelector
  , hreflangSelector
  , nameSelector
  , pathnameSelector
  , portSelector
  , protocolSelector
  , relSelector
  , revSelector
  , searchSelector
  , setAccessKeySelector
  , setCharsetSelector
  , setCoordsSelector
  , setHrefSelector
  , setHreflangSelector
  , setNameSelector
  , setRelSelector
  , setRevSelector
  , setShapeSelector
  , setTargetSelector
  , setTypeSelector
  , shapeSelector
  , targetSelector
  , textSelector
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

-- | @- charset@
charset :: IsDOMHTMLAnchorElement domhtmlAnchorElement => domhtmlAnchorElement -> IO (Id NSString)
charset domhtmlAnchorElement =
  sendMessage domhtmlAnchorElement charsetSelector

-- | @- setCharset:@
setCharset :: (IsDOMHTMLAnchorElement domhtmlAnchorElement, IsNSString value) => domhtmlAnchorElement -> value -> IO ()
setCharset domhtmlAnchorElement value =
  sendMessage domhtmlAnchorElement setCharsetSelector (toNSString value)

-- | @- coords@
coords :: IsDOMHTMLAnchorElement domhtmlAnchorElement => domhtmlAnchorElement -> IO (Id NSString)
coords domhtmlAnchorElement =
  sendMessage domhtmlAnchorElement coordsSelector

-- | @- setCoords:@
setCoords :: (IsDOMHTMLAnchorElement domhtmlAnchorElement, IsNSString value) => domhtmlAnchorElement -> value -> IO ()
setCoords domhtmlAnchorElement value =
  sendMessage domhtmlAnchorElement setCoordsSelector (toNSString value)

-- | @- hreflang@
hreflang :: IsDOMHTMLAnchorElement domhtmlAnchorElement => domhtmlAnchorElement -> IO (Id NSString)
hreflang domhtmlAnchorElement =
  sendMessage domhtmlAnchorElement hreflangSelector

-- | @- setHreflang:@
setHreflang :: (IsDOMHTMLAnchorElement domhtmlAnchorElement, IsNSString value) => domhtmlAnchorElement -> value -> IO ()
setHreflang domhtmlAnchorElement value =
  sendMessage domhtmlAnchorElement setHreflangSelector (toNSString value)

-- | @- name@
name :: IsDOMHTMLAnchorElement domhtmlAnchorElement => domhtmlAnchorElement -> IO (Id NSString)
name domhtmlAnchorElement =
  sendMessage domhtmlAnchorElement nameSelector

-- | @- setName:@
setName :: (IsDOMHTMLAnchorElement domhtmlAnchorElement, IsNSString value) => domhtmlAnchorElement -> value -> IO ()
setName domhtmlAnchorElement value =
  sendMessage domhtmlAnchorElement setNameSelector (toNSString value)

-- | @- rel@
rel :: IsDOMHTMLAnchorElement domhtmlAnchorElement => domhtmlAnchorElement -> IO (Id NSString)
rel domhtmlAnchorElement =
  sendMessage domhtmlAnchorElement relSelector

-- | @- setRel:@
setRel :: (IsDOMHTMLAnchorElement domhtmlAnchorElement, IsNSString value) => domhtmlAnchorElement -> value -> IO ()
setRel domhtmlAnchorElement value =
  sendMessage domhtmlAnchorElement setRelSelector (toNSString value)

-- | @- rev@
rev :: IsDOMHTMLAnchorElement domhtmlAnchorElement => domhtmlAnchorElement -> IO (Id NSString)
rev domhtmlAnchorElement =
  sendMessage domhtmlAnchorElement revSelector

-- | @- setRev:@
setRev :: (IsDOMHTMLAnchorElement domhtmlAnchorElement, IsNSString value) => domhtmlAnchorElement -> value -> IO ()
setRev domhtmlAnchorElement value =
  sendMessage domhtmlAnchorElement setRevSelector (toNSString value)

-- | @- shape@
shape :: IsDOMHTMLAnchorElement domhtmlAnchorElement => domhtmlAnchorElement -> IO (Id NSString)
shape domhtmlAnchorElement =
  sendMessage domhtmlAnchorElement shapeSelector

-- | @- setShape:@
setShape :: (IsDOMHTMLAnchorElement domhtmlAnchorElement, IsNSString value) => domhtmlAnchorElement -> value -> IO ()
setShape domhtmlAnchorElement value =
  sendMessage domhtmlAnchorElement setShapeSelector (toNSString value)

-- | @- target@
target :: IsDOMHTMLAnchorElement domhtmlAnchorElement => domhtmlAnchorElement -> IO (Id NSString)
target domhtmlAnchorElement =
  sendMessage domhtmlAnchorElement targetSelector

-- | @- setTarget:@
setTarget :: (IsDOMHTMLAnchorElement domhtmlAnchorElement, IsNSString value) => domhtmlAnchorElement -> value -> IO ()
setTarget domhtmlAnchorElement value =
  sendMessage domhtmlAnchorElement setTargetSelector (toNSString value)

-- | @- type@
type_ :: IsDOMHTMLAnchorElement domhtmlAnchorElement => domhtmlAnchorElement -> IO (Id NSString)
type_ domhtmlAnchorElement =
  sendMessage domhtmlAnchorElement typeSelector

-- | @- setType:@
setType :: (IsDOMHTMLAnchorElement domhtmlAnchorElement, IsNSString value) => domhtmlAnchorElement -> value -> IO ()
setType domhtmlAnchorElement value =
  sendMessage domhtmlAnchorElement setTypeSelector (toNSString value)

-- | @- accessKey@
accessKey :: IsDOMHTMLAnchorElement domhtmlAnchorElement => domhtmlAnchorElement -> IO (Id NSString)
accessKey domhtmlAnchorElement =
  sendMessage domhtmlAnchorElement accessKeySelector

-- | @- setAccessKey:@
setAccessKey :: (IsDOMHTMLAnchorElement domhtmlAnchorElement, IsNSString value) => domhtmlAnchorElement -> value -> IO ()
setAccessKey domhtmlAnchorElement value =
  sendMessage domhtmlAnchorElement setAccessKeySelector (toNSString value)

-- | @- text@
text :: IsDOMHTMLAnchorElement domhtmlAnchorElement => domhtmlAnchorElement -> IO (Id NSString)
text domhtmlAnchorElement =
  sendMessage domhtmlAnchorElement textSelector

-- | @- absoluteLinkURL@
absoluteLinkURL :: IsDOMHTMLAnchorElement domhtmlAnchorElement => domhtmlAnchorElement -> IO (Id NSURL)
absoluteLinkURL domhtmlAnchorElement =
  sendMessage domhtmlAnchorElement absoluteLinkURLSelector

-- | @- href@
href :: IsDOMHTMLAnchorElement domhtmlAnchorElement => domhtmlAnchorElement -> IO (Id NSString)
href domhtmlAnchorElement =
  sendMessage domhtmlAnchorElement hrefSelector

-- | @- setHref:@
setHref :: (IsDOMHTMLAnchorElement domhtmlAnchorElement, IsNSString value) => domhtmlAnchorElement -> value -> IO ()
setHref domhtmlAnchorElement value =
  sendMessage domhtmlAnchorElement setHrefSelector (toNSString value)

-- | @- protocol@
protocol :: IsDOMHTMLAnchorElement domhtmlAnchorElement => domhtmlAnchorElement -> IO (Id NSString)
protocol domhtmlAnchorElement =
  sendMessage domhtmlAnchorElement protocolSelector

-- | @- host@
host :: IsDOMHTMLAnchorElement domhtmlAnchorElement => domhtmlAnchorElement -> IO (Id NSString)
host domhtmlAnchorElement =
  sendMessage domhtmlAnchorElement hostSelector

-- | @- hostname@
hostname :: IsDOMHTMLAnchorElement domhtmlAnchorElement => domhtmlAnchorElement -> IO (Id NSString)
hostname domhtmlAnchorElement =
  sendMessage domhtmlAnchorElement hostnameSelector

-- | @- port@
port :: IsDOMHTMLAnchorElement domhtmlAnchorElement => domhtmlAnchorElement -> IO (Id NSString)
port domhtmlAnchorElement =
  sendMessage domhtmlAnchorElement portSelector

-- | @- pathname@
pathname :: IsDOMHTMLAnchorElement domhtmlAnchorElement => domhtmlAnchorElement -> IO (Id NSString)
pathname domhtmlAnchorElement =
  sendMessage domhtmlAnchorElement pathnameSelector

-- | @- search@
search :: IsDOMHTMLAnchorElement domhtmlAnchorElement => domhtmlAnchorElement -> IO (Id NSString)
search domhtmlAnchorElement =
  sendMessage domhtmlAnchorElement searchSelector

-- | @- hashName@
hashName :: IsDOMHTMLAnchorElement domhtmlAnchorElement => domhtmlAnchorElement -> IO (Id NSString)
hashName domhtmlAnchorElement =
  sendMessage domhtmlAnchorElement hashNameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @charset@
charsetSelector :: Selector '[] (Id NSString)
charsetSelector = mkSelector "charset"

-- | @Selector@ for @setCharset:@
setCharsetSelector :: Selector '[Id NSString] ()
setCharsetSelector = mkSelector "setCharset:"

-- | @Selector@ for @coords@
coordsSelector :: Selector '[] (Id NSString)
coordsSelector = mkSelector "coords"

-- | @Selector@ for @setCoords:@
setCoordsSelector :: Selector '[Id NSString] ()
setCoordsSelector = mkSelector "setCoords:"

-- | @Selector@ for @hreflang@
hreflangSelector :: Selector '[] (Id NSString)
hreflangSelector = mkSelector "hreflang"

-- | @Selector@ for @setHreflang:@
setHreflangSelector :: Selector '[Id NSString] ()
setHreflangSelector = mkSelector "setHreflang:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

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

-- | @Selector@ for @shape@
shapeSelector :: Selector '[] (Id NSString)
shapeSelector = mkSelector "shape"

-- | @Selector@ for @setShape:@
setShapeSelector :: Selector '[Id NSString] ()
setShapeSelector = mkSelector "setShape:"

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

-- | @Selector@ for @accessKey@
accessKeySelector :: Selector '[] (Id NSString)
accessKeySelector = mkSelector "accessKey"

-- | @Selector@ for @setAccessKey:@
setAccessKeySelector :: Selector '[Id NSString] ()
setAccessKeySelector = mkSelector "setAccessKey:"

-- | @Selector@ for @text@
textSelector :: Selector '[] (Id NSString)
textSelector = mkSelector "text"

-- | @Selector@ for @absoluteLinkURL@
absoluteLinkURLSelector :: Selector '[] (Id NSURL)
absoluteLinkURLSelector = mkSelector "absoluteLinkURL"

-- | @Selector@ for @href@
hrefSelector :: Selector '[] (Id NSString)
hrefSelector = mkSelector "href"

-- | @Selector@ for @setHref:@
setHrefSelector :: Selector '[Id NSString] ()
setHrefSelector = mkSelector "setHref:"

-- | @Selector@ for @protocol@
protocolSelector :: Selector '[] (Id NSString)
protocolSelector = mkSelector "protocol"

-- | @Selector@ for @host@
hostSelector :: Selector '[] (Id NSString)
hostSelector = mkSelector "host"

-- | @Selector@ for @hostname@
hostnameSelector :: Selector '[] (Id NSString)
hostnameSelector = mkSelector "hostname"

-- | @Selector@ for @port@
portSelector :: Selector '[] (Id NSString)
portSelector = mkSelector "port"

-- | @Selector@ for @pathname@
pathnameSelector :: Selector '[] (Id NSString)
pathnameSelector = mkSelector "pathname"

-- | @Selector@ for @search@
searchSelector :: Selector '[] (Id NSString)
searchSelector = mkSelector "search"

-- | @Selector@ for @hashName@
hashNameSelector :: Selector '[] (Id NSString)
hashNameSelector = mkSelector "hashName"

