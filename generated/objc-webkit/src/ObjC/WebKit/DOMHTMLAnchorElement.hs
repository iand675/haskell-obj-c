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
  , charsetSelector
  , setCharsetSelector
  , coordsSelector
  , setCoordsSelector
  , hreflangSelector
  , setHreflangSelector
  , nameSelector
  , setNameSelector
  , relSelector
  , setRelSelector
  , revSelector
  , setRevSelector
  , shapeSelector
  , setShapeSelector
  , targetSelector
  , setTargetSelector
  , typeSelector
  , setTypeSelector
  , accessKeySelector
  , setAccessKeySelector
  , textSelector
  , absoluteLinkURLSelector
  , hrefSelector
  , setHrefSelector
  , protocolSelector
  , hostSelector
  , hostnameSelector
  , portSelector
  , pathnameSelector
  , searchSelector
  , hashNameSelector


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

-- | @- charset@
charset :: IsDOMHTMLAnchorElement domhtmlAnchorElement => domhtmlAnchorElement -> IO (Id NSString)
charset domhtmlAnchorElement  =
  sendMsg domhtmlAnchorElement (mkSelector "charset") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCharset:@
setCharset :: (IsDOMHTMLAnchorElement domhtmlAnchorElement, IsNSString value) => domhtmlAnchorElement -> value -> IO ()
setCharset domhtmlAnchorElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlAnchorElement (mkSelector "setCharset:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- coords@
coords :: IsDOMHTMLAnchorElement domhtmlAnchorElement => domhtmlAnchorElement -> IO (Id NSString)
coords domhtmlAnchorElement  =
  sendMsg domhtmlAnchorElement (mkSelector "coords") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCoords:@
setCoords :: (IsDOMHTMLAnchorElement domhtmlAnchorElement, IsNSString value) => domhtmlAnchorElement -> value -> IO ()
setCoords domhtmlAnchorElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlAnchorElement (mkSelector "setCoords:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- hreflang@
hreflang :: IsDOMHTMLAnchorElement domhtmlAnchorElement => domhtmlAnchorElement -> IO (Id NSString)
hreflang domhtmlAnchorElement  =
  sendMsg domhtmlAnchorElement (mkSelector "hreflang") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setHreflang:@
setHreflang :: (IsDOMHTMLAnchorElement domhtmlAnchorElement, IsNSString value) => domhtmlAnchorElement -> value -> IO ()
setHreflang domhtmlAnchorElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlAnchorElement (mkSelector "setHreflang:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- name@
name :: IsDOMHTMLAnchorElement domhtmlAnchorElement => domhtmlAnchorElement -> IO (Id NSString)
name domhtmlAnchorElement  =
  sendMsg domhtmlAnchorElement (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsDOMHTMLAnchorElement domhtmlAnchorElement, IsNSString value) => domhtmlAnchorElement -> value -> IO ()
setName domhtmlAnchorElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlAnchorElement (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- rel@
rel :: IsDOMHTMLAnchorElement domhtmlAnchorElement => domhtmlAnchorElement -> IO (Id NSString)
rel domhtmlAnchorElement  =
  sendMsg domhtmlAnchorElement (mkSelector "rel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRel:@
setRel :: (IsDOMHTMLAnchorElement domhtmlAnchorElement, IsNSString value) => domhtmlAnchorElement -> value -> IO ()
setRel domhtmlAnchorElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlAnchorElement (mkSelector "setRel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- rev@
rev :: IsDOMHTMLAnchorElement domhtmlAnchorElement => domhtmlAnchorElement -> IO (Id NSString)
rev domhtmlAnchorElement  =
  sendMsg domhtmlAnchorElement (mkSelector "rev") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRev:@
setRev :: (IsDOMHTMLAnchorElement domhtmlAnchorElement, IsNSString value) => domhtmlAnchorElement -> value -> IO ()
setRev domhtmlAnchorElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlAnchorElement (mkSelector "setRev:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- shape@
shape :: IsDOMHTMLAnchorElement domhtmlAnchorElement => domhtmlAnchorElement -> IO (Id NSString)
shape domhtmlAnchorElement  =
  sendMsg domhtmlAnchorElement (mkSelector "shape") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setShape:@
setShape :: (IsDOMHTMLAnchorElement domhtmlAnchorElement, IsNSString value) => domhtmlAnchorElement -> value -> IO ()
setShape domhtmlAnchorElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlAnchorElement (mkSelector "setShape:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- target@
target :: IsDOMHTMLAnchorElement domhtmlAnchorElement => domhtmlAnchorElement -> IO (Id NSString)
target domhtmlAnchorElement  =
  sendMsg domhtmlAnchorElement (mkSelector "target") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTarget:@
setTarget :: (IsDOMHTMLAnchorElement domhtmlAnchorElement, IsNSString value) => domhtmlAnchorElement -> value -> IO ()
setTarget domhtmlAnchorElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlAnchorElement (mkSelector "setTarget:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- type@
type_ :: IsDOMHTMLAnchorElement domhtmlAnchorElement => domhtmlAnchorElement -> IO (Id NSString)
type_ domhtmlAnchorElement  =
  sendMsg domhtmlAnchorElement (mkSelector "type") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setType:@
setType :: (IsDOMHTMLAnchorElement domhtmlAnchorElement, IsNSString value) => domhtmlAnchorElement -> value -> IO ()
setType domhtmlAnchorElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlAnchorElement (mkSelector "setType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- accessKey@
accessKey :: IsDOMHTMLAnchorElement domhtmlAnchorElement => domhtmlAnchorElement -> IO (Id NSString)
accessKey domhtmlAnchorElement  =
  sendMsg domhtmlAnchorElement (mkSelector "accessKey") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAccessKey:@
setAccessKey :: (IsDOMHTMLAnchorElement domhtmlAnchorElement, IsNSString value) => domhtmlAnchorElement -> value -> IO ()
setAccessKey domhtmlAnchorElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlAnchorElement (mkSelector "setAccessKey:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- text@
text :: IsDOMHTMLAnchorElement domhtmlAnchorElement => domhtmlAnchorElement -> IO (Id NSString)
text domhtmlAnchorElement  =
  sendMsg domhtmlAnchorElement (mkSelector "text") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- absoluteLinkURL@
absoluteLinkURL :: IsDOMHTMLAnchorElement domhtmlAnchorElement => domhtmlAnchorElement -> IO (Id NSURL)
absoluteLinkURL domhtmlAnchorElement  =
  sendMsg domhtmlAnchorElement (mkSelector "absoluteLinkURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- href@
href :: IsDOMHTMLAnchorElement domhtmlAnchorElement => domhtmlAnchorElement -> IO (Id NSString)
href domhtmlAnchorElement  =
  sendMsg domhtmlAnchorElement (mkSelector "href") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setHref:@
setHref :: (IsDOMHTMLAnchorElement domhtmlAnchorElement, IsNSString value) => domhtmlAnchorElement -> value -> IO ()
setHref domhtmlAnchorElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlAnchorElement (mkSelector "setHref:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- protocol@
protocol :: IsDOMHTMLAnchorElement domhtmlAnchorElement => domhtmlAnchorElement -> IO (Id NSString)
protocol domhtmlAnchorElement  =
  sendMsg domhtmlAnchorElement (mkSelector "protocol") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- host@
host :: IsDOMHTMLAnchorElement domhtmlAnchorElement => domhtmlAnchorElement -> IO (Id NSString)
host domhtmlAnchorElement  =
  sendMsg domhtmlAnchorElement (mkSelector "host") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- hostname@
hostname :: IsDOMHTMLAnchorElement domhtmlAnchorElement => domhtmlAnchorElement -> IO (Id NSString)
hostname domhtmlAnchorElement  =
  sendMsg domhtmlAnchorElement (mkSelector "hostname") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- port@
port :: IsDOMHTMLAnchorElement domhtmlAnchorElement => domhtmlAnchorElement -> IO (Id NSString)
port domhtmlAnchorElement  =
  sendMsg domhtmlAnchorElement (mkSelector "port") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- pathname@
pathname :: IsDOMHTMLAnchorElement domhtmlAnchorElement => domhtmlAnchorElement -> IO (Id NSString)
pathname domhtmlAnchorElement  =
  sendMsg domhtmlAnchorElement (mkSelector "pathname") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- search@
search :: IsDOMHTMLAnchorElement domhtmlAnchorElement => domhtmlAnchorElement -> IO (Id NSString)
search domhtmlAnchorElement  =
  sendMsg domhtmlAnchorElement (mkSelector "search") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- hashName@
hashName :: IsDOMHTMLAnchorElement domhtmlAnchorElement => domhtmlAnchorElement -> IO (Id NSString)
hashName domhtmlAnchorElement  =
  sendMsg domhtmlAnchorElement (mkSelector "hashName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @charset@
charsetSelector :: Selector
charsetSelector = mkSelector "charset"

-- | @Selector@ for @setCharset:@
setCharsetSelector :: Selector
setCharsetSelector = mkSelector "setCharset:"

-- | @Selector@ for @coords@
coordsSelector :: Selector
coordsSelector = mkSelector "coords"

-- | @Selector@ for @setCoords:@
setCoordsSelector :: Selector
setCoordsSelector = mkSelector "setCoords:"

-- | @Selector@ for @hreflang@
hreflangSelector :: Selector
hreflangSelector = mkSelector "hreflang"

-- | @Selector@ for @setHreflang:@
setHreflangSelector :: Selector
setHreflangSelector = mkSelector "setHreflang:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

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

-- | @Selector@ for @shape@
shapeSelector :: Selector
shapeSelector = mkSelector "shape"

-- | @Selector@ for @setShape:@
setShapeSelector :: Selector
setShapeSelector = mkSelector "setShape:"

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

-- | @Selector@ for @accessKey@
accessKeySelector :: Selector
accessKeySelector = mkSelector "accessKey"

-- | @Selector@ for @setAccessKey:@
setAccessKeySelector :: Selector
setAccessKeySelector = mkSelector "setAccessKey:"

-- | @Selector@ for @text@
textSelector :: Selector
textSelector = mkSelector "text"

-- | @Selector@ for @absoluteLinkURL@
absoluteLinkURLSelector :: Selector
absoluteLinkURLSelector = mkSelector "absoluteLinkURL"

-- | @Selector@ for @href@
hrefSelector :: Selector
hrefSelector = mkSelector "href"

-- | @Selector@ for @setHref:@
setHrefSelector :: Selector
setHrefSelector = mkSelector "setHref:"

-- | @Selector@ for @protocol@
protocolSelector :: Selector
protocolSelector = mkSelector "protocol"

-- | @Selector@ for @host@
hostSelector :: Selector
hostSelector = mkSelector "host"

-- | @Selector@ for @hostname@
hostnameSelector :: Selector
hostnameSelector = mkSelector "hostname"

-- | @Selector@ for @port@
portSelector :: Selector
portSelector = mkSelector "port"

-- | @Selector@ for @pathname@
pathnameSelector :: Selector
pathnameSelector = mkSelector "pathname"

-- | @Selector@ for @search@
searchSelector :: Selector
searchSelector = mkSelector "search"

-- | @Selector@ for @hashName@
hashNameSelector :: Selector
hashNameSelector = mkSelector "hashName"

