{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLAreaElement@.
module ObjC.WebKit.DOMHTMLAreaElement
  ( DOMHTMLAreaElement
  , IsDOMHTMLAreaElement(..)
  , alt
  , setAlt
  , coords
  , setCoords
  , noHref
  , setNoHref
  , shape
  , setShape
  , target
  , setTarget
  , accessKey
  , setAccessKey
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
  , altSelector
  , setAltSelector
  , coordsSelector
  , setCoordsSelector
  , noHrefSelector
  , setNoHrefSelector
  , shapeSelector
  , setShapeSelector
  , targetSelector
  , setTargetSelector
  , accessKeySelector
  , setAccessKeySelector
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

-- | @- alt@
alt :: IsDOMHTMLAreaElement domhtmlAreaElement => domhtmlAreaElement -> IO (Id NSString)
alt domhtmlAreaElement  =
  sendMsg domhtmlAreaElement (mkSelector "alt") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAlt:@
setAlt :: (IsDOMHTMLAreaElement domhtmlAreaElement, IsNSString value) => domhtmlAreaElement -> value -> IO ()
setAlt domhtmlAreaElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlAreaElement (mkSelector "setAlt:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- coords@
coords :: IsDOMHTMLAreaElement domhtmlAreaElement => domhtmlAreaElement -> IO (Id NSString)
coords domhtmlAreaElement  =
  sendMsg domhtmlAreaElement (mkSelector "coords") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCoords:@
setCoords :: (IsDOMHTMLAreaElement domhtmlAreaElement, IsNSString value) => domhtmlAreaElement -> value -> IO ()
setCoords domhtmlAreaElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlAreaElement (mkSelector "setCoords:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- noHref@
noHref :: IsDOMHTMLAreaElement domhtmlAreaElement => domhtmlAreaElement -> IO Bool
noHref domhtmlAreaElement  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domhtmlAreaElement (mkSelector "noHref") retCULong []

-- | @- setNoHref:@
setNoHref :: IsDOMHTMLAreaElement domhtmlAreaElement => domhtmlAreaElement -> Bool -> IO ()
setNoHref domhtmlAreaElement  value =
  sendMsg domhtmlAreaElement (mkSelector "setNoHref:") retVoid [argCULong (if value then 1 else 0)]

-- | @- shape@
shape :: IsDOMHTMLAreaElement domhtmlAreaElement => domhtmlAreaElement -> IO (Id NSString)
shape domhtmlAreaElement  =
  sendMsg domhtmlAreaElement (mkSelector "shape") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setShape:@
setShape :: (IsDOMHTMLAreaElement domhtmlAreaElement, IsNSString value) => domhtmlAreaElement -> value -> IO ()
setShape domhtmlAreaElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlAreaElement (mkSelector "setShape:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- target@
target :: IsDOMHTMLAreaElement domhtmlAreaElement => domhtmlAreaElement -> IO (Id NSString)
target domhtmlAreaElement  =
  sendMsg domhtmlAreaElement (mkSelector "target") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTarget:@
setTarget :: (IsDOMHTMLAreaElement domhtmlAreaElement, IsNSString value) => domhtmlAreaElement -> value -> IO ()
setTarget domhtmlAreaElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlAreaElement (mkSelector "setTarget:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- accessKey@
accessKey :: IsDOMHTMLAreaElement domhtmlAreaElement => domhtmlAreaElement -> IO (Id NSString)
accessKey domhtmlAreaElement  =
  sendMsg domhtmlAreaElement (mkSelector "accessKey") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAccessKey:@
setAccessKey :: (IsDOMHTMLAreaElement domhtmlAreaElement, IsNSString value) => domhtmlAreaElement -> value -> IO ()
setAccessKey domhtmlAreaElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlAreaElement (mkSelector "setAccessKey:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- absoluteLinkURL@
absoluteLinkURL :: IsDOMHTMLAreaElement domhtmlAreaElement => domhtmlAreaElement -> IO (Id NSURL)
absoluteLinkURL domhtmlAreaElement  =
  sendMsg domhtmlAreaElement (mkSelector "absoluteLinkURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- href@
href :: IsDOMHTMLAreaElement domhtmlAreaElement => domhtmlAreaElement -> IO (Id NSString)
href domhtmlAreaElement  =
  sendMsg domhtmlAreaElement (mkSelector "href") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setHref:@
setHref :: (IsDOMHTMLAreaElement domhtmlAreaElement, IsNSString value) => domhtmlAreaElement -> value -> IO ()
setHref domhtmlAreaElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlAreaElement (mkSelector "setHref:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- protocol@
protocol :: IsDOMHTMLAreaElement domhtmlAreaElement => domhtmlAreaElement -> IO (Id NSString)
protocol domhtmlAreaElement  =
  sendMsg domhtmlAreaElement (mkSelector "protocol") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- host@
host :: IsDOMHTMLAreaElement domhtmlAreaElement => domhtmlAreaElement -> IO (Id NSString)
host domhtmlAreaElement  =
  sendMsg domhtmlAreaElement (mkSelector "host") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- hostname@
hostname :: IsDOMHTMLAreaElement domhtmlAreaElement => domhtmlAreaElement -> IO (Id NSString)
hostname domhtmlAreaElement  =
  sendMsg domhtmlAreaElement (mkSelector "hostname") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- port@
port :: IsDOMHTMLAreaElement domhtmlAreaElement => domhtmlAreaElement -> IO (Id NSString)
port domhtmlAreaElement  =
  sendMsg domhtmlAreaElement (mkSelector "port") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- pathname@
pathname :: IsDOMHTMLAreaElement domhtmlAreaElement => domhtmlAreaElement -> IO (Id NSString)
pathname domhtmlAreaElement  =
  sendMsg domhtmlAreaElement (mkSelector "pathname") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- search@
search :: IsDOMHTMLAreaElement domhtmlAreaElement => domhtmlAreaElement -> IO (Id NSString)
search domhtmlAreaElement  =
  sendMsg domhtmlAreaElement (mkSelector "search") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- hashName@
hashName :: IsDOMHTMLAreaElement domhtmlAreaElement => domhtmlAreaElement -> IO (Id NSString)
hashName domhtmlAreaElement  =
  sendMsg domhtmlAreaElement (mkSelector "hashName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @alt@
altSelector :: Selector
altSelector = mkSelector "alt"

-- | @Selector@ for @setAlt:@
setAltSelector :: Selector
setAltSelector = mkSelector "setAlt:"

-- | @Selector@ for @coords@
coordsSelector :: Selector
coordsSelector = mkSelector "coords"

-- | @Selector@ for @setCoords:@
setCoordsSelector :: Selector
setCoordsSelector = mkSelector "setCoords:"

-- | @Selector@ for @noHref@
noHrefSelector :: Selector
noHrefSelector = mkSelector "noHref"

-- | @Selector@ for @setNoHref:@
setNoHrefSelector :: Selector
setNoHrefSelector = mkSelector "setNoHref:"

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

-- | @Selector@ for @accessKey@
accessKeySelector :: Selector
accessKeySelector = mkSelector "accessKey"

-- | @Selector@ for @setAccessKey:@
setAccessKeySelector :: Selector
setAccessKeySelector = mkSelector "setAccessKey:"

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

