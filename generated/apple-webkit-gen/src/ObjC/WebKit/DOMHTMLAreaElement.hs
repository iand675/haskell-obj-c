{-# LANGUAGE DataKinds #-}
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
  , absoluteLinkURLSelector
  , accessKeySelector
  , altSelector
  , coordsSelector
  , hashNameSelector
  , hostSelector
  , hostnameSelector
  , hrefSelector
  , noHrefSelector
  , pathnameSelector
  , portSelector
  , protocolSelector
  , searchSelector
  , setAccessKeySelector
  , setAltSelector
  , setCoordsSelector
  , setHrefSelector
  , setNoHrefSelector
  , setShapeSelector
  , setTargetSelector
  , shapeSelector
  , targetSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- alt@
alt :: IsDOMHTMLAreaElement domhtmlAreaElement => domhtmlAreaElement -> IO (Id NSString)
alt domhtmlAreaElement =
  sendMessage domhtmlAreaElement altSelector

-- | @- setAlt:@
setAlt :: (IsDOMHTMLAreaElement domhtmlAreaElement, IsNSString value) => domhtmlAreaElement -> value -> IO ()
setAlt domhtmlAreaElement value =
  sendMessage domhtmlAreaElement setAltSelector (toNSString value)

-- | @- coords@
coords :: IsDOMHTMLAreaElement domhtmlAreaElement => domhtmlAreaElement -> IO (Id NSString)
coords domhtmlAreaElement =
  sendMessage domhtmlAreaElement coordsSelector

-- | @- setCoords:@
setCoords :: (IsDOMHTMLAreaElement domhtmlAreaElement, IsNSString value) => domhtmlAreaElement -> value -> IO ()
setCoords domhtmlAreaElement value =
  sendMessage domhtmlAreaElement setCoordsSelector (toNSString value)

-- | @- noHref@
noHref :: IsDOMHTMLAreaElement domhtmlAreaElement => domhtmlAreaElement -> IO Bool
noHref domhtmlAreaElement =
  sendMessage domhtmlAreaElement noHrefSelector

-- | @- setNoHref:@
setNoHref :: IsDOMHTMLAreaElement domhtmlAreaElement => domhtmlAreaElement -> Bool -> IO ()
setNoHref domhtmlAreaElement value =
  sendMessage domhtmlAreaElement setNoHrefSelector value

-- | @- shape@
shape :: IsDOMHTMLAreaElement domhtmlAreaElement => domhtmlAreaElement -> IO (Id NSString)
shape domhtmlAreaElement =
  sendMessage domhtmlAreaElement shapeSelector

-- | @- setShape:@
setShape :: (IsDOMHTMLAreaElement domhtmlAreaElement, IsNSString value) => domhtmlAreaElement -> value -> IO ()
setShape domhtmlAreaElement value =
  sendMessage domhtmlAreaElement setShapeSelector (toNSString value)

-- | @- target@
target :: IsDOMHTMLAreaElement domhtmlAreaElement => domhtmlAreaElement -> IO (Id NSString)
target domhtmlAreaElement =
  sendMessage domhtmlAreaElement targetSelector

-- | @- setTarget:@
setTarget :: (IsDOMHTMLAreaElement domhtmlAreaElement, IsNSString value) => domhtmlAreaElement -> value -> IO ()
setTarget domhtmlAreaElement value =
  sendMessage domhtmlAreaElement setTargetSelector (toNSString value)

-- | @- accessKey@
accessKey :: IsDOMHTMLAreaElement domhtmlAreaElement => domhtmlAreaElement -> IO (Id NSString)
accessKey domhtmlAreaElement =
  sendMessage domhtmlAreaElement accessKeySelector

-- | @- setAccessKey:@
setAccessKey :: (IsDOMHTMLAreaElement domhtmlAreaElement, IsNSString value) => domhtmlAreaElement -> value -> IO ()
setAccessKey domhtmlAreaElement value =
  sendMessage domhtmlAreaElement setAccessKeySelector (toNSString value)

-- | @- absoluteLinkURL@
absoluteLinkURL :: IsDOMHTMLAreaElement domhtmlAreaElement => domhtmlAreaElement -> IO (Id NSURL)
absoluteLinkURL domhtmlAreaElement =
  sendMessage domhtmlAreaElement absoluteLinkURLSelector

-- | @- href@
href :: IsDOMHTMLAreaElement domhtmlAreaElement => domhtmlAreaElement -> IO (Id NSString)
href domhtmlAreaElement =
  sendMessage domhtmlAreaElement hrefSelector

-- | @- setHref:@
setHref :: (IsDOMHTMLAreaElement domhtmlAreaElement, IsNSString value) => domhtmlAreaElement -> value -> IO ()
setHref domhtmlAreaElement value =
  sendMessage domhtmlAreaElement setHrefSelector (toNSString value)

-- | @- protocol@
protocol :: IsDOMHTMLAreaElement domhtmlAreaElement => domhtmlAreaElement -> IO (Id NSString)
protocol domhtmlAreaElement =
  sendMessage domhtmlAreaElement protocolSelector

-- | @- host@
host :: IsDOMHTMLAreaElement domhtmlAreaElement => domhtmlAreaElement -> IO (Id NSString)
host domhtmlAreaElement =
  sendMessage domhtmlAreaElement hostSelector

-- | @- hostname@
hostname :: IsDOMHTMLAreaElement domhtmlAreaElement => domhtmlAreaElement -> IO (Id NSString)
hostname domhtmlAreaElement =
  sendMessage domhtmlAreaElement hostnameSelector

-- | @- port@
port :: IsDOMHTMLAreaElement domhtmlAreaElement => domhtmlAreaElement -> IO (Id NSString)
port domhtmlAreaElement =
  sendMessage domhtmlAreaElement portSelector

-- | @- pathname@
pathname :: IsDOMHTMLAreaElement domhtmlAreaElement => domhtmlAreaElement -> IO (Id NSString)
pathname domhtmlAreaElement =
  sendMessage domhtmlAreaElement pathnameSelector

-- | @- search@
search :: IsDOMHTMLAreaElement domhtmlAreaElement => domhtmlAreaElement -> IO (Id NSString)
search domhtmlAreaElement =
  sendMessage domhtmlAreaElement searchSelector

-- | @- hashName@
hashName :: IsDOMHTMLAreaElement domhtmlAreaElement => domhtmlAreaElement -> IO (Id NSString)
hashName domhtmlAreaElement =
  sendMessage domhtmlAreaElement hashNameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @alt@
altSelector :: Selector '[] (Id NSString)
altSelector = mkSelector "alt"

-- | @Selector@ for @setAlt:@
setAltSelector :: Selector '[Id NSString] ()
setAltSelector = mkSelector "setAlt:"

-- | @Selector@ for @coords@
coordsSelector :: Selector '[] (Id NSString)
coordsSelector = mkSelector "coords"

-- | @Selector@ for @setCoords:@
setCoordsSelector :: Selector '[Id NSString] ()
setCoordsSelector = mkSelector "setCoords:"

-- | @Selector@ for @noHref@
noHrefSelector :: Selector '[] Bool
noHrefSelector = mkSelector "noHref"

-- | @Selector@ for @setNoHref:@
setNoHrefSelector :: Selector '[Bool] ()
setNoHrefSelector = mkSelector "setNoHref:"

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

-- | @Selector@ for @accessKey@
accessKeySelector :: Selector '[] (Id NSString)
accessKeySelector = mkSelector "accessKey"

-- | @Selector@ for @setAccessKey:@
setAccessKeySelector :: Selector '[Id NSString] ()
setAccessKeySelector = mkSelector "setAccessKey:"

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

