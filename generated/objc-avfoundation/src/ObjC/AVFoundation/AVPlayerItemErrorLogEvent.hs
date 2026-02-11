{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An AVPlayerItemErrorLogEvent represents a single log entry.
--
-- An AVPlayerItemErrorLogEvent provides named properties for accessing the data fields of each log event. None of the properties of this class are observable.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVPlayerItemErrorLogEvent@.
module ObjC.AVFoundation.AVPlayerItemErrorLogEvent
  ( AVPlayerItemErrorLogEvent
  , IsAVPlayerItemErrorLogEvent(..)
  , init_
  , new
  , date
  , uri
  , serverAddress
  , playbackSessionID
  , errorStatusCode
  , errorDomain
  , errorComment
  , allHTTPResponseHeaderFields
  , initSelector
  , newSelector
  , dateSelector
  , uriSelector
  , serverAddressSelector
  , playbackSessionIDSelector
  , errorStatusCodeSelector
  , errorDomainSelector
  , errorCommentSelector
  , allHTTPResponseHeaderFieldsSelector


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

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVPlayerItemErrorLogEvent avPlayerItemErrorLogEvent => avPlayerItemErrorLogEvent -> IO (Id AVPlayerItemErrorLogEvent)
init_ avPlayerItemErrorLogEvent  =
  sendMsg avPlayerItemErrorLogEvent (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVPlayerItemErrorLogEvent)
new  =
  do
    cls' <- getRequiredClass "AVPlayerItemErrorLogEvent"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The date and time when the error occured. Can be nil.
--
-- If nil is returned the date is unknown. Corresponds to "date". This property is not observable.
--
-- ObjC selector: @- date@
date :: IsAVPlayerItemErrorLogEvent avPlayerItemErrorLogEvent => avPlayerItemErrorLogEvent -> IO (Id NSDate)
date avPlayerItemErrorLogEvent  =
  sendMsg avPlayerItemErrorLogEvent (mkSelector "date") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The URI of the playback item. Can be nil.
--
-- If nil is returned the URI is unknown. Corresponds to "uri". This property is not observable.
--
-- ObjC selector: @- URI@
uri :: IsAVPlayerItemErrorLogEvent avPlayerItemErrorLogEvent => avPlayerItemErrorLogEvent -> IO (Id NSString)
uri avPlayerItemErrorLogEvent  =
  sendMsg avPlayerItemErrorLogEvent (mkSelector "URI") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The IP address of the server that was the source of the error. Can be nil.
--
-- If nil is returned the address is unknown. Can be either an IPv4 or IPv6 address. Corresponds to "s-ip". This property is not observable.
--
-- ObjC selector: @- serverAddress@
serverAddress :: IsAVPlayerItemErrorLogEvent avPlayerItemErrorLogEvent => avPlayerItemErrorLogEvent -> IO (Id NSString)
serverAddress avPlayerItemErrorLogEvent  =
  sendMsg avPlayerItemErrorLogEvent (mkSelector "serverAddress") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A GUID that identifies the playback session. This value is used in HTTP requests. Can be nil.
--
-- If nil is returned the GUID is unknown. Corresponds to "cs-guid". This property is not observable.
--
-- ObjC selector: @- playbackSessionID@
playbackSessionID :: IsAVPlayerItemErrorLogEvent avPlayerItemErrorLogEvent => avPlayerItemErrorLogEvent -> IO (Id NSString)
playbackSessionID avPlayerItemErrorLogEvent  =
  sendMsg avPlayerItemErrorLogEvent (mkSelector "playbackSessionID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A unique error code identifier.
--
-- Corresponds to "status". This property is not observable.
--
-- ObjC selector: @- errorStatusCode@
errorStatusCode :: IsAVPlayerItemErrorLogEvent avPlayerItemErrorLogEvent => avPlayerItemErrorLogEvent -> IO CLong
errorStatusCode avPlayerItemErrorLogEvent  =
  sendMsg avPlayerItemErrorLogEvent (mkSelector "errorStatusCode") retCLong []

-- | The domain of the error.
--
-- Corresponds to "domain". This property is not observable.
--
-- ObjC selector: @- errorDomain@
errorDomain :: IsAVPlayerItemErrorLogEvent avPlayerItemErrorLogEvent => avPlayerItemErrorLogEvent -> IO (Id NSString)
errorDomain avPlayerItemErrorLogEvent  =
  sendMsg avPlayerItemErrorLogEvent (mkSelector "errorDomain") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A description of the error encountered. Can be nil.
--
-- If nil is returned further information is not available. Corresponds to "comment". This property is not observable.
--
-- ObjC selector: @- errorComment@
errorComment :: IsAVPlayerItemErrorLogEvent avPlayerItemErrorLogEvent => avPlayerItemErrorLogEvent -> IO (Id NSString)
errorComment avPlayerItemErrorLogEvent  =
  sendMsg avPlayerItemErrorLogEvent (mkSelector "errorComment") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The HTTP header fields returned by the server, if an HTTP response was received as part of this error.
--
-- See -[NSHTTPURLResponse allHeaderFields] for more information.
--
-- ObjC selector: @- allHTTPResponseHeaderFields@
allHTTPResponseHeaderFields :: IsAVPlayerItemErrorLogEvent avPlayerItemErrorLogEvent => avPlayerItemErrorLogEvent -> IO (Id NSDictionary)
allHTTPResponseHeaderFields avPlayerItemErrorLogEvent  =
  sendMsg avPlayerItemErrorLogEvent (mkSelector "allHTTPResponseHeaderFields") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @date@
dateSelector :: Selector
dateSelector = mkSelector "date"

-- | @Selector@ for @URI@
uriSelector :: Selector
uriSelector = mkSelector "URI"

-- | @Selector@ for @serverAddress@
serverAddressSelector :: Selector
serverAddressSelector = mkSelector "serverAddress"

-- | @Selector@ for @playbackSessionID@
playbackSessionIDSelector :: Selector
playbackSessionIDSelector = mkSelector "playbackSessionID"

-- | @Selector@ for @errorStatusCode@
errorStatusCodeSelector :: Selector
errorStatusCodeSelector = mkSelector "errorStatusCode"

-- | @Selector@ for @errorDomain@
errorDomainSelector :: Selector
errorDomainSelector = mkSelector "errorDomain"

-- | @Selector@ for @errorComment@
errorCommentSelector :: Selector
errorCommentSelector = mkSelector "errorComment"

-- | @Selector@ for @allHTTPResponseHeaderFields@
allHTTPResponseHeaderFieldsSelector :: Selector
allHTTPResponseHeaderFieldsSelector = mkSelector "allHTTPResponseHeaderFields"

