{-# LANGUAGE DataKinds #-}
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
  , allHTTPResponseHeaderFieldsSelector
  , dateSelector
  , errorCommentSelector
  , errorDomainSelector
  , errorStatusCodeSelector
  , initSelector
  , newSelector
  , playbackSessionIDSelector
  , serverAddressSelector
  , uriSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVPlayerItemErrorLogEvent avPlayerItemErrorLogEvent => avPlayerItemErrorLogEvent -> IO (Id AVPlayerItemErrorLogEvent)
init_ avPlayerItemErrorLogEvent =
  sendOwnedMessage avPlayerItemErrorLogEvent initSelector

-- | @+ new@
new :: IO (Id AVPlayerItemErrorLogEvent)
new  =
  do
    cls' <- getRequiredClass "AVPlayerItemErrorLogEvent"
    sendOwnedClassMessage cls' newSelector

-- | The date and time when the error occured. Can be nil.
--
-- If nil is returned the date is unknown. Corresponds to "date". This property is not observable.
--
-- ObjC selector: @- date@
date :: IsAVPlayerItemErrorLogEvent avPlayerItemErrorLogEvent => avPlayerItemErrorLogEvent -> IO (Id NSDate)
date avPlayerItemErrorLogEvent =
  sendMessage avPlayerItemErrorLogEvent dateSelector

-- | The URI of the playback item. Can be nil.
--
-- If nil is returned the URI is unknown. Corresponds to "uri". This property is not observable.
--
-- ObjC selector: @- URI@
uri :: IsAVPlayerItemErrorLogEvent avPlayerItemErrorLogEvent => avPlayerItemErrorLogEvent -> IO (Id NSString)
uri avPlayerItemErrorLogEvent =
  sendMessage avPlayerItemErrorLogEvent uriSelector

-- | The IP address of the server that was the source of the error. Can be nil.
--
-- If nil is returned the address is unknown. Can be either an IPv4 or IPv6 address. Corresponds to "s-ip". This property is not observable.
--
-- ObjC selector: @- serverAddress@
serverAddress :: IsAVPlayerItemErrorLogEvent avPlayerItemErrorLogEvent => avPlayerItemErrorLogEvent -> IO (Id NSString)
serverAddress avPlayerItemErrorLogEvent =
  sendMessage avPlayerItemErrorLogEvent serverAddressSelector

-- | A GUID that identifies the playback session. This value is used in HTTP requests. Can be nil.
--
-- If nil is returned the GUID is unknown. Corresponds to "cs-guid". This property is not observable.
--
-- ObjC selector: @- playbackSessionID@
playbackSessionID :: IsAVPlayerItemErrorLogEvent avPlayerItemErrorLogEvent => avPlayerItemErrorLogEvent -> IO (Id NSString)
playbackSessionID avPlayerItemErrorLogEvent =
  sendMessage avPlayerItemErrorLogEvent playbackSessionIDSelector

-- | A unique error code identifier.
--
-- Corresponds to "status". This property is not observable.
--
-- ObjC selector: @- errorStatusCode@
errorStatusCode :: IsAVPlayerItemErrorLogEvent avPlayerItemErrorLogEvent => avPlayerItemErrorLogEvent -> IO CLong
errorStatusCode avPlayerItemErrorLogEvent =
  sendMessage avPlayerItemErrorLogEvent errorStatusCodeSelector

-- | The domain of the error.
--
-- Corresponds to "domain". This property is not observable.
--
-- ObjC selector: @- errorDomain@
errorDomain :: IsAVPlayerItemErrorLogEvent avPlayerItemErrorLogEvent => avPlayerItemErrorLogEvent -> IO (Id NSString)
errorDomain avPlayerItemErrorLogEvent =
  sendMessage avPlayerItemErrorLogEvent errorDomainSelector

-- | A description of the error encountered. Can be nil.
--
-- If nil is returned further information is not available. Corresponds to "comment". This property is not observable.
--
-- ObjC selector: @- errorComment@
errorComment :: IsAVPlayerItemErrorLogEvent avPlayerItemErrorLogEvent => avPlayerItemErrorLogEvent -> IO (Id NSString)
errorComment avPlayerItemErrorLogEvent =
  sendMessage avPlayerItemErrorLogEvent errorCommentSelector

-- | The HTTP header fields returned by the server, if an HTTP response was received as part of this error.
--
-- See -[NSHTTPURLResponse allHeaderFields] for more information.
--
-- ObjC selector: @- allHTTPResponseHeaderFields@
allHTTPResponseHeaderFields :: IsAVPlayerItemErrorLogEvent avPlayerItemErrorLogEvent => avPlayerItemErrorLogEvent -> IO (Id NSDictionary)
allHTTPResponseHeaderFields avPlayerItemErrorLogEvent =
  sendMessage avPlayerItemErrorLogEvent allHTTPResponseHeaderFieldsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVPlayerItemErrorLogEvent)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVPlayerItemErrorLogEvent)
newSelector = mkSelector "new"

-- | @Selector@ for @date@
dateSelector :: Selector '[] (Id NSDate)
dateSelector = mkSelector "date"

-- | @Selector@ for @URI@
uriSelector :: Selector '[] (Id NSString)
uriSelector = mkSelector "URI"

-- | @Selector@ for @serverAddress@
serverAddressSelector :: Selector '[] (Id NSString)
serverAddressSelector = mkSelector "serverAddress"

-- | @Selector@ for @playbackSessionID@
playbackSessionIDSelector :: Selector '[] (Id NSString)
playbackSessionIDSelector = mkSelector "playbackSessionID"

-- | @Selector@ for @errorStatusCode@
errorStatusCodeSelector :: Selector '[] CLong
errorStatusCodeSelector = mkSelector "errorStatusCode"

-- | @Selector@ for @errorDomain@
errorDomainSelector :: Selector '[] (Id NSString)
errorDomainSelector = mkSelector "errorDomain"

-- | @Selector@ for @errorComment@
errorCommentSelector :: Selector '[] (Id NSString)
errorCommentSelector = mkSelector "errorComment"

-- | @Selector@ for @allHTTPResponseHeaderFields@
allHTTPResponseHeaderFieldsSelector :: Selector '[] (Id NSDictionary)
allHTTPResponseHeaderFieldsSelector = mkSelector "allHTTPResponseHeaderFields"

