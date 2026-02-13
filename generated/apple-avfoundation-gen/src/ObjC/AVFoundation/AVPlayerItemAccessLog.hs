{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An AVPlayerItemAccessLog provides methods to retrieve the access log in a format suitable for serialization.
--
-- An AVPlayerItemAccessLog acculumulates key metrics about network playback and presents them as a collection  of AVPlayerItemAccessLogEvent instances. Each AVPlayerItemAccessLogEvent instance collates the data  that relates to each uninterrupted period of playback.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVPlayerItemAccessLog@.
module ObjC.AVFoundation.AVPlayerItemAccessLog
  ( AVPlayerItemAccessLog
  , IsAVPlayerItemAccessLog(..)
  , init_
  , new
  , extendedLogData
  , extendedLogDataStringEncoding
  , events
  , eventsSelector
  , extendedLogDataSelector
  , extendedLogDataStringEncodingSelector
  , initSelector
  , newSelector


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
init_ :: IsAVPlayerItemAccessLog avPlayerItemAccessLog => avPlayerItemAccessLog -> IO (Id AVPlayerItemAccessLog)
init_ avPlayerItemAccessLog =
  sendOwnedMessage avPlayerItemAccessLog initSelector

-- | @+ new@
new :: IO (Id AVPlayerItemAccessLog)
new  =
  do
    cls' <- getRequiredClass "AVPlayerItemAccessLog"
    sendOwnedClassMessage cls' newSelector

-- | Serializes an AVPlayerItemAccessLog in the Extended Log File Format.
--
-- This method converts the webserver access log into a textual format that conforms to the W3C Extended Log File Format for web server log files. For more information see: http://www.w3.org/pub/WWW/TR/WD-logfile.html
--
-- - Returns: An autoreleased NSData instance.
--
-- ObjC selector: @- extendedLogData@
extendedLogData :: IsAVPlayerItemAccessLog avPlayerItemAccessLog => avPlayerItemAccessLog -> IO (Id NSData)
extendedLogData avPlayerItemAccessLog =
  sendMessage avPlayerItemAccessLog extendedLogDataSelector

-- | Returns the NSStringEncoding for extendedLogData, see above.
--
-- A string suitable for console output is obtainable by:  [[NSString alloc] initWithData:[myLog extendedLogData] encoding:[myLog extendedLogDataStringEncoding]]
--
-- ObjC selector: @- extendedLogDataStringEncoding@
extendedLogDataStringEncoding :: IsAVPlayerItemAccessLog avPlayerItemAccessLog => avPlayerItemAccessLog -> IO CULong
extendedLogDataStringEncoding avPlayerItemAccessLog =
  sendMessage avPlayerItemAccessLog extendedLogDataStringEncodingSelector

-- | An ordered collection of AVPlayerItemAccessLogEvent instances.
--
-- An ordered collection of AVPlayerItemAccessLogEvent instances that represent the chronological sequence of events contained in the access log. This property is not observable.
--
-- ObjC selector: @- events@
events :: IsAVPlayerItemAccessLog avPlayerItemAccessLog => avPlayerItemAccessLog -> IO (Id NSArray)
events avPlayerItemAccessLog =
  sendMessage avPlayerItemAccessLog eventsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVPlayerItemAccessLog)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVPlayerItemAccessLog)
newSelector = mkSelector "new"

-- | @Selector@ for @extendedLogData@
extendedLogDataSelector :: Selector '[] (Id NSData)
extendedLogDataSelector = mkSelector "extendedLogData"

-- | @Selector@ for @extendedLogDataStringEncoding@
extendedLogDataStringEncodingSelector :: Selector '[] CULong
extendedLogDataStringEncodingSelector = mkSelector "extendedLogDataStringEncoding"

-- | @Selector@ for @events@
eventsSelector :: Selector '[] (Id NSArray)
eventsSelector = mkSelector "events"

