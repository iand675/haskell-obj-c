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
  , initSelector
  , newSelector
  , extendedLogDataSelector
  , extendedLogDataStringEncodingSelector
  , eventsSelector


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
init_ :: IsAVPlayerItemAccessLog avPlayerItemAccessLog => avPlayerItemAccessLog -> IO (Id AVPlayerItemAccessLog)
init_ avPlayerItemAccessLog  =
  sendMsg avPlayerItemAccessLog (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVPlayerItemAccessLog)
new  =
  do
    cls' <- getRequiredClass "AVPlayerItemAccessLog"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Serializes an AVPlayerItemAccessLog in the Extended Log File Format.
--
-- This method converts the webserver access log into a textual format that conforms to the W3C Extended Log File Format for web server log files. For more information see: http://www.w3.org/pub/WWW/TR/WD-logfile.html
--
-- - Returns: An autoreleased NSData instance.
--
-- ObjC selector: @- extendedLogData@
extendedLogData :: IsAVPlayerItemAccessLog avPlayerItemAccessLog => avPlayerItemAccessLog -> IO (Id NSData)
extendedLogData avPlayerItemAccessLog  =
  sendMsg avPlayerItemAccessLog (mkSelector "extendedLogData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the NSStringEncoding for extendedLogData, see above.
--
-- A string suitable for console output is obtainable by:  [[NSString alloc] initWithData:[myLog extendedLogData] encoding:[myLog extendedLogDataStringEncoding]]
--
-- ObjC selector: @- extendedLogDataStringEncoding@
extendedLogDataStringEncoding :: IsAVPlayerItemAccessLog avPlayerItemAccessLog => avPlayerItemAccessLog -> IO CULong
extendedLogDataStringEncoding avPlayerItemAccessLog  =
  sendMsg avPlayerItemAccessLog (mkSelector "extendedLogDataStringEncoding") retCULong []

-- | An ordered collection of AVPlayerItemAccessLogEvent instances.
--
-- An ordered collection of AVPlayerItemAccessLogEvent instances that represent the chronological sequence of events contained in the access log. This property is not observable.
--
-- ObjC selector: @- events@
events :: IsAVPlayerItemAccessLog avPlayerItemAccessLog => avPlayerItemAccessLog -> IO (Id NSArray)
events avPlayerItemAccessLog  =
  sendMsg avPlayerItemAccessLog (mkSelector "events") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @extendedLogData@
extendedLogDataSelector :: Selector
extendedLogDataSelector = mkSelector "extendedLogData"

-- | @Selector@ for @extendedLogDataStringEncoding@
extendedLogDataStringEncodingSelector :: Selector
extendedLogDataStringEncodingSelector = mkSelector "extendedLogDataStringEncoding"

-- | @Selector@ for @events@
eventsSelector :: Selector
eventsSelector = mkSelector "events"

