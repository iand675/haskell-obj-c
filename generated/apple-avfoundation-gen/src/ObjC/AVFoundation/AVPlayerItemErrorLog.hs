{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An AVPlayerItemErrorLog provides methods to retrieve the error log in a format suitable for serialization.
--
-- An AVPlayerItemErrorLog provides data to identify if, and when, network resource playback failures occured.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVPlayerItemErrorLog@.
module ObjC.AVFoundation.AVPlayerItemErrorLog
  ( AVPlayerItemErrorLog
  , IsAVPlayerItemErrorLog(..)
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
init_ :: IsAVPlayerItemErrorLog avPlayerItemErrorLog => avPlayerItemErrorLog -> IO (Id AVPlayerItemErrorLog)
init_ avPlayerItemErrorLog =
  sendOwnedMessage avPlayerItemErrorLog initSelector

-- | @+ new@
new :: IO (Id AVPlayerItemErrorLog)
new  =
  do
    cls' <- getRequiredClass "AVPlayerItemErrorLog"
    sendOwnedClassMessage cls' newSelector

-- | Serializes an AVPlayerItemErrorLog in the Extended Log File Format.
--
-- This method converts the webserver error log into a textual format that conforms to the W3C Extended Log File Format for web server log files. For more information see: http://www.w3.org/pub/WWW/TR/WD-logfile.html
--
-- - Returns: An autoreleased NSData instance.
--
-- ObjC selector: @- extendedLogData@
extendedLogData :: IsAVPlayerItemErrorLog avPlayerItemErrorLog => avPlayerItemErrorLog -> IO (Id NSData)
extendedLogData avPlayerItemErrorLog =
  sendMessage avPlayerItemErrorLog extendedLogDataSelector

-- | Returns the NSStringEncoding for extendedLogData, see above.
--
-- A string suitable for console output is obtainable by:  [[NSString alloc] initWithData:[myLog extendedLogData] encoding:[myLog extendedLogDataStringEncoding]]
--
-- ObjC selector: @- extendedLogDataStringEncoding@
extendedLogDataStringEncoding :: IsAVPlayerItemErrorLog avPlayerItemErrorLog => avPlayerItemErrorLog -> IO CULong
extendedLogDataStringEncoding avPlayerItemErrorLog =
  sendMessage avPlayerItemErrorLog extendedLogDataStringEncodingSelector

-- | An ordered collection of AVPlayerItemErrorLogEvent instances.
--
-- An ordered collection of AVPlayerItemErrorLogEvent instances that represent the chronological sequence of events contained in the error log. This property is not observable.
--
-- ObjC selector: @- events@
events :: IsAVPlayerItemErrorLog avPlayerItemErrorLog => avPlayerItemErrorLog -> IO (Id NSArray)
events avPlayerItemErrorLog =
  sendMessage avPlayerItemErrorLog eventsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVPlayerItemErrorLog)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVPlayerItemErrorLog)
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

