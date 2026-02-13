{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | DRMSFFormatter
--
-- NSFormatter subclass
--
-- Instances of DRMSFFormatter format the textual representation of cells that contain 				MSF objects and convert textual representations of msf values into MSF objects.				DRMSFFormatters are typically instantiated in IB using the DiscRecording Interface builder palette.
--
-- Generated bindings for @DRMSFFormatter@.
module ObjC.DiscRecording.DRMSFFormatter
  ( DRMSFFormatter
  , IsDRMSFFormatter(..)
  , initWithFormat
  , format
  , setFormat
  , formatSelector
  , initWithFormatSelector
  , setFormatSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.DiscRecording.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | initWithFormat:
--
-- Initializes the formatter with the format string
--
-- @format@ — An NString specifying the printf-style format string.
--
-- Returns: A DRMSFFormatter
--
-- ObjC selector: @- initWithFormat:@
initWithFormat :: (IsDRMSFFormatter drmsfFormatter, IsNSString format) => drmsfFormatter -> format -> IO RawId
initWithFormat drmsfFormatter format =
  sendOwnedMessage drmsfFormatter initWithFormatSelector (toNSString format)

-- | format
--
-- Returns the format string to the caller
--
-- ObjC selector: @- format@
format :: IsDRMSFFormatter drmsfFormatter => drmsfFormatter -> IO (Id NSString)
format drmsfFormatter =
  sendMessage drmsfFormatter formatSelector

-- | setFormat:
--
-- Sets the format string of the receiver
--
-- The format string is very similar to				a printf-style format string with %-escaped formatting characters.
--
-- %%	A "%" character				%m	Minutes as a decimal number				%s	Seconds as a decimal number				%f	Frames as a decimal number
--
-- In addition to these formatting characters an optional length specifier can come between then				% and the formatting character. This length specifier will force the field in question to 				be at least that wide. for example a format specifier of "%02m:%02s" will cause a 				DRMSF object representing 3 minutes 9 seconds to be formatted as "03:09".
--
-- A formatter is aware of and respects rounding. If a bit of the msf is not zero, but the format				does not display that value, the next higher value will be increased by one to reflect that.				Extending our example above, an DRMSF with a value of 3 minutes, 9 seconds, 15 frames using a 				format specfier of "%02m:%02s", will be formatted as "03:10" since the 15 frames rounds up the				seconds to the next value
--
-- @format@ — An NString specifying the printf-style format string.
--
-- ObjC selector: @- setFormat:@
setFormat :: (IsDRMSFFormatter drmsfFormatter, IsNSString format) => drmsfFormatter -> format -> IO ()
setFormat drmsfFormatter format =
  sendMessage drmsfFormatter setFormatSelector (toNSString format)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFormat:@
initWithFormatSelector :: Selector '[Id NSString] RawId
initWithFormatSelector = mkSelector "initWithFormat:"

-- | @Selector@ for @format@
formatSelector :: Selector '[] (Id NSString)
formatSelector = mkSelector "format"

-- | @Selector@ for @setFormat:@
setFormatSelector :: Selector '[Id NSString] ()
setFormatSelector = mkSelector "setFormat:"

