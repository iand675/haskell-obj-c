{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DRMSF@.
module ObjC.DiscRecording.DRMSF
  ( DRMSF
  , IsDRMSF(..)
  , msf
  , msfWithFrames
  , msfWithString
  , initWithFrames
  , initWithString
  , minutes
  , seconds
  , frames
  , sectors
  , msfByAdding
  , msfBySubtracting
  , description
  , descriptionWithFormat
  , isEqualToMSF
  , descriptionSelector
  , descriptionWithFormatSelector
  , framesSelector
  , initWithFramesSelector
  , initWithStringSelector
  , isEqualToMSFSelector
  , minutesSelector
  , msfByAddingSelector
  , msfBySubtractingSelector
  , msfSelector
  , msfWithFramesSelector
  , msfWithStringSelector
  , secondsSelector
  , sectorsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.DiscRecording.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | msf
--
-- Creates an msf object with no length/time.
--
-- Returns: An autoreleased DRMSF object.
--
-- ObjC selector: @+ msf@
msf :: IO (Id DRMSF)
msf  =
  do
    cls' <- getRequiredClass "DRMSF"
    sendClassMessage cls' msfSelector

-- | msfWithFrames
--
-- Creates an msf object whose length is frames.
--
-- Returns: An autoreleased DRMSF object.
--
-- ObjC selector: @+ msfWithFrames:@
msfWithFrames :: CUInt -> IO (Id DRMSF)
msfWithFrames frames =
  do
    cls' <- getRequiredClass "DRMSF"
    sendClassMessage cls' msfWithFramesSelector frames

-- | msfWithString
--
-- Creates an msf object initialized to the value represented by string
--
-- Returns: An autoreleased DRMSF object.
--
-- ObjC selector: @+ msfWithString:@
msfWithString :: IsNSString string => string -> IO (Id DRMSF)
msfWithString string =
  do
    cls' <- getRequiredClass "DRMSF"
    sendClassMessage cls' msfWithStringSelector (toNSString string)

-- | initWithFrames
--
-- Initializes an msf object whose length is frames.
--
-- Returns: A DRMSF object.
--
-- ObjC selector: @- initWithFrames:@
initWithFrames :: IsDRMSF drmsf => drmsf -> CUInt -> IO RawId
initWithFrames drmsf frames =
  sendOwnedMessage drmsf initWithFramesSelector frames

-- | initWithString
--
-- Initializes an msf object initialized to the value represented by string
--
-- Returns: A DRMSF object.
--
-- ObjC selector: @- initWithString:@
initWithString :: (IsDRMSF drmsf, IsNSString string) => drmsf -> string -> IO RawId
initWithString drmsf string =
  sendOwnedMessage drmsf initWithStringSelector (toNSString string)

-- | minutes
--
-- Returns the number of minutes represented by the receiver.
--
-- If the receiver represents 				a non integral number of minutes, only the whole minute value is returned. For example				an DRMSF value of 5:30:72 will return 5 from a message to
--
-- //apple_ref/occ/instm/DRMSF/minutes minutes
--
-- .
--
-- ObjC selector: @- minutes@
minutes :: IsDRMSF drmsf => drmsf -> IO CUInt
minutes drmsf =
  sendMessage drmsf minutesSelector

-- | seconds
--
-- Returns the number of seconds represented by the receiver.
--
-- If the receiver represents 				a non integral number of seconds, only the whole second value is returned. For example				an DRMSF value of 5:30:72 will return 30 from a message to
--
-- //apple_ref/occ/instm/DRMSF/seconds seconds
--
-- .
--
-- ObjC selector: @- seconds@
seconds :: IsDRMSF drmsf => drmsf -> IO CUInt
seconds drmsf =
  sendMessage drmsf secondsSelector

-- | frames
--
-- Returns the number of frames represented by the receiver.
--
-- This method differs from
--
-- //apple_ref/occ/instm/DFMSF/sectors sectors
--
-- in that it				returns to the caller the number of frames remaining in the current second. For example				an DRMSF value of 5:30:72 will return 72 from a message to
--
-- //apple_ref/occ/instm/DRMSF/frames frames
--
-- .
--
-- ObjC selector: @- frames@
frames :: IsDRMSF drmsf => drmsf -> IO CUInt
frames drmsf =
  sendMessage drmsf framesSelector

-- | sectors
--
-- Returns the total number of frames/sectors represented by the receiver.
--
-- This method differs from
--
-- //apple_ref/occ/instm/DRMSF/frames frames
--
-- in that it				returns to the caller the total number of frames/sectors represented by the object.				For example an DRMSF value of 5:30:72 will return 24822 from a message to
--
-- //apple_ref/occ/instm/DRMSF/sectors sectors
--
-- .
--
-- ObjC selector: @- sectors@
sectors :: IsDRMSF drmsf => drmsf -> IO CUInt
sectors drmsf =
  sendMessage drmsf sectorsSelector

-- | msfByAdding
--
-- Adds an msf to the receiver.
--
-- @msf@ — The msf to add to the receiver
--
-- Returns: A new DRMSF object totalling the sum of the reciever and msf
--
-- ObjC selector: @- msfByAdding:@
msfByAdding :: (IsDRMSF drmsf, IsDRMSF msf) => drmsf -> msf -> IO (Id DRMSF)
msfByAdding drmsf msf =
  sendMessage drmsf msfByAddingSelector (toDRMSF msf)

-- | msfBySubtracting
--
-- Subtracts an msf to the receiver.
--
-- @msf@ — The msf to subtract from the receiver
--
-- Returns: A new DRMSF object totalling the difference of the reciever and msf
--
-- ObjC selector: @- msfBySubtracting:@
msfBySubtracting :: (IsDRMSF drmsf, IsDRMSF msf) => drmsf -> msf -> IO (Id DRMSF)
msfBySubtracting drmsf msf =
  sendMessage drmsf msfBySubtractingSelector (toDRMSF msf)

-- | description
--
-- Returns a textual representation of the receiver.
--
-- Returns: NSString containing a textual representation of the object with the standard formatting.
--
-- ObjC selector: @- description@
description :: IsDRMSF drmsf => drmsf -> IO (Id NSString)
description drmsf =
  sendMessage drmsf descriptionSelector

-- | descriptionWithFormat
--
-- Returns a textual representation of the receiver.
--
-- The format string is very similar to				a printf-style format string with %-escaped formatting characters.
--
-- %%	A "%" character				%m	Minutes as a decimal number				%s	Seconds as a decimal number				%f	Frames as a decimal number
--
-- In addition to these formatting characters an optional length specifier can come between then				% and the formatting character. This length specifier will force the field in question to 				be at least that wide. For example a format specifier of "%02m:%02s" will cause a 				DRMSF object representing 3 minutes 9 seconds to be formatted as "03:09".
--
-- A formatter is aware of and respects rounding. If a bit of the msf is not zero, but the format				does not display that value, the next higher value will be increased by one to reflect that.				Extending our example above, an DRMSF with a value of 3 minutes, 9 seconds, 15 frames using a 				format specfier of "%02m:%02s", will be formatted as "03:10" since the 15 frames rounds up the				seconds to the next value
--
-- @format@ — The format of the description string.
--
-- Returns: NSString containing a textual representation of the object utilizing the specified format.
--
-- ObjC selector: @- descriptionWithFormat:@
descriptionWithFormat :: (IsDRMSF drmsf, IsNSString format) => drmsf -> format -> IO (Id NSString)
descriptionWithFormat drmsf format =
  sendMessage drmsf descriptionWithFormatSelector (toNSString format)

-- | isEqualToMSF
--
-- Compares on emsf to another.
--
-- @otherDRMSF@ — The msf to compare to the receiver
--
-- Returns: YES if the two object are equal, NO otherwise.
--
-- ObjC selector: @- isEqualToMSF:@
isEqualToMSF :: (IsDRMSF drmsf, IsDRMSF otherDRMSF) => drmsf -> otherDRMSF -> IO Bool
isEqualToMSF drmsf otherDRMSF =
  sendMessage drmsf isEqualToMSFSelector (toDRMSF otherDRMSF)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @msf@
msfSelector :: Selector '[] (Id DRMSF)
msfSelector = mkSelector "msf"

-- | @Selector@ for @msfWithFrames:@
msfWithFramesSelector :: Selector '[CUInt] (Id DRMSF)
msfWithFramesSelector = mkSelector "msfWithFrames:"

-- | @Selector@ for @msfWithString:@
msfWithStringSelector :: Selector '[Id NSString] (Id DRMSF)
msfWithStringSelector = mkSelector "msfWithString:"

-- | @Selector@ for @initWithFrames:@
initWithFramesSelector :: Selector '[CUInt] RawId
initWithFramesSelector = mkSelector "initWithFrames:"

-- | @Selector@ for @initWithString:@
initWithStringSelector :: Selector '[Id NSString] RawId
initWithStringSelector = mkSelector "initWithString:"

-- | @Selector@ for @minutes@
minutesSelector :: Selector '[] CUInt
minutesSelector = mkSelector "minutes"

-- | @Selector@ for @seconds@
secondsSelector :: Selector '[] CUInt
secondsSelector = mkSelector "seconds"

-- | @Selector@ for @frames@
framesSelector :: Selector '[] CUInt
framesSelector = mkSelector "frames"

-- | @Selector@ for @sectors@
sectorsSelector :: Selector '[] CUInt
sectorsSelector = mkSelector "sectors"

-- | @Selector@ for @msfByAdding:@
msfByAddingSelector :: Selector '[Id DRMSF] (Id DRMSF)
msfByAddingSelector = mkSelector "msfByAdding:"

-- | @Selector@ for @msfBySubtracting:@
msfBySubtractingSelector :: Selector '[Id DRMSF] (Id DRMSF)
msfBySubtractingSelector = mkSelector "msfBySubtracting:"

-- | @Selector@ for @description@
descriptionSelector :: Selector '[] (Id NSString)
descriptionSelector = mkSelector "description"

-- | @Selector@ for @descriptionWithFormat:@
descriptionWithFormatSelector :: Selector '[Id NSString] (Id NSString)
descriptionWithFormatSelector = mkSelector "descriptionWithFormat:"

-- | @Selector@ for @isEqualToMSF:@
isEqualToMSFSelector :: Selector '[Id DRMSF] Bool
isEqualToMSFSelector = mkSelector "isEqualToMSF:"

