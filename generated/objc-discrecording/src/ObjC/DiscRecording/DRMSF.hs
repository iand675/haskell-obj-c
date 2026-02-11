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
  , msfSelector
  , msfWithFramesSelector
  , msfWithStringSelector
  , initWithFramesSelector
  , initWithStringSelector
  , minutesSelector
  , secondsSelector
  , framesSelector
  , sectorsSelector
  , msfByAddingSelector
  , msfBySubtractingSelector
  , descriptionSelector
  , descriptionWithFormatSelector
  , isEqualToMSFSelector


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
    sendClassMsg cls' (mkSelector "msf") (retPtr retVoid) [] >>= retainedObject . castPtr

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
    sendClassMsg cls' (mkSelector "msfWithFrames:") (retPtr retVoid) [argCUInt (fromIntegral frames)] >>= retainedObject . castPtr

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
    withObjCPtr string $ \raw_string ->
      sendClassMsg cls' (mkSelector "msfWithString:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ())] >>= retainedObject . castPtr

-- | initWithFrames
--
-- Initializes an msf object whose length is frames.
--
-- Returns: A DRMSF object.
--
-- ObjC selector: @- initWithFrames:@
initWithFrames :: IsDRMSF drmsf => drmsf -> CUInt -> IO RawId
initWithFrames drmsf  frames =
  fmap (RawId . castPtr) $ sendMsg drmsf (mkSelector "initWithFrames:") (retPtr retVoid) [argCUInt (fromIntegral frames)]

-- | initWithString
--
-- Initializes an msf object initialized to the value represented by string
--
-- Returns: A DRMSF object.
--
-- ObjC selector: @- initWithString:@
initWithString :: (IsDRMSF drmsf, IsNSString string) => drmsf -> string -> IO RawId
initWithString drmsf  string =
withObjCPtr string $ \raw_string ->
    fmap (RawId . castPtr) $ sendMsg drmsf (mkSelector "initWithString:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ())]

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
minutes drmsf  =
  sendMsg drmsf (mkSelector "minutes") retCUInt []

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
seconds drmsf  =
  sendMsg drmsf (mkSelector "seconds") retCUInt []

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
frames drmsf  =
  sendMsg drmsf (mkSelector "frames") retCUInt []

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
sectors drmsf  =
  sendMsg drmsf (mkSelector "sectors") retCUInt []

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
msfByAdding drmsf  msf =
withObjCPtr msf $ \raw_msf ->
    sendMsg drmsf (mkSelector "msfByAdding:") (retPtr retVoid) [argPtr (castPtr raw_msf :: Ptr ())] >>= retainedObject . castPtr

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
msfBySubtracting drmsf  msf =
withObjCPtr msf $ \raw_msf ->
    sendMsg drmsf (mkSelector "msfBySubtracting:") (retPtr retVoid) [argPtr (castPtr raw_msf :: Ptr ())] >>= retainedObject . castPtr

-- | description
--
-- Returns a textual representation of the receiver.
--
-- Returns: NSString containing a textual representation of the object with the standard formatting.
--
-- ObjC selector: @- description@
description :: IsDRMSF drmsf => drmsf -> IO (Id NSString)
description drmsf  =
  sendMsg drmsf (mkSelector "description") (retPtr retVoid) [] >>= retainedObject . castPtr

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
descriptionWithFormat drmsf  format =
withObjCPtr format $ \raw_format ->
    sendMsg drmsf (mkSelector "descriptionWithFormat:") (retPtr retVoid) [argPtr (castPtr raw_format :: Ptr ())] >>= retainedObject . castPtr

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
isEqualToMSF drmsf  otherDRMSF =
withObjCPtr otherDRMSF $ \raw_otherDRMSF ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg drmsf (mkSelector "isEqualToMSF:") retCULong [argPtr (castPtr raw_otherDRMSF :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @msf@
msfSelector :: Selector
msfSelector = mkSelector "msf"

-- | @Selector@ for @msfWithFrames:@
msfWithFramesSelector :: Selector
msfWithFramesSelector = mkSelector "msfWithFrames:"

-- | @Selector@ for @msfWithString:@
msfWithStringSelector :: Selector
msfWithStringSelector = mkSelector "msfWithString:"

-- | @Selector@ for @initWithFrames:@
initWithFramesSelector :: Selector
initWithFramesSelector = mkSelector "initWithFrames:"

-- | @Selector@ for @initWithString:@
initWithStringSelector :: Selector
initWithStringSelector = mkSelector "initWithString:"

-- | @Selector@ for @minutes@
minutesSelector :: Selector
minutesSelector = mkSelector "minutes"

-- | @Selector@ for @seconds@
secondsSelector :: Selector
secondsSelector = mkSelector "seconds"

-- | @Selector@ for @frames@
framesSelector :: Selector
framesSelector = mkSelector "frames"

-- | @Selector@ for @sectors@
sectorsSelector :: Selector
sectorsSelector = mkSelector "sectors"

-- | @Selector@ for @msfByAdding:@
msfByAddingSelector :: Selector
msfByAddingSelector = mkSelector "msfByAdding:"

-- | @Selector@ for @msfBySubtracting:@
msfBySubtractingSelector :: Selector
msfBySubtractingSelector = mkSelector "msfBySubtracting:"

-- | @Selector@ for @description@
descriptionSelector :: Selector
descriptionSelector = mkSelector "description"

-- | @Selector@ for @descriptionWithFormat:@
descriptionWithFormatSelector :: Selector
descriptionWithFormatSelector = mkSelector "descriptionWithFormat:"

-- | @Selector@ for @isEqualToMSF:@
isEqualToMSFSelector :: Selector
isEqualToMSFSelector = mkSelector "isEqualToMSF:"

