{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVMIDISysexEvent
--
-- The event class representing MIDI system exclusive messages.
--
-- The size and contents of an AVMIDISysexEvent cannot be modified once created.
--
-- Generated bindings for @AVMIDISysexEvent@.
module ObjC.AVFAudio.AVMIDISysexEvent
  ( AVMIDISysexEvent
  , IsAVMIDISysexEvent(..)
  , initWithData
  , sizeInBytes
  , initWithDataSelector
  , sizeInBytesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFAudio.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | initWithData:
--
-- Initialize the event with an NSData.
--
-- @data@ — An NSData object containing the raw contents of the system exclusive event.
--
-- ObjC selector: @- initWithData:@
initWithData :: (IsAVMIDISysexEvent avmidiSysexEvent, IsNSData data_) => avmidiSysexEvent -> data_ -> IO (Id AVMIDISysexEvent)
initWithData avmidiSysexEvent data_ =
  sendOwnedMessage avmidiSysexEvent initWithDataSelector (toNSData data_)

-- | sizeInBytes
--
-- The size of the raw data associated with this system exclusive event.
--
-- ObjC selector: @- sizeInBytes@
sizeInBytes :: IsAVMIDISysexEvent avmidiSysexEvent => avmidiSysexEvent -> IO CUInt
sizeInBytes avmidiSysexEvent =
  sendMessage avmidiSysexEvent sizeInBytesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithData:@
initWithDataSelector :: Selector '[Id NSData] (Id AVMIDISysexEvent)
initWithDataSelector = mkSelector "initWithData:"

-- | @Selector@ for @sizeInBytes@
sizeInBytesSelector :: Selector '[] CUInt
sizeInBytesSelector = mkSelector "sizeInBytes"

