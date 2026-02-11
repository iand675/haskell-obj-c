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
initWithData avmidiSysexEvent  data_ =
withObjCPtr data_ $ \raw_data_ ->
    sendMsg avmidiSysexEvent (mkSelector "initWithData:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ())] >>= ownedObject . castPtr

-- | sizeInBytes
--
-- The size of the raw data associated with this system exclusive event.
--
-- ObjC selector: @- sizeInBytes@
sizeInBytes :: IsAVMIDISysexEvent avmidiSysexEvent => avmidiSysexEvent -> IO CUInt
sizeInBytes avmidiSysexEvent  =
  sendMsg avmidiSysexEvent (mkSelector "sizeInBytes") retCUInt []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithData:@
initWithDataSelector :: Selector
initWithDataSelector = mkSelector "initWithData:"

-- | @Selector@ for @sizeInBytes@
sizeInBytesSelector :: Selector
sizeInBytesSelector = mkSelector "sizeInBytes"

