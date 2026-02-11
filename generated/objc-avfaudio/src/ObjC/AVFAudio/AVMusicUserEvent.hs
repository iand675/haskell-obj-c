{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVMusicUserEvent
--
-- The event class representing custom user messages.
--
-- When a scheduled AVMusicUserEvent is reached during playback of a AVMusicTrack, the track's		user callback block will be called if it has been set.  The event's NSData will be provided as		an argument to that block.		The size and contents of an AVMusicUserEvent cannot be modified once created.
--
-- Generated bindings for @AVMusicUserEvent@.
module ObjC.AVFAudio.AVMusicUserEvent
  ( AVMusicUserEvent
  , IsAVMusicUserEvent(..)
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
-- @data@ — An NSData object containing the contents to be returned via the AVMusicTrack's user callback.
--
-- ObjC selector: @- initWithData:@
initWithData :: (IsAVMusicUserEvent avMusicUserEvent, IsNSData data_) => avMusicUserEvent -> data_ -> IO (Id AVMusicUserEvent)
initWithData avMusicUserEvent  data_ =
withObjCPtr data_ $ \raw_data_ ->
    sendMsg avMusicUserEvent (mkSelector "initWithData:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ())] >>= ownedObject . castPtr

-- | sizeInBytes
--
-- The size of the data associated with this user event.
--
-- ObjC selector: @- sizeInBytes@
sizeInBytes :: IsAVMusicUserEvent avMusicUserEvent => avMusicUserEvent -> IO CUInt
sizeInBytes avMusicUserEvent  =
  sendMsg avMusicUserEvent (mkSelector "sizeInBytes") retCUInt []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithData:@
initWithDataSelector :: Selector
initWithDataSelector = mkSelector "initWithData:"

-- | @Selector@ for @sizeInBytes@
sizeInBytesSelector :: Selector
sizeInBytesSelector = mkSelector "sizeInBytes"

