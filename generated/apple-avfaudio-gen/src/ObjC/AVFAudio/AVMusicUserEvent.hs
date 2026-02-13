{-# LANGUAGE DataKinds #-}
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
-- @data@ — An NSData object containing the contents to be returned via the AVMusicTrack's user callback.
--
-- ObjC selector: @- initWithData:@
initWithData :: (IsAVMusicUserEvent avMusicUserEvent, IsNSData data_) => avMusicUserEvent -> data_ -> IO (Id AVMusicUserEvent)
initWithData avMusicUserEvent data_ =
  sendOwnedMessage avMusicUserEvent initWithDataSelector (toNSData data_)

-- | sizeInBytes
--
-- The size of the data associated with this user event.
--
-- ObjC selector: @- sizeInBytes@
sizeInBytes :: IsAVMusicUserEvent avMusicUserEvent => avMusicUserEvent -> IO CUInt
sizeInBytes avMusicUserEvent =
  sendMessage avMusicUserEvent sizeInBytesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithData:@
initWithDataSelector :: Selector '[Id NSData] (Id AVMusicUserEvent)
initWithDataSelector = mkSelector "initWithData:"

-- | @Selector@ for @sizeInBytes@
sizeInBytesSelector :: Selector '[] CUInt
sizeInBytesSelector = mkSelector "sizeInBytes"

