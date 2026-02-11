{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A participant in a coordinated playback group connected through AVPlaybackCoordinator.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVCoordinatedPlaybackParticipant@.
module ObjC.AVFoundation.AVCoordinatedPlaybackParticipant
  ( AVCoordinatedPlaybackParticipant
  , IsAVCoordinatedPlaybackParticipant(..)
  , suspensionReasons
  , readyToPlay
  , identifier
  , suspensionReasonsSelector
  , readyToPlaySelector
  , identifierSelector


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

-- | The reason, if any, this participant is currently not participating in coordinated playback.
--
-- ObjC selector: @- suspensionReasons@
suspensionReasons :: IsAVCoordinatedPlaybackParticipant avCoordinatedPlaybackParticipant => avCoordinatedPlaybackParticipant -> IO (Id NSArray)
suspensionReasons avCoordinatedPlaybackParticipant  =
  sendMsg avCoordinatedPlaybackParticipant (mkSelector "suspensionReasons") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | YES if the participant is ready to play.
--
-- ObjC selector: @- readyToPlay@
readyToPlay :: IsAVCoordinatedPlaybackParticipant avCoordinatedPlaybackParticipant => avCoordinatedPlaybackParticipant -> IO Bool
readyToPlay avCoordinatedPlaybackParticipant  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCoordinatedPlaybackParticipant (mkSelector "readyToPlay") retCULong []

-- | A unique id for the participant.
--
-- Use this identifier to distinguish participants.
--
-- ObjC selector: @- identifier@
identifier :: IsAVCoordinatedPlaybackParticipant avCoordinatedPlaybackParticipant => avCoordinatedPlaybackParticipant -> IO (Id NSUUID)
identifier avCoordinatedPlaybackParticipant  =
  sendMsg avCoordinatedPlaybackParticipant (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @suspensionReasons@
suspensionReasonsSelector :: Selector
suspensionReasonsSelector = mkSelector "suspensionReasons"

-- | @Selector@ for @readyToPlay@
readyToPlaySelector :: Selector
readyToPlaySelector = mkSelector "readyToPlay"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

