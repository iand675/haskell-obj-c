{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A playback command requesting a pause
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVDelegatingPlaybackCoordinatorPauseCommand@.
module ObjC.AVFoundation.AVDelegatingPlaybackCoordinatorPauseCommand
  ( AVDelegatingPlaybackCoordinatorPauseCommand
  , IsAVDelegatingPlaybackCoordinatorPauseCommand(..)
  , init_
  , new
  , shouldBufferInAnticipationOfPlayback
  , anticipatedPlaybackRate
  , initSelector
  , newSelector
  , shouldBufferInAnticipationOfPlaybackSelector
  , anticipatedPlaybackRateSelector


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
init_ :: IsAVDelegatingPlaybackCoordinatorPauseCommand avDelegatingPlaybackCoordinatorPauseCommand => avDelegatingPlaybackCoordinatorPauseCommand -> IO (Id AVDelegatingPlaybackCoordinatorPauseCommand)
init_ avDelegatingPlaybackCoordinatorPauseCommand  =
  sendMsg avDelegatingPlaybackCoordinatorPauseCommand (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVDelegatingPlaybackCoordinatorPauseCommand)
new  =
  do
    cls' <- getRequiredClass "AVDelegatingPlaybackCoordinatorPauseCommand"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Indicates that playback is anticipated and the player should begin buffering if necessary.
--
-- When shouldBufferInAnticipationOfPlayback is YES, some participant wants to resume playback at the rate indicated by the anticipatedPlaybackRate property. This should be treated similar to receiving a separate AVDelegatingPlaybackCoordinatorBufferingCommand. If YES, the command should only be considered complete once the player is ready to receive an AVDelegatingPlaybackCoordinatorPlayCommand with the indicated rate.
--
-- ObjC selector: @- shouldBufferInAnticipationOfPlayback@
shouldBufferInAnticipationOfPlayback :: IsAVDelegatingPlaybackCoordinatorPauseCommand avDelegatingPlaybackCoordinatorPauseCommand => avDelegatingPlaybackCoordinatorPauseCommand -> IO Bool
shouldBufferInAnticipationOfPlayback avDelegatingPlaybackCoordinatorPauseCommand  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avDelegatingPlaybackCoordinatorPauseCommand (mkSelector "shouldBufferInAnticipationOfPlayback") retCULong []

-- | The rate to prepare for if shouldBufferInAnticipationOfPlayback is YES.
--
-- ObjC selector: @- anticipatedPlaybackRate@
anticipatedPlaybackRate :: IsAVDelegatingPlaybackCoordinatorPauseCommand avDelegatingPlaybackCoordinatorPauseCommand => avDelegatingPlaybackCoordinatorPauseCommand -> IO CFloat
anticipatedPlaybackRate avDelegatingPlaybackCoordinatorPauseCommand  =
  sendMsg avDelegatingPlaybackCoordinatorPauseCommand (mkSelector "anticipatedPlaybackRate") retCFloat []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @shouldBufferInAnticipationOfPlayback@
shouldBufferInAnticipationOfPlaybackSelector :: Selector
shouldBufferInAnticipationOfPlaybackSelector = mkSelector "shouldBufferInAnticipationOfPlayback"

-- | @Selector@ for @anticipatedPlaybackRate@
anticipatedPlaybackRateSelector :: Selector
anticipatedPlaybackRateSelector = mkSelector "anticipatedPlaybackRate"

