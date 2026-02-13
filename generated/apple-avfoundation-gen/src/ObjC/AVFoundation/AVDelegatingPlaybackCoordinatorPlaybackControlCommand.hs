{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Abstract superclass for playback commands
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVDelegatingPlaybackCoordinatorPlaybackControlCommand@.
module ObjC.AVFoundation.AVDelegatingPlaybackCoordinatorPlaybackControlCommand
  ( AVDelegatingPlaybackCoordinatorPlaybackControlCommand
  , IsAVDelegatingPlaybackCoordinatorPlaybackControlCommand(..)
  , init_
  , new
  , originator
  , expectedCurrentItemIdentifier
  , expectedCurrentItemIdentifierSelector
  , initSelector
  , newSelector
  , originatorSelector


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
init_ :: IsAVDelegatingPlaybackCoordinatorPlaybackControlCommand avDelegatingPlaybackCoordinatorPlaybackControlCommand => avDelegatingPlaybackCoordinatorPlaybackControlCommand -> IO (Id AVDelegatingPlaybackCoordinatorPlaybackControlCommand)
init_ avDelegatingPlaybackCoordinatorPlaybackControlCommand =
  sendOwnedMessage avDelegatingPlaybackCoordinatorPlaybackControlCommand initSelector

-- | @+ new@
new :: IO (Id AVDelegatingPlaybackCoordinatorPlaybackControlCommand)
new  =
  do
    cls' <- getRequiredClass "AVDelegatingPlaybackCoordinatorPlaybackControlCommand"
    sendOwnedClassMessage cls' newSelector

-- | The participant causing this command to be issued.
--
-- Only commands issued on behalf of another participant will contain an originator. Commands caused by local requests, e.g., requests to coordinate a rate change, will not contain an originator. Similarly, re-application of older commands, e.g., in response to a call to [AVDelegatingPlaybackCoordinator reapplyCurrentItemStateToPlaybackControlDelegate], will not contain an originator. If the originator is non-nil, it may be appropriate to show UI indicating someone else's action.
--
-- ObjC selector: @- originator@
originator :: IsAVDelegatingPlaybackCoordinatorPlaybackControlCommand avDelegatingPlaybackCoordinatorPlaybackControlCommand => avDelegatingPlaybackCoordinatorPlaybackControlCommand -> IO (Id AVCoordinatedPlaybackParticipant)
originator avDelegatingPlaybackCoordinatorPlaybackControlCommand =
  sendMessage avDelegatingPlaybackCoordinatorPlaybackControlCommand originatorSelector

-- | Indicates the item this command was issued for.
--
-- Commands are always meant for the current item. A command handler should verify that the identifier of its current item matches this identifier. If it doesn't this command is obsolete and should be ignored. Note that any completion handler of the delegate method issuing the command must still be invoked.
--
-- ObjC selector: @- expectedCurrentItemIdentifier@
expectedCurrentItemIdentifier :: IsAVDelegatingPlaybackCoordinatorPlaybackControlCommand avDelegatingPlaybackCoordinatorPlaybackControlCommand => avDelegatingPlaybackCoordinatorPlaybackControlCommand -> IO (Id NSString)
expectedCurrentItemIdentifier avDelegatingPlaybackCoordinatorPlaybackControlCommand =
  sendMessage avDelegatingPlaybackCoordinatorPlaybackControlCommand expectedCurrentItemIdentifierSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVDelegatingPlaybackCoordinatorPlaybackControlCommand)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVDelegatingPlaybackCoordinatorPlaybackControlCommand)
newSelector = mkSelector "new"

-- | @Selector@ for @originator@
originatorSelector :: Selector '[] (Id AVCoordinatedPlaybackParticipant)
originatorSelector = mkSelector "originator"

-- | @Selector@ for @expectedCurrentItemIdentifier@
expectedCurrentItemIdentifierSelector :: Selector '[] (Id NSString)
expectedCurrentItemIdentifierSelector = mkSelector "expectedCurrentItemIdentifier"

