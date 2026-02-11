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
  , initSelector
  , newSelector
  , originatorSelector
  , expectedCurrentItemIdentifierSelector


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
init_ :: IsAVDelegatingPlaybackCoordinatorPlaybackControlCommand avDelegatingPlaybackCoordinatorPlaybackControlCommand => avDelegatingPlaybackCoordinatorPlaybackControlCommand -> IO (Id AVDelegatingPlaybackCoordinatorPlaybackControlCommand)
init_ avDelegatingPlaybackCoordinatorPlaybackControlCommand  =
  sendMsg avDelegatingPlaybackCoordinatorPlaybackControlCommand (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVDelegatingPlaybackCoordinatorPlaybackControlCommand)
new  =
  do
    cls' <- getRequiredClass "AVDelegatingPlaybackCoordinatorPlaybackControlCommand"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The participant causing this command to be issued.
--
-- Only commands issued on behalf of another participant will contain an originator. Commands caused by local requests, e.g., requests to coordinate a rate change, will not contain an originator. Similarly, re-application of older commands, e.g., in response to a call to [AVDelegatingPlaybackCoordinator reapplyCurrentItemStateToPlaybackControlDelegate], will not contain an originator. If the originator is non-nil, it may be appropriate to show UI indicating someone else's action.
--
-- ObjC selector: @- originator@
originator :: IsAVDelegatingPlaybackCoordinatorPlaybackControlCommand avDelegatingPlaybackCoordinatorPlaybackControlCommand => avDelegatingPlaybackCoordinatorPlaybackControlCommand -> IO (Id AVCoordinatedPlaybackParticipant)
originator avDelegatingPlaybackCoordinatorPlaybackControlCommand  =
  sendMsg avDelegatingPlaybackCoordinatorPlaybackControlCommand (mkSelector "originator") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Indicates the item this command was issued for.
--
-- Commands are always meant for the current item. A command handler should verify that the identifier of its current item matches this identifier. If it doesn't this command is obsolete and should be ignored. Note that any completion handler of the delegate method issuing the command must still be invoked.
--
-- ObjC selector: @- expectedCurrentItemIdentifier@
expectedCurrentItemIdentifier :: IsAVDelegatingPlaybackCoordinatorPlaybackControlCommand avDelegatingPlaybackCoordinatorPlaybackControlCommand => avDelegatingPlaybackCoordinatorPlaybackControlCommand -> IO (Id NSString)
expectedCurrentItemIdentifier avDelegatingPlaybackCoordinatorPlaybackControlCommand  =
  sendMsg avDelegatingPlaybackCoordinatorPlaybackControlCommand (mkSelector "expectedCurrentItemIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @originator@
originatorSelector :: Selector
originatorSelector = mkSelector "originator"

-- | @Selector@ for @expectedCurrentItemIdentifier@
expectedCurrentItemIdentifierSelector :: Selector
expectedCurrentItemIdentifierSelector = mkSelector "expectedCurrentItemIdentifier"

