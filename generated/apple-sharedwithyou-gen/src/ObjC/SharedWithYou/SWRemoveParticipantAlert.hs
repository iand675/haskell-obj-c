{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SWRemoveParticipantAlert@.
module ObjC.SharedWithYou.SWRemoveParticipantAlert
  ( SWRemoveParticipantAlert
  , IsSWRemoveParticipantAlert(..)
  , showAlertWithParticipant_highlight_inWindow
  , init_
  , new
  , initSelector
  , newSelector
  , showAlertWithParticipant_highlight_inWindowSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SharedWithYou.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.SharedWithYouCore.Internal.Classes

-- | @+ showAlertWithParticipant:highlight:inWindow:@
showAlertWithParticipant_highlight_inWindow :: (IsSWPerson participant, IsSWCollaborationHighlight highlight, IsNSWindow window) => participant -> highlight -> window -> IO ()
showAlertWithParticipant_highlight_inWindow participant highlight window =
  do
    cls' <- getRequiredClass "SWRemoveParticipantAlert"
    sendClassMessage cls' showAlertWithParticipant_highlight_inWindowSelector (toSWPerson participant) (toSWCollaborationHighlight highlight) (toNSWindow window)

-- | @- init@
init_ :: IsSWRemoveParticipantAlert swRemoveParticipantAlert => swRemoveParticipantAlert -> IO (Id SWRemoveParticipantAlert)
init_ swRemoveParticipantAlert =
  sendOwnedMessage swRemoveParticipantAlert initSelector

-- | @+ new@
new :: IO (Id SWRemoveParticipantAlert)
new  =
  do
    cls' <- getRequiredClass "SWRemoveParticipantAlert"
    sendOwnedClassMessage cls' newSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @showAlertWithParticipant:highlight:inWindow:@
showAlertWithParticipant_highlight_inWindowSelector :: Selector '[Id SWPerson, Id SWCollaborationHighlight, Id NSWindow] ()
showAlertWithParticipant_highlight_inWindowSelector = mkSelector "showAlertWithParticipant:highlight:inWindow:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SWRemoveParticipantAlert)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id SWRemoveParticipantAlert)
newSelector = mkSelector "new"

