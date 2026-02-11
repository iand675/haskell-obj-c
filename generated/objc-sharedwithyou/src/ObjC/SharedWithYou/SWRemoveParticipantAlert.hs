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
  , showAlertWithParticipant_highlight_inWindowSelector
  , initSelector
  , newSelector


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

import ObjC.SharedWithYou.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.SharedWithYouCore.Internal.Classes

-- | @+ showAlertWithParticipant:highlight:inWindow:@
showAlertWithParticipant_highlight_inWindow :: (IsSWPerson participant, IsSWCollaborationHighlight highlight, IsNSWindow window) => participant -> highlight -> window -> IO ()
showAlertWithParticipant_highlight_inWindow participant highlight window =
  do
    cls' <- getRequiredClass "SWRemoveParticipantAlert"
    withObjCPtr participant $ \raw_participant ->
      withObjCPtr highlight $ \raw_highlight ->
        withObjCPtr window $ \raw_window ->
          sendClassMsg cls' (mkSelector "showAlertWithParticipant:highlight:inWindow:") retVoid [argPtr (castPtr raw_participant :: Ptr ()), argPtr (castPtr raw_highlight :: Ptr ()), argPtr (castPtr raw_window :: Ptr ())]

-- | @- init@
init_ :: IsSWRemoveParticipantAlert swRemoveParticipantAlert => swRemoveParticipantAlert -> IO (Id SWRemoveParticipantAlert)
init_ swRemoveParticipantAlert  =
  sendMsg swRemoveParticipantAlert (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id SWRemoveParticipantAlert)
new  =
  do
    cls' <- getRequiredClass "SWRemoveParticipantAlert"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @showAlertWithParticipant:highlight:inWindow:@
showAlertWithParticipant_highlight_inWindowSelector :: Selector
showAlertWithParticipant_highlight_inWindowSelector = mkSelector "showAlertWithParticipant:highlight:inWindow:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

