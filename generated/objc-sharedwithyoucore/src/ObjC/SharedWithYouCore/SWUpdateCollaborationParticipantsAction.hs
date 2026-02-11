{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SWUpdateCollaborationParticipantsAction@.
module ObjC.SharedWithYouCore.SWUpdateCollaborationParticipantsAction
  ( SWUpdateCollaborationParticipantsAction
  , IsSWUpdateCollaborationParticipantsAction(..)
  , init_
  , new
  , collaborationMetadata
  , addedIdentities
  , removedIdentities
  , initSelector
  , newSelector
  , collaborationMetadataSelector
  , addedIdentitiesSelector
  , removedIdentitiesSelector


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

import ObjC.SharedWithYouCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsSWUpdateCollaborationParticipantsAction swUpdateCollaborationParticipantsAction => swUpdateCollaborationParticipantsAction -> IO (Id SWUpdateCollaborationParticipantsAction)
init_ swUpdateCollaborationParticipantsAction  =
  sendMsg swUpdateCollaborationParticipantsAction (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id SWUpdateCollaborationParticipantsAction)
new  =
  do
    cls' <- getRequiredClass "SWUpdateCollaborationParticipantsAction"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- collaborationMetadata@
collaborationMetadata :: IsSWUpdateCollaborationParticipantsAction swUpdateCollaborationParticipantsAction => swUpdateCollaborationParticipantsAction -> IO (Id SWCollaborationMetadata)
collaborationMetadata swUpdateCollaborationParticipantsAction  =
  sendMsg swUpdateCollaborationParticipantsAction (mkSelector "collaborationMetadata") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- addedIdentities@
addedIdentities :: IsSWUpdateCollaborationParticipantsAction swUpdateCollaborationParticipantsAction => swUpdateCollaborationParticipantsAction -> IO (Id NSArray)
addedIdentities swUpdateCollaborationParticipantsAction  =
  sendMsg swUpdateCollaborationParticipantsAction (mkSelector "addedIdentities") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- removedIdentities@
removedIdentities :: IsSWUpdateCollaborationParticipantsAction swUpdateCollaborationParticipantsAction => swUpdateCollaborationParticipantsAction -> IO (Id NSArray)
removedIdentities swUpdateCollaborationParticipantsAction  =
  sendMsg swUpdateCollaborationParticipantsAction (mkSelector "removedIdentities") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @collaborationMetadata@
collaborationMetadataSelector :: Selector
collaborationMetadataSelector = mkSelector "collaborationMetadata"

-- | @Selector@ for @addedIdentities@
addedIdentitiesSelector :: Selector
addedIdentitiesSelector = mkSelector "addedIdentities"

-- | @Selector@ for @removedIdentities@
removedIdentitiesSelector :: Selector
removedIdentitiesSelector = mkSelector "removedIdentities"

