{-# LANGUAGE DataKinds #-}
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
  , addedIdentitiesSelector
  , collaborationMetadataSelector
  , initSelector
  , newSelector
  , removedIdentitiesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SharedWithYouCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsSWUpdateCollaborationParticipantsAction swUpdateCollaborationParticipantsAction => swUpdateCollaborationParticipantsAction -> IO (Id SWUpdateCollaborationParticipantsAction)
init_ swUpdateCollaborationParticipantsAction =
  sendOwnedMessage swUpdateCollaborationParticipantsAction initSelector

-- | @+ new@
new :: IO (Id SWUpdateCollaborationParticipantsAction)
new  =
  do
    cls' <- getRequiredClass "SWUpdateCollaborationParticipantsAction"
    sendOwnedClassMessage cls' newSelector

-- | @- collaborationMetadata@
collaborationMetadata :: IsSWUpdateCollaborationParticipantsAction swUpdateCollaborationParticipantsAction => swUpdateCollaborationParticipantsAction -> IO (Id SWCollaborationMetadata)
collaborationMetadata swUpdateCollaborationParticipantsAction =
  sendMessage swUpdateCollaborationParticipantsAction collaborationMetadataSelector

-- | @- addedIdentities@
addedIdentities :: IsSWUpdateCollaborationParticipantsAction swUpdateCollaborationParticipantsAction => swUpdateCollaborationParticipantsAction -> IO (Id NSArray)
addedIdentities swUpdateCollaborationParticipantsAction =
  sendMessage swUpdateCollaborationParticipantsAction addedIdentitiesSelector

-- | @- removedIdentities@
removedIdentities :: IsSWUpdateCollaborationParticipantsAction swUpdateCollaborationParticipantsAction => swUpdateCollaborationParticipantsAction -> IO (Id NSArray)
removedIdentities swUpdateCollaborationParticipantsAction =
  sendMessage swUpdateCollaborationParticipantsAction removedIdentitiesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SWUpdateCollaborationParticipantsAction)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id SWUpdateCollaborationParticipantsAction)
newSelector = mkSelector "new"

-- | @Selector@ for @collaborationMetadata@
collaborationMetadataSelector :: Selector '[] (Id SWCollaborationMetadata)
collaborationMetadataSelector = mkSelector "collaborationMetadata"

-- | @Selector@ for @addedIdentities@
addedIdentitiesSelector :: Selector '[] (Id NSArray)
addedIdentitiesSelector = mkSelector "addedIdentities"

-- | @Selector@ for @removedIdentities@
removedIdentitiesSelector :: Selector '[] (Id NSArray)
removedIdentitiesSelector = mkSelector "removedIdentities"

