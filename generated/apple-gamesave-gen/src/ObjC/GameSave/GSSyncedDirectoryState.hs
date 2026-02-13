{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents the state and its associated properties of the directory
--
-- Use the ``state`` property to determine the validity of the other properties
--
-- Generated bindings for @GSSyncedDirectoryState@.
module ObjC.GameSave.GSSyncedDirectoryState
  ( GSSyncedDirectoryState
  , IsGSSyncedDirectoryState(..)
  , init_
  , new
  , state
  , url
  , conflictedVersions
  , error_
  , conflictedVersionsSelector
  , errorSelector
  , initSelector
  , newSelector
  , stateSelector
  , urlSelector

  -- * Enum types
  , GSSyncState(GSSyncState)
  , pattern GSSyncStateReady
  , pattern GSSyncStateOffline
  , pattern GSSyncStateLocal
  , pattern GSSyncStateSyncing
  , pattern GSSyncStateConflicted
  , pattern GSSyncStateError
  , pattern GSSyncStateClosed

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameSave.Internal.Classes
import ObjC.GameSave.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsGSSyncedDirectoryState gsSyncedDirectoryState => gsSyncedDirectoryState -> IO (Id GSSyncedDirectoryState)
init_ gsSyncedDirectoryState =
  sendOwnedMessage gsSyncedDirectoryState initSelector

-- | @- new@
new :: IsGSSyncedDirectoryState gsSyncedDirectoryState => gsSyncedDirectoryState -> IO (Id GSSyncedDirectoryState)
new gsSyncedDirectoryState =
  sendOwnedMessage gsSyncedDirectoryState newSelector

-- | Specifies the current state of the directory
--
-- ObjC selector: @- state@
state :: IsGSSyncedDirectoryState gsSyncedDirectoryState => gsSyncedDirectoryState -> IO GSSyncState
state gsSyncedDirectoryState =
  sendMessage gsSyncedDirectoryState stateSelector

-- | The URL of a directory to read and write game-save data in.
--
-- This property's value is @nil@ unless the state is @GSSyncStateReady@, @GSSyncStateOffline@, or @GSSyncStateLocal@.
--
-- ObjC selector: @- url@
url :: IsGSSyncedDirectoryState gsSyncedDirectoryState => gsSyncedDirectoryState -> IO (Id NSURL)
url gsSyncedDirectoryState =
  sendMessage gsSyncedDirectoryState urlSelector

-- | The conflicting versions.
--
-- If you're implementing your own conflict resolution, read all of the conflicting versions, and modify one of them to incorporate the state and changes from the others. Then call ``GSSyncedDirectory/resolveConflictsWithVersion:``, passing that version.
--
-- This property's value is @nil@ unless the state is @GSSyncStateConflicted@.
--
-- ObjC selector: @- conflictedVersions@
conflictedVersions :: IsGSSyncedDirectoryState gsSyncedDirectoryState => gsSyncedDirectoryState -> IO (Id NSArray)
conflictedVersions gsSyncedDirectoryState =
  sendMessage gsSyncedDirectoryState conflictedVersionsSelector

-- | The error preventing you from using the directory.
--
-- This property's value is @nil@ unless the state is @GSSyncStateError@.
--
-- ObjC selector: @- error@
error_ :: IsGSSyncedDirectoryState gsSyncedDirectoryState => gsSyncedDirectoryState -> IO (Id NSError)
error_ gsSyncedDirectoryState =
  sendMessage gsSyncedDirectoryState errorSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id GSSyncedDirectoryState)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id GSSyncedDirectoryState)
newSelector = mkSelector "new"

-- | @Selector@ for @state@
stateSelector :: Selector '[] GSSyncState
stateSelector = mkSelector "state"

-- | @Selector@ for @url@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "url"

-- | @Selector@ for @conflictedVersions@
conflictedVersionsSelector :: Selector '[] (Id NSArray)
conflictedVersionsSelector = mkSelector "conflictedVersions"

-- | @Selector@ for @error@
errorSelector :: Selector '[] (Id NSError)
errorSelector = mkSelector "error"

