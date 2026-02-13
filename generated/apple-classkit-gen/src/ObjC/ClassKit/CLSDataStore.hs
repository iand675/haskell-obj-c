{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The data store maintains and syncs your app's contexts.
--
-- Generated bindings for @CLSDataStore@.
module ObjC.ClassKit.CLSDataStore
  ( CLSDataStore
  , IsCLSDataStore(..)
  , new
  , init_
  , saveWithCompletion
  , completeAllAssignedActivitiesMatching
  , removeContext
  , fetchActivityForURL_completion
  , shared
  , mainAppContext
  , activeContext
  , runningActivity
  , delegate
  , setDelegate
  , activeContextSelector
  , completeAllAssignedActivitiesMatchingSelector
  , delegateSelector
  , fetchActivityForURL_completionSelector
  , initSelector
  , mainAppContextSelector
  , newSelector
  , removeContextSelector
  , runningActivitySelector
  , saveWithCompletionSelector
  , setDelegateSelector
  , sharedSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ClassKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id CLSDataStore)
new  =
  do
    cls' <- getRequiredClass "CLSDataStore"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsCLSDataStore clsDataStore => clsDataStore -> IO (Id CLSDataStore)
init_ clsDataStore =
  sendOwnedMessage clsDataStore initSelector

-- | Save changes made in the data store.
--
-- Save new/modified/removed contexts, activities, etc. to the local store. In case of an error -[NSError userInfo] will contain the object that caused the error under the CLSErrorObjectKey..
--
-- ObjC selector: @- saveWithCompletion:@
saveWithCompletion :: IsCLSDataStore clsDataStore => clsDataStore -> Ptr () -> IO ()
saveWithCompletion clsDataStore completion =
  sendMessage clsDataStore saveWithCompletionSelector completion

-- | Complete all assigned actvities.
--
-- Marks all of the currently active assigned activities for this contextPath as complete.
--
-- ObjC selector: @- completeAllAssignedActivitiesMatching:@
completeAllAssignedActivitiesMatching :: (IsCLSDataStore clsDataStore, IsNSArray contextPath) => clsDataStore -> contextPath -> IO ()
completeAllAssignedActivitiesMatching clsDataStore contextPath =
  sendMessage clsDataStore completeAllAssignedActivitiesMatchingSelector (toNSArray contextPath)

-- | Mark a context for removal.
--
-- Save to commit removal. Removal cascades and deletes all descendants.
--
-- ObjC selector: @- removeContext:@
removeContext :: (IsCLSDataStore clsDataStore, IsCLSContext context) => clsDataStore -> context -> IO ()
removeContext clsDataStore context =
  sendMessage clsDataStore removeContextSelector (toCLSContext context)

-- | Implement to fetch the current CLSActivity instance for your document to add progress to.
--
-- Gets the currently CLSActivity for the file. If no current activity exists, one will be created for you.
--
-- @url@ — File url for the document.
--
-- ObjC selector: @- fetchActivityForURL:completion:@
fetchActivityForURL_completion :: (IsCLSDataStore clsDataStore, IsNSURL url) => clsDataStore -> url -> Ptr () -> IO ()
fetchActivityForURL_completion clsDataStore url completion =
  sendMessage clsDataStore fetchActivityForURL_completionSelector (toNSURL url) completion

-- | The data store provides read/write access to your app's ClassKit data.
--
-- Data written to the data store is automatically synced via iCloud across the user's devices.
--
-- ObjC selector: @+ shared@
shared :: IO (Id CLSDataStore)
shared  =
  do
    cls' <- getRequiredClass "CLSDataStore"
    sendClassMessage cls' sharedSelector

-- | Fetch the top level context for the current app.
--
-- The main context is automatically created. Add child contexts to this context to persist them in the data store.
--
-- ObjC selector: @- mainAppContext@
mainAppContext :: IsCLSDataStore clsDataStore => clsDataStore -> IO (Id CLSContext)
mainAppContext clsDataStore =
  sendMessage clsDataStore mainAppContextSelector

-- | Returns the context that is currently active. If no context is active, this will return nil.
--
-- ObjC selector: @- activeContext@
activeContext :: IsCLSDataStore clsDataStore => clsDataStore -> IO (Id CLSContext)
activeContext clsDataStore =
  sendMessage clsDataStore activeContextSelector

-- | Returns the most recently started activity that is running.
--
-- ObjC selector: @- runningActivity@
runningActivity :: IsCLSDataStore clsDataStore => clsDataStore -> IO (Id CLSActivity)
runningActivity clsDataStore =
  sendMessage clsDataStore runningActivitySelector

-- | The data store delegate allows for easy population of the app's context hierarchy.
--
-- ObjC selector: @- delegate@
delegate :: IsCLSDataStore clsDataStore => clsDataStore -> IO RawId
delegate clsDataStore =
  sendMessage clsDataStore delegateSelector

-- | The data store delegate allows for easy population of the app's context hierarchy.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsCLSDataStore clsDataStore => clsDataStore -> RawId -> IO ()
setDelegate clsDataStore value =
  sendMessage clsDataStore setDelegateSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CLSDataStore)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CLSDataStore)
initSelector = mkSelector "init"

-- | @Selector@ for @saveWithCompletion:@
saveWithCompletionSelector :: Selector '[Ptr ()] ()
saveWithCompletionSelector = mkSelector "saveWithCompletion:"

-- | @Selector@ for @completeAllAssignedActivitiesMatching:@
completeAllAssignedActivitiesMatchingSelector :: Selector '[Id NSArray] ()
completeAllAssignedActivitiesMatchingSelector = mkSelector "completeAllAssignedActivitiesMatching:"

-- | @Selector@ for @removeContext:@
removeContextSelector :: Selector '[Id CLSContext] ()
removeContextSelector = mkSelector "removeContext:"

-- | @Selector@ for @fetchActivityForURL:completion:@
fetchActivityForURL_completionSelector :: Selector '[Id NSURL, Ptr ()] ()
fetchActivityForURL_completionSelector = mkSelector "fetchActivityForURL:completion:"

-- | @Selector@ for @shared@
sharedSelector :: Selector '[] (Id CLSDataStore)
sharedSelector = mkSelector "shared"

-- | @Selector@ for @mainAppContext@
mainAppContextSelector :: Selector '[] (Id CLSContext)
mainAppContextSelector = mkSelector "mainAppContext"

-- | @Selector@ for @activeContext@
activeContextSelector :: Selector '[] (Id CLSContext)
activeContextSelector = mkSelector "activeContext"

-- | @Selector@ for @runningActivity@
runningActivitySelector :: Selector '[] (Id CLSActivity)
runningActivitySelector = mkSelector "runningActivity"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

