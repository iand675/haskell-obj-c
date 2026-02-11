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
  , newSelector
  , initSelector
  , saveWithCompletionSelector
  , completeAllAssignedActivitiesMatchingSelector
  , removeContextSelector
  , fetchActivityForURL_completionSelector
  , sharedSelector
  , mainAppContextSelector
  , activeContextSelector
  , runningActivitySelector
  , delegateSelector
  , setDelegateSelector


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

import ObjC.ClassKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id CLSDataStore)
new  =
  do
    cls' <- getRequiredClass "CLSDataStore"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsCLSDataStore clsDataStore => clsDataStore -> IO (Id CLSDataStore)
init_ clsDataStore  =
    sendMsg clsDataStore (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Save changes made in the data store.
--
-- Save new/modified/removed contexts, activities, etc. to the local store. In case of an error -[NSError userInfo] will contain the object that caused the error under the CLSErrorObjectKey..
--
-- ObjC selector: @- saveWithCompletion:@
saveWithCompletion :: IsCLSDataStore clsDataStore => clsDataStore -> Ptr () -> IO ()
saveWithCompletion clsDataStore  completion =
    sendMsg clsDataStore (mkSelector "saveWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | Complete all assigned actvities.
--
-- Marks all of the currently active assigned activities for this contextPath as complete.
--
-- ObjC selector: @- completeAllAssignedActivitiesMatching:@
completeAllAssignedActivitiesMatching :: (IsCLSDataStore clsDataStore, IsNSArray contextPath) => clsDataStore -> contextPath -> IO ()
completeAllAssignedActivitiesMatching clsDataStore  contextPath =
  withObjCPtr contextPath $ \raw_contextPath ->
      sendMsg clsDataStore (mkSelector "completeAllAssignedActivitiesMatching:") retVoid [argPtr (castPtr raw_contextPath :: Ptr ())]

-- | Mark a context for removal.
--
-- Save to commit removal. Removal cascades and deletes all descendants.
--
-- ObjC selector: @- removeContext:@
removeContext :: (IsCLSDataStore clsDataStore, IsCLSContext context) => clsDataStore -> context -> IO ()
removeContext clsDataStore  context =
  withObjCPtr context $ \raw_context ->
      sendMsg clsDataStore (mkSelector "removeContext:") retVoid [argPtr (castPtr raw_context :: Ptr ())]

-- | Implement to fetch the current CLSActivity instance for your document to add progress to.
--
-- Gets the currently CLSActivity for the file. If no current activity exists, one will be created for you.
--
-- @url@ — File url for the document.
--
-- ObjC selector: @- fetchActivityForURL:completion:@
fetchActivityForURL_completion :: (IsCLSDataStore clsDataStore, IsNSURL url) => clsDataStore -> url -> Ptr () -> IO ()
fetchActivityForURL_completion clsDataStore  url completion =
  withObjCPtr url $ \raw_url ->
      sendMsg clsDataStore (mkSelector "fetchActivityForURL:completion:") retVoid [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | The data store provides read/write access to your app's ClassKit data.
--
-- Data written to the data store is automatically synced via iCloud across the user's devices.
--
-- ObjC selector: @+ shared@
shared :: IO (Id CLSDataStore)
shared  =
  do
    cls' <- getRequiredClass "CLSDataStore"
    sendClassMsg cls' (mkSelector "shared") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Fetch the top level context for the current app.
--
-- The main context is automatically created. Add child contexts to this context to persist them in the data store.
--
-- ObjC selector: @- mainAppContext@
mainAppContext :: IsCLSDataStore clsDataStore => clsDataStore -> IO (Id CLSContext)
mainAppContext clsDataStore  =
    sendMsg clsDataStore (mkSelector "mainAppContext") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the context that is currently active. If no context is active, this will return nil.
--
-- ObjC selector: @- activeContext@
activeContext :: IsCLSDataStore clsDataStore => clsDataStore -> IO (Id CLSContext)
activeContext clsDataStore  =
    sendMsg clsDataStore (mkSelector "activeContext") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the most recently started activity that is running.
--
-- ObjC selector: @- runningActivity@
runningActivity :: IsCLSDataStore clsDataStore => clsDataStore -> IO (Id CLSActivity)
runningActivity clsDataStore  =
    sendMsg clsDataStore (mkSelector "runningActivity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The data store delegate allows for easy population of the app's context hierarchy.
--
-- ObjC selector: @- delegate@
delegate :: IsCLSDataStore clsDataStore => clsDataStore -> IO RawId
delegate clsDataStore  =
    fmap (RawId . castPtr) $ sendMsg clsDataStore (mkSelector "delegate") (retPtr retVoid) []

-- | The data store delegate allows for easy population of the app's context hierarchy.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsCLSDataStore clsDataStore => clsDataStore -> RawId -> IO ()
setDelegate clsDataStore  value =
    sendMsg clsDataStore (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @saveWithCompletion:@
saveWithCompletionSelector :: Selector
saveWithCompletionSelector = mkSelector "saveWithCompletion:"

-- | @Selector@ for @completeAllAssignedActivitiesMatching:@
completeAllAssignedActivitiesMatchingSelector :: Selector
completeAllAssignedActivitiesMatchingSelector = mkSelector "completeAllAssignedActivitiesMatching:"

-- | @Selector@ for @removeContext:@
removeContextSelector :: Selector
removeContextSelector = mkSelector "removeContext:"

-- | @Selector@ for @fetchActivityForURL:completion:@
fetchActivityForURL_completionSelector :: Selector
fetchActivityForURL_completionSelector = mkSelector "fetchActivityForURL:completion:"

-- | @Selector@ for @shared@
sharedSelector :: Selector
sharedSelector = mkSelector "shared"

-- | @Selector@ for @mainAppContext@
mainAppContextSelector :: Selector
mainAppContextSelector = mkSelector "mainAppContext"

-- | @Selector@ for @activeContext@
activeContextSelector :: Selector
activeContextSelector = mkSelector "activeContext"

-- | @Selector@ for @runningActivity@
runningActivitySelector :: Selector
runningActivitySelector = mkSelector "runningActivity"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

