{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSearchForNotebookItemsIntentResponse@.
module ObjC.Intents.INSearchForNotebookItemsIntentResponse
  ( INSearchForNotebookItemsIntentResponse
  , IsINSearchForNotebookItemsIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , notes
  , setNotes
  , taskLists
  , setTaskLists
  , tasks
  , setTasks
  , sortType
  , setSortType
  , initSelector
  , initWithCode_userActivitySelector
  , codeSelector
  , notesSelector
  , setNotesSelector
  , taskListsSelector
  , setTaskListsSelector
  , tasksSelector
  , setTasksSelector
  , sortTypeSelector
  , setSortTypeSelector

  -- * Enum types
  , INSearchForNotebookItemsIntentResponseCode(INSearchForNotebookItemsIntentResponseCode)
  , pattern INSearchForNotebookItemsIntentResponseCodeUnspecified
  , pattern INSearchForNotebookItemsIntentResponseCodeReady
  , pattern INSearchForNotebookItemsIntentResponseCodeInProgress
  , pattern INSearchForNotebookItemsIntentResponseCodeSuccess
  , pattern INSearchForNotebookItemsIntentResponseCodeFailure
  , pattern INSearchForNotebookItemsIntentResponseCodeFailureRequiringAppLaunch
  , INSortType(INSortType)
  , pattern INSortTypeUnknown
  , pattern INSortTypeAsIs
  , pattern INSortTypeByDate

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

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINSearchForNotebookItemsIntentResponse inSearchForNotebookItemsIntentResponse => inSearchForNotebookItemsIntentResponse -> IO RawId
init_ inSearchForNotebookItemsIntentResponse  =
  fmap (RawId . castPtr) $ sendMsg inSearchForNotebookItemsIntentResponse (mkSelector "init") (retPtr retVoid) []

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINSearchForNotebookItemsIntentResponse inSearchForNotebookItemsIntentResponse, IsNSUserActivity userActivity) => inSearchForNotebookItemsIntentResponse -> INSearchForNotebookItemsIntentResponseCode -> userActivity -> IO (Id INSearchForNotebookItemsIntentResponse)
initWithCode_userActivity inSearchForNotebookItemsIntentResponse  code userActivity =
withObjCPtr userActivity $ \raw_userActivity ->
    sendMsg inSearchForNotebookItemsIntentResponse (mkSelector "initWithCode:userActivity:") (retPtr retVoid) [argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- code@
code :: IsINSearchForNotebookItemsIntentResponse inSearchForNotebookItemsIntentResponse => inSearchForNotebookItemsIntentResponse -> IO INSearchForNotebookItemsIntentResponseCode
code inSearchForNotebookItemsIntentResponse  =
  fmap (coerce :: CLong -> INSearchForNotebookItemsIntentResponseCode) $ sendMsg inSearchForNotebookItemsIntentResponse (mkSelector "code") retCLong []

-- | @- notes@
notes :: IsINSearchForNotebookItemsIntentResponse inSearchForNotebookItemsIntentResponse => inSearchForNotebookItemsIntentResponse -> IO (Id NSArray)
notes inSearchForNotebookItemsIntentResponse  =
  sendMsg inSearchForNotebookItemsIntentResponse (mkSelector "notes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNotes:@
setNotes :: (IsINSearchForNotebookItemsIntentResponse inSearchForNotebookItemsIntentResponse, IsNSArray value) => inSearchForNotebookItemsIntentResponse -> value -> IO ()
setNotes inSearchForNotebookItemsIntentResponse  value =
withObjCPtr value $ \raw_value ->
    sendMsg inSearchForNotebookItemsIntentResponse (mkSelector "setNotes:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- taskLists@
taskLists :: IsINSearchForNotebookItemsIntentResponse inSearchForNotebookItemsIntentResponse => inSearchForNotebookItemsIntentResponse -> IO (Id NSArray)
taskLists inSearchForNotebookItemsIntentResponse  =
  sendMsg inSearchForNotebookItemsIntentResponse (mkSelector "taskLists") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTaskLists:@
setTaskLists :: (IsINSearchForNotebookItemsIntentResponse inSearchForNotebookItemsIntentResponse, IsNSArray value) => inSearchForNotebookItemsIntentResponse -> value -> IO ()
setTaskLists inSearchForNotebookItemsIntentResponse  value =
withObjCPtr value $ \raw_value ->
    sendMsg inSearchForNotebookItemsIntentResponse (mkSelector "setTaskLists:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- tasks@
tasks :: IsINSearchForNotebookItemsIntentResponse inSearchForNotebookItemsIntentResponse => inSearchForNotebookItemsIntentResponse -> IO (Id NSArray)
tasks inSearchForNotebookItemsIntentResponse  =
  sendMsg inSearchForNotebookItemsIntentResponse (mkSelector "tasks") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTasks:@
setTasks :: (IsINSearchForNotebookItemsIntentResponse inSearchForNotebookItemsIntentResponse, IsNSArray value) => inSearchForNotebookItemsIntentResponse -> value -> IO ()
setTasks inSearchForNotebookItemsIntentResponse  value =
withObjCPtr value $ \raw_value ->
    sendMsg inSearchForNotebookItemsIntentResponse (mkSelector "setTasks:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- sortType@
sortType :: IsINSearchForNotebookItemsIntentResponse inSearchForNotebookItemsIntentResponse => inSearchForNotebookItemsIntentResponse -> IO INSortType
sortType inSearchForNotebookItemsIntentResponse  =
  fmap (coerce :: CLong -> INSortType) $ sendMsg inSearchForNotebookItemsIntentResponse (mkSelector "sortType") retCLong []

-- | @- setSortType:@
setSortType :: IsINSearchForNotebookItemsIntentResponse inSearchForNotebookItemsIntentResponse => inSearchForNotebookItemsIntentResponse -> INSortType -> IO ()
setSortType inSearchForNotebookItemsIntentResponse  value =
  sendMsg inSearchForNotebookItemsIntentResponse (mkSelector "setSortType:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector
codeSelector = mkSelector "code"

-- | @Selector@ for @notes@
notesSelector :: Selector
notesSelector = mkSelector "notes"

-- | @Selector@ for @setNotes:@
setNotesSelector :: Selector
setNotesSelector = mkSelector "setNotes:"

-- | @Selector@ for @taskLists@
taskListsSelector :: Selector
taskListsSelector = mkSelector "taskLists"

-- | @Selector@ for @setTaskLists:@
setTaskListsSelector :: Selector
setTaskListsSelector = mkSelector "setTaskLists:"

-- | @Selector@ for @tasks@
tasksSelector :: Selector
tasksSelector = mkSelector "tasks"

-- | @Selector@ for @setTasks:@
setTasksSelector :: Selector
setTasksSelector = mkSelector "setTasks:"

-- | @Selector@ for @sortType@
sortTypeSelector :: Selector
sortTypeSelector = mkSelector "sortType"

-- | @Selector@ for @setSortType:@
setSortTypeSelector :: Selector
setSortTypeSelector = mkSelector "setSortType:"

