{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , codeSelector
  , initSelector
  , initWithCode_userActivitySelector
  , notesSelector
  , setNotesSelector
  , setSortTypeSelector
  , setTaskListsSelector
  , setTasksSelector
  , sortTypeSelector
  , taskListsSelector
  , tasksSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINSearchForNotebookItemsIntentResponse inSearchForNotebookItemsIntentResponse => inSearchForNotebookItemsIntentResponse -> IO RawId
init_ inSearchForNotebookItemsIntentResponse =
  sendOwnedMessage inSearchForNotebookItemsIntentResponse initSelector

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINSearchForNotebookItemsIntentResponse inSearchForNotebookItemsIntentResponse, IsNSUserActivity userActivity) => inSearchForNotebookItemsIntentResponse -> INSearchForNotebookItemsIntentResponseCode -> userActivity -> IO (Id INSearchForNotebookItemsIntentResponse)
initWithCode_userActivity inSearchForNotebookItemsIntentResponse code userActivity =
  sendOwnedMessage inSearchForNotebookItemsIntentResponse initWithCode_userActivitySelector code (toNSUserActivity userActivity)

-- | @- code@
code :: IsINSearchForNotebookItemsIntentResponse inSearchForNotebookItemsIntentResponse => inSearchForNotebookItemsIntentResponse -> IO INSearchForNotebookItemsIntentResponseCode
code inSearchForNotebookItemsIntentResponse =
  sendMessage inSearchForNotebookItemsIntentResponse codeSelector

-- | @- notes@
notes :: IsINSearchForNotebookItemsIntentResponse inSearchForNotebookItemsIntentResponse => inSearchForNotebookItemsIntentResponse -> IO (Id NSArray)
notes inSearchForNotebookItemsIntentResponse =
  sendMessage inSearchForNotebookItemsIntentResponse notesSelector

-- | @- setNotes:@
setNotes :: (IsINSearchForNotebookItemsIntentResponse inSearchForNotebookItemsIntentResponse, IsNSArray value) => inSearchForNotebookItemsIntentResponse -> value -> IO ()
setNotes inSearchForNotebookItemsIntentResponse value =
  sendMessage inSearchForNotebookItemsIntentResponse setNotesSelector (toNSArray value)

-- | @- taskLists@
taskLists :: IsINSearchForNotebookItemsIntentResponse inSearchForNotebookItemsIntentResponse => inSearchForNotebookItemsIntentResponse -> IO (Id NSArray)
taskLists inSearchForNotebookItemsIntentResponse =
  sendMessage inSearchForNotebookItemsIntentResponse taskListsSelector

-- | @- setTaskLists:@
setTaskLists :: (IsINSearchForNotebookItemsIntentResponse inSearchForNotebookItemsIntentResponse, IsNSArray value) => inSearchForNotebookItemsIntentResponse -> value -> IO ()
setTaskLists inSearchForNotebookItemsIntentResponse value =
  sendMessage inSearchForNotebookItemsIntentResponse setTaskListsSelector (toNSArray value)

-- | @- tasks@
tasks :: IsINSearchForNotebookItemsIntentResponse inSearchForNotebookItemsIntentResponse => inSearchForNotebookItemsIntentResponse -> IO (Id NSArray)
tasks inSearchForNotebookItemsIntentResponse =
  sendMessage inSearchForNotebookItemsIntentResponse tasksSelector

-- | @- setTasks:@
setTasks :: (IsINSearchForNotebookItemsIntentResponse inSearchForNotebookItemsIntentResponse, IsNSArray value) => inSearchForNotebookItemsIntentResponse -> value -> IO ()
setTasks inSearchForNotebookItemsIntentResponse value =
  sendMessage inSearchForNotebookItemsIntentResponse setTasksSelector (toNSArray value)

-- | @- sortType@
sortType :: IsINSearchForNotebookItemsIntentResponse inSearchForNotebookItemsIntentResponse => inSearchForNotebookItemsIntentResponse -> IO INSortType
sortType inSearchForNotebookItemsIntentResponse =
  sendMessage inSearchForNotebookItemsIntentResponse sortTypeSelector

-- | @- setSortType:@
setSortType :: IsINSearchForNotebookItemsIntentResponse inSearchForNotebookItemsIntentResponse => inSearchForNotebookItemsIntentResponse -> INSortType -> IO ()
setSortType inSearchForNotebookItemsIntentResponse value =
  sendMessage inSearchForNotebookItemsIntentResponse setSortTypeSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector '[INSearchForNotebookItemsIntentResponseCode, Id NSUserActivity] (Id INSearchForNotebookItemsIntentResponse)
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INSearchForNotebookItemsIntentResponseCode
codeSelector = mkSelector "code"

-- | @Selector@ for @notes@
notesSelector :: Selector '[] (Id NSArray)
notesSelector = mkSelector "notes"

-- | @Selector@ for @setNotes:@
setNotesSelector :: Selector '[Id NSArray] ()
setNotesSelector = mkSelector "setNotes:"

-- | @Selector@ for @taskLists@
taskListsSelector :: Selector '[] (Id NSArray)
taskListsSelector = mkSelector "taskLists"

-- | @Selector@ for @setTaskLists:@
setTaskListsSelector :: Selector '[Id NSArray] ()
setTaskListsSelector = mkSelector "setTaskLists:"

-- | @Selector@ for @tasks@
tasksSelector :: Selector '[] (Id NSArray)
tasksSelector = mkSelector "tasks"

-- | @Selector@ for @setTasks:@
setTasksSelector :: Selector '[Id NSArray] ()
setTasksSelector = mkSelector "setTasks:"

-- | @Selector@ for @sortType@
sortTypeSelector :: Selector '[] INSortType
sortTypeSelector = mkSelector "sortType"

-- | @Selector@ for @setSortType:@
setSortTypeSelector :: Selector '[INSortType] ()
setSortTypeSelector = mkSelector "setSortType:"

