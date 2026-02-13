{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INAddTasksIntentResponse@.
module ObjC.Intents.INAddTasksIntentResponse
  ( INAddTasksIntentResponse
  , IsINAddTasksIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , modifiedTaskList
  , setModifiedTaskList
  , addedTasks
  , setAddedTasks
  , addedTasksSelector
  , codeSelector
  , initSelector
  , initWithCode_userActivitySelector
  , modifiedTaskListSelector
  , setAddedTasksSelector
  , setModifiedTaskListSelector

  -- * Enum types
  , INAddTasksIntentResponseCode(INAddTasksIntentResponseCode)
  , pattern INAddTasksIntentResponseCodeUnspecified
  , pattern INAddTasksIntentResponseCodeReady
  , pattern INAddTasksIntentResponseCodeInProgress
  , pattern INAddTasksIntentResponseCodeSuccess
  , pattern INAddTasksIntentResponseCodeFailure
  , pattern INAddTasksIntentResponseCodeFailureRequiringAppLaunch

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
init_ :: IsINAddTasksIntentResponse inAddTasksIntentResponse => inAddTasksIntentResponse -> IO RawId
init_ inAddTasksIntentResponse =
  sendOwnedMessage inAddTasksIntentResponse initSelector

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINAddTasksIntentResponse inAddTasksIntentResponse, IsNSUserActivity userActivity) => inAddTasksIntentResponse -> INAddTasksIntentResponseCode -> userActivity -> IO (Id INAddTasksIntentResponse)
initWithCode_userActivity inAddTasksIntentResponse code userActivity =
  sendOwnedMessage inAddTasksIntentResponse initWithCode_userActivitySelector code (toNSUserActivity userActivity)

-- | @- code@
code :: IsINAddTasksIntentResponse inAddTasksIntentResponse => inAddTasksIntentResponse -> IO INAddTasksIntentResponseCode
code inAddTasksIntentResponse =
  sendMessage inAddTasksIntentResponse codeSelector

-- | @- modifiedTaskList@
modifiedTaskList :: IsINAddTasksIntentResponse inAddTasksIntentResponse => inAddTasksIntentResponse -> IO (Id INTaskList)
modifiedTaskList inAddTasksIntentResponse =
  sendMessage inAddTasksIntentResponse modifiedTaskListSelector

-- | @- setModifiedTaskList:@
setModifiedTaskList :: (IsINAddTasksIntentResponse inAddTasksIntentResponse, IsINTaskList value) => inAddTasksIntentResponse -> value -> IO ()
setModifiedTaskList inAddTasksIntentResponse value =
  sendMessage inAddTasksIntentResponse setModifiedTaskListSelector (toINTaskList value)

-- | @- addedTasks@
addedTasks :: IsINAddTasksIntentResponse inAddTasksIntentResponse => inAddTasksIntentResponse -> IO (Id NSArray)
addedTasks inAddTasksIntentResponse =
  sendMessage inAddTasksIntentResponse addedTasksSelector

-- | @- setAddedTasks:@
setAddedTasks :: (IsINAddTasksIntentResponse inAddTasksIntentResponse, IsNSArray value) => inAddTasksIntentResponse -> value -> IO ()
setAddedTasks inAddTasksIntentResponse value =
  sendMessage inAddTasksIntentResponse setAddedTasksSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector '[INAddTasksIntentResponseCode, Id NSUserActivity] (Id INAddTasksIntentResponse)
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INAddTasksIntentResponseCode
codeSelector = mkSelector "code"

-- | @Selector@ for @modifiedTaskList@
modifiedTaskListSelector :: Selector '[] (Id INTaskList)
modifiedTaskListSelector = mkSelector "modifiedTaskList"

-- | @Selector@ for @setModifiedTaskList:@
setModifiedTaskListSelector :: Selector '[Id INTaskList] ()
setModifiedTaskListSelector = mkSelector "setModifiedTaskList:"

-- | @Selector@ for @addedTasks@
addedTasksSelector :: Selector '[] (Id NSArray)
addedTasksSelector = mkSelector "addedTasks"

-- | @Selector@ for @setAddedTasks:@
setAddedTasksSelector :: Selector '[Id NSArray] ()
setAddedTasksSelector = mkSelector "setAddedTasks:"

