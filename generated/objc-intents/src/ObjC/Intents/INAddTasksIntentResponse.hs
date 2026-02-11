{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , initWithCode_userActivitySelector
  , codeSelector
  , modifiedTaskListSelector
  , setModifiedTaskListSelector
  , addedTasksSelector
  , setAddedTasksSelector

  -- * Enum types
  , INAddTasksIntentResponseCode(INAddTasksIntentResponseCode)
  , pattern INAddTasksIntentResponseCodeUnspecified
  , pattern INAddTasksIntentResponseCodeReady
  , pattern INAddTasksIntentResponseCodeInProgress
  , pattern INAddTasksIntentResponseCodeSuccess
  , pattern INAddTasksIntentResponseCodeFailure
  , pattern INAddTasksIntentResponseCodeFailureRequiringAppLaunch

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
init_ :: IsINAddTasksIntentResponse inAddTasksIntentResponse => inAddTasksIntentResponse -> IO RawId
init_ inAddTasksIntentResponse  =
  fmap (RawId . castPtr) $ sendMsg inAddTasksIntentResponse (mkSelector "init") (retPtr retVoid) []

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINAddTasksIntentResponse inAddTasksIntentResponse, IsNSUserActivity userActivity) => inAddTasksIntentResponse -> INAddTasksIntentResponseCode -> userActivity -> IO (Id INAddTasksIntentResponse)
initWithCode_userActivity inAddTasksIntentResponse  code userActivity =
withObjCPtr userActivity $ \raw_userActivity ->
    sendMsg inAddTasksIntentResponse (mkSelector "initWithCode:userActivity:") (retPtr retVoid) [argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- code@
code :: IsINAddTasksIntentResponse inAddTasksIntentResponse => inAddTasksIntentResponse -> IO INAddTasksIntentResponseCode
code inAddTasksIntentResponse  =
  fmap (coerce :: CLong -> INAddTasksIntentResponseCode) $ sendMsg inAddTasksIntentResponse (mkSelector "code") retCLong []

-- | @- modifiedTaskList@
modifiedTaskList :: IsINAddTasksIntentResponse inAddTasksIntentResponse => inAddTasksIntentResponse -> IO (Id INTaskList)
modifiedTaskList inAddTasksIntentResponse  =
  sendMsg inAddTasksIntentResponse (mkSelector "modifiedTaskList") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setModifiedTaskList:@
setModifiedTaskList :: (IsINAddTasksIntentResponse inAddTasksIntentResponse, IsINTaskList value) => inAddTasksIntentResponse -> value -> IO ()
setModifiedTaskList inAddTasksIntentResponse  value =
withObjCPtr value $ \raw_value ->
    sendMsg inAddTasksIntentResponse (mkSelector "setModifiedTaskList:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- addedTasks@
addedTasks :: IsINAddTasksIntentResponse inAddTasksIntentResponse => inAddTasksIntentResponse -> IO (Id NSArray)
addedTasks inAddTasksIntentResponse  =
  sendMsg inAddTasksIntentResponse (mkSelector "addedTasks") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAddedTasks:@
setAddedTasks :: (IsINAddTasksIntentResponse inAddTasksIntentResponse, IsNSArray value) => inAddTasksIntentResponse -> value -> IO ()
setAddedTasks inAddTasksIntentResponse  value =
withObjCPtr value $ \raw_value ->
    sendMsg inAddTasksIntentResponse (mkSelector "setAddedTasks:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @Selector@ for @modifiedTaskList@
modifiedTaskListSelector :: Selector
modifiedTaskListSelector = mkSelector "modifiedTaskList"

-- | @Selector@ for @setModifiedTaskList:@
setModifiedTaskListSelector :: Selector
setModifiedTaskListSelector = mkSelector "setModifiedTaskList:"

-- | @Selector@ for @addedTasks@
addedTasksSelector :: Selector
addedTasksSelector = mkSelector "addedTasks"

-- | @Selector@ for @setAddedTasks:@
setAddedTasksSelector :: Selector
setAddedTasksSelector = mkSelector "setAddedTasks:"

