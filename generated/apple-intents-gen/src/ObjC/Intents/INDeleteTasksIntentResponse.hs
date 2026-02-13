{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INDeleteTasksIntentResponse@.
module ObjC.Intents.INDeleteTasksIntentResponse
  ( INDeleteTasksIntentResponse
  , IsINDeleteTasksIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , deletedTasks
  , setDeletedTasks
  , codeSelector
  , deletedTasksSelector
  , initSelector
  , initWithCode_userActivitySelector
  , setDeletedTasksSelector

  -- * Enum types
  , INDeleteTasksIntentResponseCode(INDeleteTasksIntentResponseCode)
  , pattern INDeleteTasksIntentResponseCodeUnspecified
  , pattern INDeleteTasksIntentResponseCodeReady
  , pattern INDeleteTasksIntentResponseCodeInProgress
  , pattern INDeleteTasksIntentResponseCodeSuccess
  , pattern INDeleteTasksIntentResponseCodeFailure
  , pattern INDeleteTasksIntentResponseCodeFailureRequiringAppLaunch

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
init_ :: IsINDeleteTasksIntentResponse inDeleteTasksIntentResponse => inDeleteTasksIntentResponse -> IO RawId
init_ inDeleteTasksIntentResponse =
  sendOwnedMessage inDeleteTasksIntentResponse initSelector

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINDeleteTasksIntentResponse inDeleteTasksIntentResponse, IsNSUserActivity userActivity) => inDeleteTasksIntentResponse -> INDeleteTasksIntentResponseCode -> userActivity -> IO (Id INDeleteTasksIntentResponse)
initWithCode_userActivity inDeleteTasksIntentResponse code userActivity =
  sendOwnedMessage inDeleteTasksIntentResponse initWithCode_userActivitySelector code (toNSUserActivity userActivity)

-- | @- code@
code :: IsINDeleteTasksIntentResponse inDeleteTasksIntentResponse => inDeleteTasksIntentResponse -> IO INDeleteTasksIntentResponseCode
code inDeleteTasksIntentResponse =
  sendMessage inDeleteTasksIntentResponse codeSelector

-- | @- deletedTasks@
deletedTasks :: IsINDeleteTasksIntentResponse inDeleteTasksIntentResponse => inDeleteTasksIntentResponse -> IO (Id NSArray)
deletedTasks inDeleteTasksIntentResponse =
  sendMessage inDeleteTasksIntentResponse deletedTasksSelector

-- | @- setDeletedTasks:@
setDeletedTasks :: (IsINDeleteTasksIntentResponse inDeleteTasksIntentResponse, IsNSArray value) => inDeleteTasksIntentResponse -> value -> IO ()
setDeletedTasks inDeleteTasksIntentResponse value =
  sendMessage inDeleteTasksIntentResponse setDeletedTasksSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector '[INDeleteTasksIntentResponseCode, Id NSUserActivity] (Id INDeleteTasksIntentResponse)
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INDeleteTasksIntentResponseCode
codeSelector = mkSelector "code"

-- | @Selector@ for @deletedTasks@
deletedTasksSelector :: Selector '[] (Id NSArray)
deletedTasksSelector = mkSelector "deletedTasks"

-- | @Selector@ for @setDeletedTasks:@
setDeletedTasksSelector :: Selector '[Id NSArray] ()
setDeletedTasksSelector = mkSelector "setDeletedTasks:"

