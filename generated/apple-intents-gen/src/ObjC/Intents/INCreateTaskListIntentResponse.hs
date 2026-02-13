{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INCreateTaskListIntentResponse@.
module ObjC.Intents.INCreateTaskListIntentResponse
  ( INCreateTaskListIntentResponse
  , IsINCreateTaskListIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , createdTaskList
  , setCreatedTaskList
  , codeSelector
  , createdTaskListSelector
  , initSelector
  , initWithCode_userActivitySelector
  , setCreatedTaskListSelector

  -- * Enum types
  , INCreateTaskListIntentResponseCode(INCreateTaskListIntentResponseCode)
  , pattern INCreateTaskListIntentResponseCodeUnspecified
  , pattern INCreateTaskListIntentResponseCodeReady
  , pattern INCreateTaskListIntentResponseCodeInProgress
  , pattern INCreateTaskListIntentResponseCodeSuccess
  , pattern INCreateTaskListIntentResponseCodeFailure
  , pattern INCreateTaskListIntentResponseCodeFailureRequiringAppLaunch

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
init_ :: IsINCreateTaskListIntentResponse inCreateTaskListIntentResponse => inCreateTaskListIntentResponse -> IO RawId
init_ inCreateTaskListIntentResponse =
  sendOwnedMessage inCreateTaskListIntentResponse initSelector

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINCreateTaskListIntentResponse inCreateTaskListIntentResponse, IsNSUserActivity userActivity) => inCreateTaskListIntentResponse -> INCreateTaskListIntentResponseCode -> userActivity -> IO (Id INCreateTaskListIntentResponse)
initWithCode_userActivity inCreateTaskListIntentResponse code userActivity =
  sendOwnedMessage inCreateTaskListIntentResponse initWithCode_userActivitySelector code (toNSUserActivity userActivity)

-- | @- code@
code :: IsINCreateTaskListIntentResponse inCreateTaskListIntentResponse => inCreateTaskListIntentResponse -> IO INCreateTaskListIntentResponseCode
code inCreateTaskListIntentResponse =
  sendMessage inCreateTaskListIntentResponse codeSelector

-- | @- createdTaskList@
createdTaskList :: IsINCreateTaskListIntentResponse inCreateTaskListIntentResponse => inCreateTaskListIntentResponse -> IO (Id INTaskList)
createdTaskList inCreateTaskListIntentResponse =
  sendMessage inCreateTaskListIntentResponse createdTaskListSelector

-- | @- setCreatedTaskList:@
setCreatedTaskList :: (IsINCreateTaskListIntentResponse inCreateTaskListIntentResponse, IsINTaskList value) => inCreateTaskListIntentResponse -> value -> IO ()
setCreatedTaskList inCreateTaskListIntentResponse value =
  sendMessage inCreateTaskListIntentResponse setCreatedTaskListSelector (toINTaskList value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector '[INCreateTaskListIntentResponseCode, Id NSUserActivity] (Id INCreateTaskListIntentResponse)
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INCreateTaskListIntentResponseCode
codeSelector = mkSelector "code"

-- | @Selector@ for @createdTaskList@
createdTaskListSelector :: Selector '[] (Id INTaskList)
createdTaskListSelector = mkSelector "createdTaskList"

-- | @Selector@ for @setCreatedTaskList:@
setCreatedTaskListSelector :: Selector '[Id INTaskList] ()
setCreatedTaskListSelector = mkSelector "setCreatedTaskList:"

