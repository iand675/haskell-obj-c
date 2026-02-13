{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSnoozeTasksIntentResponse@.
module ObjC.Intents.INSnoozeTasksIntentResponse
  ( INSnoozeTasksIntentResponse
  , IsINSnoozeTasksIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , snoozedTasks
  , setSnoozedTasks
  , codeSelector
  , initSelector
  , initWithCode_userActivitySelector
  , setSnoozedTasksSelector
  , snoozedTasksSelector

  -- * Enum types
  , INSnoozeTasksIntentResponseCode(INSnoozeTasksIntentResponseCode)
  , pattern INSnoozeTasksIntentResponseCodeUnspecified
  , pattern INSnoozeTasksIntentResponseCodeReady
  , pattern INSnoozeTasksIntentResponseCodeInProgress
  , pattern INSnoozeTasksIntentResponseCodeSuccess
  , pattern INSnoozeTasksIntentResponseCodeFailure
  , pattern INSnoozeTasksIntentResponseCodeFailureRequiringAppLaunch

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
init_ :: IsINSnoozeTasksIntentResponse inSnoozeTasksIntentResponse => inSnoozeTasksIntentResponse -> IO RawId
init_ inSnoozeTasksIntentResponse =
  sendOwnedMessage inSnoozeTasksIntentResponse initSelector

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINSnoozeTasksIntentResponse inSnoozeTasksIntentResponse, IsNSUserActivity userActivity) => inSnoozeTasksIntentResponse -> INSnoozeTasksIntentResponseCode -> userActivity -> IO (Id INSnoozeTasksIntentResponse)
initWithCode_userActivity inSnoozeTasksIntentResponse code userActivity =
  sendOwnedMessage inSnoozeTasksIntentResponse initWithCode_userActivitySelector code (toNSUserActivity userActivity)

-- | @- code@
code :: IsINSnoozeTasksIntentResponse inSnoozeTasksIntentResponse => inSnoozeTasksIntentResponse -> IO INSnoozeTasksIntentResponseCode
code inSnoozeTasksIntentResponse =
  sendMessage inSnoozeTasksIntentResponse codeSelector

-- | @- snoozedTasks@
snoozedTasks :: IsINSnoozeTasksIntentResponse inSnoozeTasksIntentResponse => inSnoozeTasksIntentResponse -> IO (Id NSArray)
snoozedTasks inSnoozeTasksIntentResponse =
  sendMessage inSnoozeTasksIntentResponse snoozedTasksSelector

-- | @- setSnoozedTasks:@
setSnoozedTasks :: (IsINSnoozeTasksIntentResponse inSnoozeTasksIntentResponse, IsNSArray value) => inSnoozeTasksIntentResponse -> value -> IO ()
setSnoozedTasks inSnoozeTasksIntentResponse value =
  sendMessage inSnoozeTasksIntentResponse setSnoozedTasksSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector '[INSnoozeTasksIntentResponseCode, Id NSUserActivity] (Id INSnoozeTasksIntentResponse)
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INSnoozeTasksIntentResponseCode
codeSelector = mkSelector "code"

-- | @Selector@ for @snoozedTasks@
snoozedTasksSelector :: Selector '[] (Id NSArray)
snoozedTasksSelector = mkSelector "snoozedTasks"

-- | @Selector@ for @setSnoozedTasks:@
setSnoozedTasksSelector :: Selector '[Id NSArray] ()
setSnoozedTasksSelector = mkSelector "setSnoozedTasks:"

