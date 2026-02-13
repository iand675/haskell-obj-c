{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INResumeWorkoutIntentResponse@.
module ObjC.Intents.INResumeWorkoutIntentResponse
  ( INResumeWorkoutIntentResponse
  , IsINResumeWorkoutIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , codeSelector
  , initSelector
  , initWithCode_userActivitySelector

  -- * Enum types
  , INResumeWorkoutIntentResponseCode(INResumeWorkoutIntentResponseCode)
  , pattern INResumeWorkoutIntentResponseCodeUnspecified
  , pattern INResumeWorkoutIntentResponseCodeReady
  , pattern INResumeWorkoutIntentResponseCodeContinueInApp
  , pattern INResumeWorkoutIntentResponseCodeFailure
  , pattern INResumeWorkoutIntentResponseCodeFailureRequiringAppLaunch
  , pattern INResumeWorkoutIntentResponseCodeFailureNoMatchingWorkout
  , pattern INResumeWorkoutIntentResponseCodeHandleInApp
  , pattern INResumeWorkoutIntentResponseCodeSuccess

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
init_ :: IsINResumeWorkoutIntentResponse inResumeWorkoutIntentResponse => inResumeWorkoutIntentResponse -> IO RawId
init_ inResumeWorkoutIntentResponse =
  sendOwnedMessage inResumeWorkoutIntentResponse initSelector

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINResumeWorkoutIntentResponse inResumeWorkoutIntentResponse, IsNSUserActivity userActivity) => inResumeWorkoutIntentResponse -> INResumeWorkoutIntentResponseCode -> userActivity -> IO (Id INResumeWorkoutIntentResponse)
initWithCode_userActivity inResumeWorkoutIntentResponse code userActivity =
  sendOwnedMessage inResumeWorkoutIntentResponse initWithCode_userActivitySelector code (toNSUserActivity userActivity)

-- | @- code@
code :: IsINResumeWorkoutIntentResponse inResumeWorkoutIntentResponse => inResumeWorkoutIntentResponse -> IO INResumeWorkoutIntentResponseCode
code inResumeWorkoutIntentResponse =
  sendMessage inResumeWorkoutIntentResponse codeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector '[INResumeWorkoutIntentResponseCode, Id NSUserActivity] (Id INResumeWorkoutIntentResponse)
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INResumeWorkoutIntentResponseCode
codeSelector = mkSelector "code"

