{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INEndWorkoutIntentResponse@.
module ObjC.Intents.INEndWorkoutIntentResponse
  ( INEndWorkoutIntentResponse
  , IsINEndWorkoutIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , codeSelector
  , initSelector
  , initWithCode_userActivitySelector

  -- * Enum types
  , INEndWorkoutIntentResponseCode(INEndWorkoutIntentResponseCode)
  , pattern INEndWorkoutIntentResponseCodeUnspecified
  , pattern INEndWorkoutIntentResponseCodeReady
  , pattern INEndWorkoutIntentResponseCodeContinueInApp
  , pattern INEndWorkoutIntentResponseCodeFailure
  , pattern INEndWorkoutIntentResponseCodeFailureRequiringAppLaunch
  , pattern INEndWorkoutIntentResponseCodeFailureNoMatchingWorkout
  , pattern INEndWorkoutIntentResponseCodeHandleInApp
  , pattern INEndWorkoutIntentResponseCodeSuccess

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
init_ :: IsINEndWorkoutIntentResponse inEndWorkoutIntentResponse => inEndWorkoutIntentResponse -> IO RawId
init_ inEndWorkoutIntentResponse =
  sendOwnedMessage inEndWorkoutIntentResponse initSelector

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINEndWorkoutIntentResponse inEndWorkoutIntentResponse, IsNSUserActivity userActivity) => inEndWorkoutIntentResponse -> INEndWorkoutIntentResponseCode -> userActivity -> IO (Id INEndWorkoutIntentResponse)
initWithCode_userActivity inEndWorkoutIntentResponse code userActivity =
  sendOwnedMessage inEndWorkoutIntentResponse initWithCode_userActivitySelector code (toNSUserActivity userActivity)

-- | @- code@
code :: IsINEndWorkoutIntentResponse inEndWorkoutIntentResponse => inEndWorkoutIntentResponse -> IO INEndWorkoutIntentResponseCode
code inEndWorkoutIntentResponse =
  sendMessage inEndWorkoutIntentResponse codeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector '[INEndWorkoutIntentResponseCode, Id NSUserActivity] (Id INEndWorkoutIntentResponse)
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INEndWorkoutIntentResponseCode
codeSelector = mkSelector "code"

