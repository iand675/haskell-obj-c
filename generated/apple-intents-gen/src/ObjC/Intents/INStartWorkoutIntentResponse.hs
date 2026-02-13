{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INStartWorkoutIntentResponse@.
module ObjC.Intents.INStartWorkoutIntentResponse
  ( INStartWorkoutIntentResponse
  , IsINStartWorkoutIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , codeSelector
  , initSelector
  , initWithCode_userActivitySelector

  -- * Enum types
  , INStartWorkoutIntentResponseCode(INStartWorkoutIntentResponseCode)
  , pattern INStartWorkoutIntentResponseCodeUnspecified
  , pattern INStartWorkoutIntentResponseCodeReady
  , pattern INStartWorkoutIntentResponseCodeContinueInApp
  , pattern INStartWorkoutIntentResponseCodeFailure
  , pattern INStartWorkoutIntentResponseCodeFailureRequiringAppLaunch
  , pattern INStartWorkoutIntentResponseCodeFailureOngoingWorkout
  , pattern INStartWorkoutIntentResponseCodeFailureNoMatchingWorkout
  , pattern INStartWorkoutIntentResponseCodeHandleInApp
  , pattern INStartWorkoutIntentResponseCodeSuccess

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
init_ :: IsINStartWorkoutIntentResponse inStartWorkoutIntentResponse => inStartWorkoutIntentResponse -> IO RawId
init_ inStartWorkoutIntentResponse =
  sendOwnedMessage inStartWorkoutIntentResponse initSelector

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINStartWorkoutIntentResponse inStartWorkoutIntentResponse, IsNSUserActivity userActivity) => inStartWorkoutIntentResponse -> INStartWorkoutIntentResponseCode -> userActivity -> IO (Id INStartWorkoutIntentResponse)
initWithCode_userActivity inStartWorkoutIntentResponse code userActivity =
  sendOwnedMessage inStartWorkoutIntentResponse initWithCode_userActivitySelector code (toNSUserActivity userActivity)

-- | @- code@
code :: IsINStartWorkoutIntentResponse inStartWorkoutIntentResponse => inStartWorkoutIntentResponse -> IO INStartWorkoutIntentResponseCode
code inStartWorkoutIntentResponse =
  sendMessage inStartWorkoutIntentResponse codeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector '[INStartWorkoutIntentResponseCode, Id NSUserActivity] (Id INStartWorkoutIntentResponse)
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INStartWorkoutIntentResponseCode
codeSelector = mkSelector "code"

