{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INPauseWorkoutIntentResponse@.
module ObjC.Intents.INPauseWorkoutIntentResponse
  ( INPauseWorkoutIntentResponse
  , IsINPauseWorkoutIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , codeSelector
  , initSelector
  , initWithCode_userActivitySelector

  -- * Enum types
  , INPauseWorkoutIntentResponseCode(INPauseWorkoutIntentResponseCode)
  , pattern INPauseWorkoutIntentResponseCodeUnspecified
  , pattern INPauseWorkoutIntentResponseCodeReady
  , pattern INPauseWorkoutIntentResponseCodeContinueInApp
  , pattern INPauseWorkoutIntentResponseCodeFailure
  , pattern INPauseWorkoutIntentResponseCodeFailureRequiringAppLaunch
  , pattern INPauseWorkoutIntentResponseCodeFailureNoMatchingWorkout
  , pattern INPauseWorkoutIntentResponseCodeHandleInApp
  , pattern INPauseWorkoutIntentResponseCodeSuccess

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
init_ :: IsINPauseWorkoutIntentResponse inPauseWorkoutIntentResponse => inPauseWorkoutIntentResponse -> IO RawId
init_ inPauseWorkoutIntentResponse =
  sendOwnedMessage inPauseWorkoutIntentResponse initSelector

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINPauseWorkoutIntentResponse inPauseWorkoutIntentResponse, IsNSUserActivity userActivity) => inPauseWorkoutIntentResponse -> INPauseWorkoutIntentResponseCode -> userActivity -> IO (Id INPauseWorkoutIntentResponse)
initWithCode_userActivity inPauseWorkoutIntentResponse code userActivity =
  sendOwnedMessage inPauseWorkoutIntentResponse initWithCode_userActivitySelector code (toNSUserActivity userActivity)

-- | @- code@
code :: IsINPauseWorkoutIntentResponse inPauseWorkoutIntentResponse => inPauseWorkoutIntentResponse -> IO INPauseWorkoutIntentResponseCode
code inPauseWorkoutIntentResponse =
  sendMessage inPauseWorkoutIntentResponse codeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector '[INPauseWorkoutIntentResponseCode, Id NSUserActivity] (Id INPauseWorkoutIntentResponse)
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INPauseWorkoutIntentResponseCode
codeSelector = mkSelector "code"

