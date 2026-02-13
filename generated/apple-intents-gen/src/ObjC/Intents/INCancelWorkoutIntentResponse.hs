{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INCancelWorkoutIntentResponse@.
module ObjC.Intents.INCancelWorkoutIntentResponse
  ( INCancelWorkoutIntentResponse
  , IsINCancelWorkoutIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , codeSelector
  , initSelector
  , initWithCode_userActivitySelector

  -- * Enum types
  , INCancelWorkoutIntentResponseCode(INCancelWorkoutIntentResponseCode)
  , pattern INCancelWorkoutIntentResponseCodeUnspecified
  , pattern INCancelWorkoutIntentResponseCodeReady
  , pattern INCancelWorkoutIntentResponseCodeContinueInApp
  , pattern INCancelWorkoutIntentResponseCodeFailure
  , pattern INCancelWorkoutIntentResponseCodeFailureRequiringAppLaunch
  , pattern INCancelWorkoutIntentResponseCodeFailureNoMatchingWorkout
  , pattern INCancelWorkoutIntentResponseCodeHandleInApp
  , pattern INCancelWorkoutIntentResponseCodeSuccess

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
init_ :: IsINCancelWorkoutIntentResponse inCancelWorkoutIntentResponse => inCancelWorkoutIntentResponse -> IO RawId
init_ inCancelWorkoutIntentResponse =
  sendOwnedMessage inCancelWorkoutIntentResponse initSelector

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINCancelWorkoutIntentResponse inCancelWorkoutIntentResponse, IsNSUserActivity userActivity) => inCancelWorkoutIntentResponse -> INCancelWorkoutIntentResponseCode -> userActivity -> IO (Id INCancelWorkoutIntentResponse)
initWithCode_userActivity inCancelWorkoutIntentResponse code userActivity =
  sendOwnedMessage inCancelWorkoutIntentResponse initWithCode_userActivitySelector code (toNSUserActivity userActivity)

-- | @- code@
code :: IsINCancelWorkoutIntentResponse inCancelWorkoutIntentResponse => inCancelWorkoutIntentResponse -> IO INCancelWorkoutIntentResponseCode
code inCancelWorkoutIntentResponse =
  sendMessage inCancelWorkoutIntentResponse codeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector '[INCancelWorkoutIntentResponseCode, Id NSUserActivity] (Id INCancelWorkoutIntentResponse)
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INCancelWorkoutIntentResponseCode
codeSelector = mkSelector "code"

