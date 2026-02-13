{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INStartCallIntentResponse@.
module ObjC.Intents.INStartCallIntentResponse
  ( INStartCallIntentResponse
  , IsINStartCallIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , codeSelector
  , initSelector
  , initWithCode_userActivitySelector

  -- * Enum types
  , INStartCallIntentResponseCode(INStartCallIntentResponseCode)
  , pattern INStartCallIntentResponseCodeUnspecified
  , pattern INStartCallIntentResponseCodeReady
  , pattern INStartCallIntentResponseCodeContinueInApp
  , pattern INStartCallIntentResponseCodeUserConfirmationRequired
  , pattern INStartCallIntentResponseCodeFailure
  , pattern INStartCallIntentResponseCodeFailureRequiringAppLaunch
  , pattern INStartCallIntentResponseCodeFailureCallingServiceNotAvailable
  , pattern INStartCallIntentResponseCodeFailureContactNotSupportedByApp
  , pattern INStartCallIntentResponseCodeFailureAirplaneModeEnabled
  , pattern INStartCallIntentResponseCodeFailureUnableToHandOff
  , pattern INStartCallIntentResponseCodeFailureAppConfigurationRequired
  , pattern INStartCallIntentResponseCodeFailureCallInProgress
  , pattern INStartCallIntentResponseCodeFailureCallRinging
  , pattern INStartCallIntentResponseCodeFailureRequiringInAppAuthentication

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
init_ :: IsINStartCallIntentResponse inStartCallIntentResponse => inStartCallIntentResponse -> IO RawId
init_ inStartCallIntentResponse =
  sendOwnedMessage inStartCallIntentResponse initSelector

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINStartCallIntentResponse inStartCallIntentResponse, IsNSUserActivity userActivity) => inStartCallIntentResponse -> INStartCallIntentResponseCode -> userActivity -> IO (Id INStartCallIntentResponse)
initWithCode_userActivity inStartCallIntentResponse code userActivity =
  sendOwnedMessage inStartCallIntentResponse initWithCode_userActivitySelector code (toNSUserActivity userActivity)

-- | @- code@
code :: IsINStartCallIntentResponse inStartCallIntentResponse => inStartCallIntentResponse -> IO INStartCallIntentResponseCode
code inStartCallIntentResponse =
  sendMessage inStartCallIntentResponse codeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector '[INStartCallIntentResponseCode, Id NSUserActivity] (Id INStartCallIntentResponse)
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INStartCallIntentResponseCode
codeSelector = mkSelector "code"

