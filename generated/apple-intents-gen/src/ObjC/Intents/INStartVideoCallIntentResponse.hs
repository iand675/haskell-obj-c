{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INStartVideoCallIntentResponse@.
module ObjC.Intents.INStartVideoCallIntentResponse
  ( INStartVideoCallIntentResponse
  , IsINStartVideoCallIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , codeSelector
  , initSelector
  , initWithCode_userActivitySelector

  -- * Enum types
  , INStartVideoCallIntentResponseCode(INStartVideoCallIntentResponseCode)
  , pattern INStartVideoCallIntentResponseCodeUnspecified
  , pattern INStartVideoCallIntentResponseCodeReady
  , pattern INStartVideoCallIntentResponseCodeContinueInApp
  , pattern INStartVideoCallIntentResponseCodeFailure
  , pattern INStartVideoCallIntentResponseCodeFailureRequiringAppLaunch
  , pattern INStartVideoCallIntentResponseCodeFailureAppConfigurationRequired
  , pattern INStartVideoCallIntentResponseCodeFailureCallingServiceNotAvailable
  , pattern INStartVideoCallIntentResponseCodeFailureContactNotSupportedByApp
  , pattern INStartVideoCallIntentResponseCodeFailureInvalidNumber

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
init_ :: IsINStartVideoCallIntentResponse inStartVideoCallIntentResponse => inStartVideoCallIntentResponse -> IO RawId
init_ inStartVideoCallIntentResponse =
  sendOwnedMessage inStartVideoCallIntentResponse initSelector

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINStartVideoCallIntentResponse inStartVideoCallIntentResponse, IsNSUserActivity userActivity) => inStartVideoCallIntentResponse -> INStartVideoCallIntentResponseCode -> userActivity -> IO (Id INStartVideoCallIntentResponse)
initWithCode_userActivity inStartVideoCallIntentResponse code userActivity =
  sendOwnedMessage inStartVideoCallIntentResponse initWithCode_userActivitySelector code (toNSUserActivity userActivity)

-- | @- code@
code :: IsINStartVideoCallIntentResponse inStartVideoCallIntentResponse => inStartVideoCallIntentResponse -> IO INStartVideoCallIntentResponseCode
code inStartVideoCallIntentResponse =
  sendMessage inStartVideoCallIntentResponse codeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector '[INStartVideoCallIntentResponseCode, Id NSUserActivity] (Id INStartVideoCallIntentResponse)
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INStartVideoCallIntentResponseCode
codeSelector = mkSelector "code"

