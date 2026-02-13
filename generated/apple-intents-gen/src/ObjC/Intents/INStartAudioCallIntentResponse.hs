{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INStartAudioCallIntentResponse@.
module ObjC.Intents.INStartAudioCallIntentResponse
  ( INStartAudioCallIntentResponse
  , IsINStartAudioCallIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , codeSelector
  , initSelector
  , initWithCode_userActivitySelector

  -- * Enum types
  , INStartAudioCallIntentResponseCode(INStartAudioCallIntentResponseCode)
  , pattern INStartAudioCallIntentResponseCodeUnspecified
  , pattern INStartAudioCallIntentResponseCodeReady
  , pattern INStartAudioCallIntentResponseCodeContinueInApp
  , pattern INStartAudioCallIntentResponseCodeFailure
  , pattern INStartAudioCallIntentResponseCodeFailureRequiringAppLaunch
  , pattern INStartAudioCallIntentResponseCodeFailureAppConfigurationRequired
  , pattern INStartAudioCallIntentResponseCodeFailureCallingServiceNotAvailable
  , pattern INStartAudioCallIntentResponseCodeFailureContactNotSupportedByApp
  , pattern INStartAudioCallIntentResponseCodeFailureNoValidNumber

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
init_ :: IsINStartAudioCallIntentResponse inStartAudioCallIntentResponse => inStartAudioCallIntentResponse -> IO RawId
init_ inStartAudioCallIntentResponse =
  sendOwnedMessage inStartAudioCallIntentResponse initSelector

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINStartAudioCallIntentResponse inStartAudioCallIntentResponse, IsNSUserActivity userActivity) => inStartAudioCallIntentResponse -> INStartAudioCallIntentResponseCode -> userActivity -> IO (Id INStartAudioCallIntentResponse)
initWithCode_userActivity inStartAudioCallIntentResponse code userActivity =
  sendOwnedMessage inStartAudioCallIntentResponse initWithCode_userActivitySelector code (toNSUserActivity userActivity)

-- | @- code@
code :: IsINStartAudioCallIntentResponse inStartAudioCallIntentResponse => inStartAudioCallIntentResponse -> IO INStartAudioCallIntentResponseCode
code inStartAudioCallIntentResponse =
  sendMessage inStartAudioCallIntentResponse codeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector '[INStartAudioCallIntentResponseCode, Id NSUserActivity] (Id INStartAudioCallIntentResponse)
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INStartAudioCallIntentResponseCode
codeSelector = mkSelector "code"

