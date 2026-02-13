{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSetAudioSourceInCarIntentResponse@.
module ObjC.Intents.INSetAudioSourceInCarIntentResponse
  ( INSetAudioSourceInCarIntentResponse
  , IsINSetAudioSourceInCarIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , codeSelector
  , initSelector
  , initWithCode_userActivitySelector

  -- * Enum types
  , INSetAudioSourceInCarIntentResponseCode(INSetAudioSourceInCarIntentResponseCode)
  , pattern INSetAudioSourceInCarIntentResponseCodeUnspecified
  , pattern INSetAudioSourceInCarIntentResponseCodeReady
  , pattern INSetAudioSourceInCarIntentResponseCodeInProgress
  , pattern INSetAudioSourceInCarIntentResponseCodeSuccess
  , pattern INSetAudioSourceInCarIntentResponseCodeFailure
  , pattern INSetAudioSourceInCarIntentResponseCodeFailureRequiringAppLaunch

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
init_ :: IsINSetAudioSourceInCarIntentResponse inSetAudioSourceInCarIntentResponse => inSetAudioSourceInCarIntentResponse -> IO RawId
init_ inSetAudioSourceInCarIntentResponse =
  sendOwnedMessage inSetAudioSourceInCarIntentResponse initSelector

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINSetAudioSourceInCarIntentResponse inSetAudioSourceInCarIntentResponse, IsNSUserActivity userActivity) => inSetAudioSourceInCarIntentResponse -> INSetAudioSourceInCarIntentResponseCode -> userActivity -> IO (Id INSetAudioSourceInCarIntentResponse)
initWithCode_userActivity inSetAudioSourceInCarIntentResponse code userActivity =
  sendOwnedMessage inSetAudioSourceInCarIntentResponse initWithCode_userActivitySelector code (toNSUserActivity userActivity)

-- | @- code@
code :: IsINSetAudioSourceInCarIntentResponse inSetAudioSourceInCarIntentResponse => inSetAudioSourceInCarIntentResponse -> IO INSetAudioSourceInCarIntentResponseCode
code inSetAudioSourceInCarIntentResponse =
  sendMessage inSetAudioSourceInCarIntentResponse codeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector '[INSetAudioSourceInCarIntentResponseCode, Id NSUserActivity] (Id INSetAudioSourceInCarIntentResponse)
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INSetAudioSourceInCarIntentResponseCode
codeSelector = mkSelector "code"

