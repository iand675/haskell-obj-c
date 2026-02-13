{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSetDefrosterSettingsInCarIntentResponse@.
module ObjC.Intents.INSetDefrosterSettingsInCarIntentResponse
  ( INSetDefrosterSettingsInCarIntentResponse
  , IsINSetDefrosterSettingsInCarIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , codeSelector
  , initSelector
  , initWithCode_userActivitySelector

  -- * Enum types
  , INSetDefrosterSettingsInCarIntentResponseCode(INSetDefrosterSettingsInCarIntentResponseCode)
  , pattern INSetDefrosterSettingsInCarIntentResponseCodeUnspecified
  , pattern INSetDefrosterSettingsInCarIntentResponseCodeReady
  , pattern INSetDefrosterSettingsInCarIntentResponseCodeInProgress
  , pattern INSetDefrosterSettingsInCarIntentResponseCodeSuccess
  , pattern INSetDefrosterSettingsInCarIntentResponseCodeFailure
  , pattern INSetDefrosterSettingsInCarIntentResponseCodeFailureRequiringAppLaunch

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
init_ :: IsINSetDefrosterSettingsInCarIntentResponse inSetDefrosterSettingsInCarIntentResponse => inSetDefrosterSettingsInCarIntentResponse -> IO RawId
init_ inSetDefrosterSettingsInCarIntentResponse =
  sendOwnedMessage inSetDefrosterSettingsInCarIntentResponse initSelector

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINSetDefrosterSettingsInCarIntentResponse inSetDefrosterSettingsInCarIntentResponse, IsNSUserActivity userActivity) => inSetDefrosterSettingsInCarIntentResponse -> INSetDefrosterSettingsInCarIntentResponseCode -> userActivity -> IO (Id INSetDefrosterSettingsInCarIntentResponse)
initWithCode_userActivity inSetDefrosterSettingsInCarIntentResponse code userActivity =
  sendOwnedMessage inSetDefrosterSettingsInCarIntentResponse initWithCode_userActivitySelector code (toNSUserActivity userActivity)

-- | @- code@
code :: IsINSetDefrosterSettingsInCarIntentResponse inSetDefrosterSettingsInCarIntentResponse => inSetDefrosterSettingsInCarIntentResponse -> IO INSetDefrosterSettingsInCarIntentResponseCode
code inSetDefrosterSettingsInCarIntentResponse =
  sendMessage inSetDefrosterSettingsInCarIntentResponse codeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector '[INSetDefrosterSettingsInCarIntentResponseCode, Id NSUserActivity] (Id INSetDefrosterSettingsInCarIntentResponse)
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INSetDefrosterSettingsInCarIntentResponseCode
codeSelector = mkSelector "code"

