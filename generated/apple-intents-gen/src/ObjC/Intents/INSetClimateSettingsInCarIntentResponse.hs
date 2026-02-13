{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSetClimateSettingsInCarIntentResponse@.
module ObjC.Intents.INSetClimateSettingsInCarIntentResponse
  ( INSetClimateSettingsInCarIntentResponse
  , IsINSetClimateSettingsInCarIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , codeSelector
  , initSelector
  , initWithCode_userActivitySelector

  -- * Enum types
  , INSetClimateSettingsInCarIntentResponseCode(INSetClimateSettingsInCarIntentResponseCode)
  , pattern INSetClimateSettingsInCarIntentResponseCodeUnspecified
  , pattern INSetClimateSettingsInCarIntentResponseCodeReady
  , pattern INSetClimateSettingsInCarIntentResponseCodeInProgress
  , pattern INSetClimateSettingsInCarIntentResponseCodeSuccess
  , pattern INSetClimateSettingsInCarIntentResponseCodeFailure
  , pattern INSetClimateSettingsInCarIntentResponseCodeFailureRequiringAppLaunch

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
init_ :: IsINSetClimateSettingsInCarIntentResponse inSetClimateSettingsInCarIntentResponse => inSetClimateSettingsInCarIntentResponse -> IO RawId
init_ inSetClimateSettingsInCarIntentResponse =
  sendOwnedMessage inSetClimateSettingsInCarIntentResponse initSelector

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINSetClimateSettingsInCarIntentResponse inSetClimateSettingsInCarIntentResponse, IsNSUserActivity userActivity) => inSetClimateSettingsInCarIntentResponse -> INSetClimateSettingsInCarIntentResponseCode -> userActivity -> IO (Id INSetClimateSettingsInCarIntentResponse)
initWithCode_userActivity inSetClimateSettingsInCarIntentResponse code userActivity =
  sendOwnedMessage inSetClimateSettingsInCarIntentResponse initWithCode_userActivitySelector code (toNSUserActivity userActivity)

-- | @- code@
code :: IsINSetClimateSettingsInCarIntentResponse inSetClimateSettingsInCarIntentResponse => inSetClimateSettingsInCarIntentResponse -> IO INSetClimateSettingsInCarIntentResponseCode
code inSetClimateSettingsInCarIntentResponse =
  sendMessage inSetClimateSettingsInCarIntentResponse codeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector '[INSetClimateSettingsInCarIntentResponseCode, Id NSUserActivity] (Id INSetClimateSettingsInCarIntentResponse)
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INSetClimateSettingsInCarIntentResponseCode
codeSelector = mkSelector "code"

