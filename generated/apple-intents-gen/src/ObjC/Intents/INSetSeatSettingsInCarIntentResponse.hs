{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSetSeatSettingsInCarIntentResponse@.
module ObjC.Intents.INSetSeatSettingsInCarIntentResponse
  ( INSetSeatSettingsInCarIntentResponse
  , IsINSetSeatSettingsInCarIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , codeSelector
  , initSelector
  , initWithCode_userActivitySelector

  -- * Enum types
  , INSetSeatSettingsInCarIntentResponseCode(INSetSeatSettingsInCarIntentResponseCode)
  , pattern INSetSeatSettingsInCarIntentResponseCodeUnspecified
  , pattern INSetSeatSettingsInCarIntentResponseCodeReady
  , pattern INSetSeatSettingsInCarIntentResponseCodeInProgress
  , pattern INSetSeatSettingsInCarIntentResponseCodeSuccess
  , pattern INSetSeatSettingsInCarIntentResponseCodeFailure
  , pattern INSetSeatSettingsInCarIntentResponseCodeFailureRequiringAppLaunch

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
init_ :: IsINSetSeatSettingsInCarIntentResponse inSetSeatSettingsInCarIntentResponse => inSetSeatSettingsInCarIntentResponse -> IO RawId
init_ inSetSeatSettingsInCarIntentResponse =
  sendOwnedMessage inSetSeatSettingsInCarIntentResponse initSelector

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINSetSeatSettingsInCarIntentResponse inSetSeatSettingsInCarIntentResponse, IsNSUserActivity userActivity) => inSetSeatSettingsInCarIntentResponse -> INSetSeatSettingsInCarIntentResponseCode -> userActivity -> IO (Id INSetSeatSettingsInCarIntentResponse)
initWithCode_userActivity inSetSeatSettingsInCarIntentResponse code userActivity =
  sendOwnedMessage inSetSeatSettingsInCarIntentResponse initWithCode_userActivitySelector code (toNSUserActivity userActivity)

-- | @- code@
code :: IsINSetSeatSettingsInCarIntentResponse inSetSeatSettingsInCarIntentResponse => inSetSeatSettingsInCarIntentResponse -> IO INSetSeatSettingsInCarIntentResponseCode
code inSetSeatSettingsInCarIntentResponse =
  sendMessage inSetSeatSettingsInCarIntentResponse codeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector '[INSetSeatSettingsInCarIntentResponseCode, Id NSUserActivity] (Id INSetSeatSettingsInCarIntentResponse)
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INSetSeatSettingsInCarIntentResponseCode
codeSelector = mkSelector "code"

