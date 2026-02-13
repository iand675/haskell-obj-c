{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSetProfileInCarIntentResponse@.
module ObjC.Intents.INSetProfileInCarIntentResponse
  ( INSetProfileInCarIntentResponse
  , IsINSetProfileInCarIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , codeSelector
  , initSelector
  , initWithCode_userActivitySelector

  -- * Enum types
  , INSetProfileInCarIntentResponseCode(INSetProfileInCarIntentResponseCode)
  , pattern INSetProfileInCarIntentResponseCodeUnspecified
  , pattern INSetProfileInCarIntentResponseCodeReady
  , pattern INSetProfileInCarIntentResponseCodeInProgress
  , pattern INSetProfileInCarIntentResponseCodeSuccess
  , pattern INSetProfileInCarIntentResponseCodeFailure
  , pattern INSetProfileInCarIntentResponseCodeFailureRequiringAppLaunch

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
init_ :: IsINSetProfileInCarIntentResponse inSetProfileInCarIntentResponse => inSetProfileInCarIntentResponse -> IO RawId
init_ inSetProfileInCarIntentResponse =
  sendOwnedMessage inSetProfileInCarIntentResponse initSelector

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINSetProfileInCarIntentResponse inSetProfileInCarIntentResponse, IsNSUserActivity userActivity) => inSetProfileInCarIntentResponse -> INSetProfileInCarIntentResponseCode -> userActivity -> IO (Id INSetProfileInCarIntentResponse)
initWithCode_userActivity inSetProfileInCarIntentResponse code userActivity =
  sendOwnedMessage inSetProfileInCarIntentResponse initWithCode_userActivitySelector code (toNSUserActivity userActivity)

-- | @- code@
code :: IsINSetProfileInCarIntentResponse inSetProfileInCarIntentResponse => inSetProfileInCarIntentResponse -> IO INSetProfileInCarIntentResponseCode
code inSetProfileInCarIntentResponse =
  sendMessage inSetProfileInCarIntentResponse codeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector '[INSetProfileInCarIntentResponseCode, Id NSUserActivity] (Id INSetProfileInCarIntentResponse)
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INSetProfileInCarIntentResponseCode
codeSelector = mkSelector "code"

