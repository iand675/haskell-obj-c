{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSaveProfileInCarIntentResponse@.
module ObjC.Intents.INSaveProfileInCarIntentResponse
  ( INSaveProfileInCarIntentResponse
  , IsINSaveProfileInCarIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , codeSelector
  , initSelector
  , initWithCode_userActivitySelector

  -- * Enum types
  , INSaveProfileInCarIntentResponseCode(INSaveProfileInCarIntentResponseCode)
  , pattern INSaveProfileInCarIntentResponseCodeUnspecified
  , pattern INSaveProfileInCarIntentResponseCodeReady
  , pattern INSaveProfileInCarIntentResponseCodeInProgress
  , pattern INSaveProfileInCarIntentResponseCodeSuccess
  , pattern INSaveProfileInCarIntentResponseCodeFailure
  , pattern INSaveProfileInCarIntentResponseCodeFailureRequiringAppLaunch

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
init_ :: IsINSaveProfileInCarIntentResponse inSaveProfileInCarIntentResponse => inSaveProfileInCarIntentResponse -> IO RawId
init_ inSaveProfileInCarIntentResponse =
  sendOwnedMessage inSaveProfileInCarIntentResponse initSelector

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINSaveProfileInCarIntentResponse inSaveProfileInCarIntentResponse, IsNSUserActivity userActivity) => inSaveProfileInCarIntentResponse -> INSaveProfileInCarIntentResponseCode -> userActivity -> IO (Id INSaveProfileInCarIntentResponse)
initWithCode_userActivity inSaveProfileInCarIntentResponse code userActivity =
  sendOwnedMessage inSaveProfileInCarIntentResponse initWithCode_userActivitySelector code (toNSUserActivity userActivity)

-- | @- code@
code :: IsINSaveProfileInCarIntentResponse inSaveProfileInCarIntentResponse => inSaveProfileInCarIntentResponse -> IO INSaveProfileInCarIntentResponseCode
code inSaveProfileInCarIntentResponse =
  sendMessage inSaveProfileInCarIntentResponse codeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector '[INSaveProfileInCarIntentResponseCode, Id NSUserActivity] (Id INSaveProfileInCarIntentResponse)
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INSaveProfileInCarIntentResponseCode
codeSelector = mkSelector "code"

