{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSetMessageAttributeIntentResponse@.
module ObjC.Intents.INSetMessageAttributeIntentResponse
  ( INSetMessageAttributeIntentResponse
  , IsINSetMessageAttributeIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , codeSelector
  , initSelector
  , initWithCode_userActivitySelector

  -- * Enum types
  , INSetMessageAttributeIntentResponseCode(INSetMessageAttributeIntentResponseCode)
  , pattern INSetMessageAttributeIntentResponseCodeUnspecified
  , pattern INSetMessageAttributeIntentResponseCodeReady
  , pattern INSetMessageAttributeIntentResponseCodeInProgress
  , pattern INSetMessageAttributeIntentResponseCodeSuccess
  , pattern INSetMessageAttributeIntentResponseCodeFailure
  , pattern INSetMessageAttributeIntentResponseCodeFailureRequiringAppLaunch
  , pattern INSetMessageAttributeIntentResponseCodeFailureMessageNotFound
  , pattern INSetMessageAttributeIntentResponseCodeFailureMessageAttributeNotSet

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
init_ :: IsINSetMessageAttributeIntentResponse inSetMessageAttributeIntentResponse => inSetMessageAttributeIntentResponse -> IO RawId
init_ inSetMessageAttributeIntentResponse =
  sendOwnedMessage inSetMessageAttributeIntentResponse initSelector

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINSetMessageAttributeIntentResponse inSetMessageAttributeIntentResponse, IsNSUserActivity userActivity) => inSetMessageAttributeIntentResponse -> INSetMessageAttributeIntentResponseCode -> userActivity -> IO (Id INSetMessageAttributeIntentResponse)
initWithCode_userActivity inSetMessageAttributeIntentResponse code userActivity =
  sendOwnedMessage inSetMessageAttributeIntentResponse initWithCode_userActivitySelector code (toNSUserActivity userActivity)

-- | @- code@
code :: IsINSetMessageAttributeIntentResponse inSetMessageAttributeIntentResponse => inSetMessageAttributeIntentResponse -> IO INSetMessageAttributeIntentResponseCode
code inSetMessageAttributeIntentResponse =
  sendMessage inSetMessageAttributeIntentResponse codeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector '[INSetMessageAttributeIntentResponseCode, Id NSUserActivity] (Id INSetMessageAttributeIntentResponse)
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INSetMessageAttributeIntentResponseCode
codeSelector = mkSelector "code"

