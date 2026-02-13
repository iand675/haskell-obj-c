{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INGetCarLockStatusIntentResponse@.
module ObjC.Intents.INGetCarLockStatusIntentResponse
  ( INGetCarLockStatusIntentResponse
  , IsINGetCarLockStatusIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , locked
  , setLocked
  , codeSelector
  , initSelector
  , initWithCode_userActivitySelector
  , lockedSelector
  , setLockedSelector

  -- * Enum types
  , INGetCarLockStatusIntentResponseCode(INGetCarLockStatusIntentResponseCode)
  , pattern INGetCarLockStatusIntentResponseCodeUnspecified
  , pattern INGetCarLockStatusIntentResponseCodeReady
  , pattern INGetCarLockStatusIntentResponseCodeInProgress
  , pattern INGetCarLockStatusIntentResponseCodeSuccess
  , pattern INGetCarLockStatusIntentResponseCodeFailure
  , pattern INGetCarLockStatusIntentResponseCodeFailureRequiringAppLaunch

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
init_ :: IsINGetCarLockStatusIntentResponse inGetCarLockStatusIntentResponse => inGetCarLockStatusIntentResponse -> IO RawId
init_ inGetCarLockStatusIntentResponse =
  sendOwnedMessage inGetCarLockStatusIntentResponse initSelector

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINGetCarLockStatusIntentResponse inGetCarLockStatusIntentResponse, IsNSUserActivity userActivity) => inGetCarLockStatusIntentResponse -> INGetCarLockStatusIntentResponseCode -> userActivity -> IO (Id INGetCarLockStatusIntentResponse)
initWithCode_userActivity inGetCarLockStatusIntentResponse code userActivity =
  sendOwnedMessage inGetCarLockStatusIntentResponse initWithCode_userActivitySelector code (toNSUserActivity userActivity)

-- | @- code@
code :: IsINGetCarLockStatusIntentResponse inGetCarLockStatusIntentResponse => inGetCarLockStatusIntentResponse -> IO INGetCarLockStatusIntentResponseCode
code inGetCarLockStatusIntentResponse =
  sendMessage inGetCarLockStatusIntentResponse codeSelector

-- | @- locked@
locked :: IsINGetCarLockStatusIntentResponse inGetCarLockStatusIntentResponse => inGetCarLockStatusIntentResponse -> IO (Id NSNumber)
locked inGetCarLockStatusIntentResponse =
  sendMessage inGetCarLockStatusIntentResponse lockedSelector

-- | @- setLocked:@
setLocked :: (IsINGetCarLockStatusIntentResponse inGetCarLockStatusIntentResponse, IsNSNumber value) => inGetCarLockStatusIntentResponse -> value -> IO ()
setLocked inGetCarLockStatusIntentResponse value =
  sendMessage inGetCarLockStatusIntentResponse setLockedSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector '[INGetCarLockStatusIntentResponseCode, Id NSUserActivity] (Id INGetCarLockStatusIntentResponse)
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INGetCarLockStatusIntentResponseCode
codeSelector = mkSelector "code"

-- | @Selector@ for @locked@
lockedSelector :: Selector '[] (Id NSNumber)
lockedSelector = mkSelector "locked"

-- | @Selector@ for @setLocked:@
setLockedSelector :: Selector '[Id NSNumber] ()
setLockedSelector = mkSelector "setLocked:"

