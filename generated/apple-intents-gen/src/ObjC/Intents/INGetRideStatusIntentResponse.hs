{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INGetRideStatusIntentResponse@.
module ObjC.Intents.INGetRideStatusIntentResponse
  ( INGetRideStatusIntentResponse
  , IsINGetRideStatusIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , rideStatus
  , setRideStatus
  , codeSelector
  , initSelector
  , initWithCode_userActivitySelector
  , rideStatusSelector
  , setRideStatusSelector

  -- * Enum types
  , INGetRideStatusIntentResponseCode(INGetRideStatusIntentResponseCode)
  , pattern INGetRideStatusIntentResponseCodeUnspecified
  , pattern INGetRideStatusIntentResponseCodeReady
  , pattern INGetRideStatusIntentResponseCodeInProgress
  , pattern INGetRideStatusIntentResponseCodeSuccess
  , pattern INGetRideStatusIntentResponseCodeFailure
  , pattern INGetRideStatusIntentResponseCodeFailureRequiringAppLaunch
  , pattern INGetRideStatusIntentResponseCodeFailureRequiringAppLaunchMustVerifyCredentials
  , pattern INGetRideStatusIntentResponseCodeFailureRequiringAppLaunchServiceTemporarilyUnavailable

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
init_ :: IsINGetRideStatusIntentResponse inGetRideStatusIntentResponse => inGetRideStatusIntentResponse -> IO RawId
init_ inGetRideStatusIntentResponse =
  sendOwnedMessage inGetRideStatusIntentResponse initSelector

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINGetRideStatusIntentResponse inGetRideStatusIntentResponse, IsNSUserActivity userActivity) => inGetRideStatusIntentResponse -> INGetRideStatusIntentResponseCode -> userActivity -> IO (Id INGetRideStatusIntentResponse)
initWithCode_userActivity inGetRideStatusIntentResponse code userActivity =
  sendOwnedMessage inGetRideStatusIntentResponse initWithCode_userActivitySelector code (toNSUserActivity userActivity)

-- | @- code@
code :: IsINGetRideStatusIntentResponse inGetRideStatusIntentResponse => inGetRideStatusIntentResponse -> IO INGetRideStatusIntentResponseCode
code inGetRideStatusIntentResponse =
  sendMessage inGetRideStatusIntentResponse codeSelector

-- | @- rideStatus@
rideStatus :: IsINGetRideStatusIntentResponse inGetRideStatusIntentResponse => inGetRideStatusIntentResponse -> IO (Id INRideStatus)
rideStatus inGetRideStatusIntentResponse =
  sendMessage inGetRideStatusIntentResponse rideStatusSelector

-- | @- setRideStatus:@
setRideStatus :: (IsINGetRideStatusIntentResponse inGetRideStatusIntentResponse, IsINRideStatus value) => inGetRideStatusIntentResponse -> value -> IO ()
setRideStatus inGetRideStatusIntentResponse value =
  sendMessage inGetRideStatusIntentResponse setRideStatusSelector (toINRideStatus value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector '[INGetRideStatusIntentResponseCode, Id NSUserActivity] (Id INGetRideStatusIntentResponse)
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INGetRideStatusIntentResponseCode
codeSelector = mkSelector "code"

-- | @Selector@ for @rideStatus@
rideStatusSelector :: Selector '[] (Id INRideStatus)
rideStatusSelector = mkSelector "rideStatus"

-- | @Selector@ for @setRideStatus:@
setRideStatusSelector :: Selector '[Id INRideStatus] ()
setRideStatusSelector = mkSelector "setRideStatus:"

