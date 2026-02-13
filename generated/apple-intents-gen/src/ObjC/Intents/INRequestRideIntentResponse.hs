{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INRequestRideIntentResponse@.
module ObjC.Intents.INRequestRideIntentResponse
  ( INRequestRideIntentResponse
  , IsINRequestRideIntentResponse(..)
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
  , INRequestRideIntentResponseCode(INRequestRideIntentResponseCode)
  , pattern INRequestRideIntentResponseCodeUnspecified
  , pattern INRequestRideIntentResponseCodeReady
  , pattern INRequestRideIntentResponseCodeInProgress
  , pattern INRequestRideIntentResponseCodeSuccess
  , pattern INRequestRideIntentResponseCodeFailure
  , pattern INRequestRideIntentResponseCodeFailureRequiringAppLaunch
  , pattern INRequestRideIntentResponseCodeFailureRequiringAppLaunchMustVerifyCredentials
  , pattern INRequestRideIntentResponseCodeFailureRequiringAppLaunchNoServiceInArea
  , pattern INRequestRideIntentResponseCodeFailureRequiringAppLaunchServiceTemporarilyUnavailable
  , pattern INRequestRideIntentResponseCodeFailureRequiringAppLaunchPreviousRideNeedsCompletion
  , pattern INRequestRideIntentResponseCodeFailureRequiringAppLaunchRideScheduledTooFar

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
init_ :: IsINRequestRideIntentResponse inRequestRideIntentResponse => inRequestRideIntentResponse -> IO RawId
init_ inRequestRideIntentResponse =
  sendOwnedMessage inRequestRideIntentResponse initSelector

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINRequestRideIntentResponse inRequestRideIntentResponse, IsNSUserActivity userActivity) => inRequestRideIntentResponse -> INRequestRideIntentResponseCode -> userActivity -> IO (Id INRequestRideIntentResponse)
initWithCode_userActivity inRequestRideIntentResponse code userActivity =
  sendOwnedMessage inRequestRideIntentResponse initWithCode_userActivitySelector code (toNSUserActivity userActivity)

-- | @- code@
code :: IsINRequestRideIntentResponse inRequestRideIntentResponse => inRequestRideIntentResponse -> IO INRequestRideIntentResponseCode
code inRequestRideIntentResponse =
  sendMessage inRequestRideIntentResponse codeSelector

-- | @- rideStatus@
rideStatus :: IsINRequestRideIntentResponse inRequestRideIntentResponse => inRequestRideIntentResponse -> IO (Id INRideStatus)
rideStatus inRequestRideIntentResponse =
  sendMessage inRequestRideIntentResponse rideStatusSelector

-- | @- setRideStatus:@
setRideStatus :: (IsINRequestRideIntentResponse inRequestRideIntentResponse, IsINRideStatus value) => inRequestRideIntentResponse -> value -> IO ()
setRideStatus inRequestRideIntentResponse value =
  sendMessage inRequestRideIntentResponse setRideStatusSelector (toINRideStatus value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector '[INRequestRideIntentResponseCode, Id NSUserActivity] (Id INRequestRideIntentResponse)
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INRequestRideIntentResponseCode
codeSelector = mkSelector "code"

-- | @Selector@ for @rideStatus@
rideStatusSelector :: Selector '[] (Id INRideStatus)
rideStatusSelector = mkSelector "rideStatus"

-- | @Selector@ for @setRideStatus:@
setRideStatusSelector :: Selector '[Id INRideStatus] ()
setRideStatusSelector = mkSelector "setRideStatus:"

