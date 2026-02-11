{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , initWithCode_userActivitySelector
  , codeSelector
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

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINGetRideStatusIntentResponse inGetRideStatusIntentResponse => inGetRideStatusIntentResponse -> IO RawId
init_ inGetRideStatusIntentResponse  =
  fmap (RawId . castPtr) $ sendMsg inGetRideStatusIntentResponse (mkSelector "init") (retPtr retVoid) []

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINGetRideStatusIntentResponse inGetRideStatusIntentResponse, IsNSUserActivity userActivity) => inGetRideStatusIntentResponse -> INGetRideStatusIntentResponseCode -> userActivity -> IO (Id INGetRideStatusIntentResponse)
initWithCode_userActivity inGetRideStatusIntentResponse  code userActivity =
withObjCPtr userActivity $ \raw_userActivity ->
    sendMsg inGetRideStatusIntentResponse (mkSelector "initWithCode:userActivity:") (retPtr retVoid) [argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- code@
code :: IsINGetRideStatusIntentResponse inGetRideStatusIntentResponse => inGetRideStatusIntentResponse -> IO INGetRideStatusIntentResponseCode
code inGetRideStatusIntentResponse  =
  fmap (coerce :: CLong -> INGetRideStatusIntentResponseCode) $ sendMsg inGetRideStatusIntentResponse (mkSelector "code") retCLong []

-- | @- rideStatus@
rideStatus :: IsINGetRideStatusIntentResponse inGetRideStatusIntentResponse => inGetRideStatusIntentResponse -> IO (Id INRideStatus)
rideStatus inGetRideStatusIntentResponse  =
  sendMsg inGetRideStatusIntentResponse (mkSelector "rideStatus") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRideStatus:@
setRideStatus :: (IsINGetRideStatusIntentResponse inGetRideStatusIntentResponse, IsINRideStatus value) => inGetRideStatusIntentResponse -> value -> IO ()
setRideStatus inGetRideStatusIntentResponse  value =
withObjCPtr value $ \raw_value ->
    sendMsg inGetRideStatusIntentResponse (mkSelector "setRideStatus:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector
codeSelector = mkSelector "code"

-- | @Selector@ for @rideStatus@
rideStatusSelector :: Selector
rideStatusSelector = mkSelector "rideStatus"

-- | @Selector@ for @setRideStatus:@
setRideStatusSelector :: Selector
setRideStatusSelector = mkSelector "setRideStatus:"

