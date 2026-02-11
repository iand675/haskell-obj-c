{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , initWithCode_userActivitySelector
  , codeSelector
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
init_ :: IsINRequestRideIntentResponse inRequestRideIntentResponse => inRequestRideIntentResponse -> IO RawId
init_ inRequestRideIntentResponse  =
  fmap (RawId . castPtr) $ sendMsg inRequestRideIntentResponse (mkSelector "init") (retPtr retVoid) []

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINRequestRideIntentResponse inRequestRideIntentResponse, IsNSUserActivity userActivity) => inRequestRideIntentResponse -> INRequestRideIntentResponseCode -> userActivity -> IO (Id INRequestRideIntentResponse)
initWithCode_userActivity inRequestRideIntentResponse  code userActivity =
withObjCPtr userActivity $ \raw_userActivity ->
    sendMsg inRequestRideIntentResponse (mkSelector "initWithCode:userActivity:") (retPtr retVoid) [argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- code@
code :: IsINRequestRideIntentResponse inRequestRideIntentResponse => inRequestRideIntentResponse -> IO INRequestRideIntentResponseCode
code inRequestRideIntentResponse  =
  fmap (coerce :: CLong -> INRequestRideIntentResponseCode) $ sendMsg inRequestRideIntentResponse (mkSelector "code") retCLong []

-- | @- rideStatus@
rideStatus :: IsINRequestRideIntentResponse inRequestRideIntentResponse => inRequestRideIntentResponse -> IO (Id INRideStatus)
rideStatus inRequestRideIntentResponse  =
  sendMsg inRequestRideIntentResponse (mkSelector "rideStatus") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRideStatus:@
setRideStatus :: (IsINRequestRideIntentResponse inRequestRideIntentResponse, IsINRideStatus value) => inRequestRideIntentResponse -> value -> IO ()
setRideStatus inRequestRideIntentResponse  value =
withObjCPtr value $ \raw_value ->
    sendMsg inRequestRideIntentResponse (mkSelector "setRideStatus:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

