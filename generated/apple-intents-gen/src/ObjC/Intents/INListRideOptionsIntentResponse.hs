{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INListRideOptionsIntentResponse@.
module ObjC.Intents.INListRideOptionsIntentResponse
  ( INListRideOptionsIntentResponse
  , IsINListRideOptionsIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , rideOptions
  , setRideOptions
  , paymentMethods
  , setPaymentMethods
  , expirationDate
  , setExpirationDate
  , codeSelector
  , expirationDateSelector
  , initSelector
  , initWithCode_userActivitySelector
  , paymentMethodsSelector
  , rideOptionsSelector
  , setExpirationDateSelector
  , setPaymentMethodsSelector
  , setRideOptionsSelector

  -- * Enum types
  , INListRideOptionsIntentResponseCode(INListRideOptionsIntentResponseCode)
  , pattern INListRideOptionsIntentResponseCodeUnspecified
  , pattern INListRideOptionsIntentResponseCodeReady
  , pattern INListRideOptionsIntentResponseCodeInProgress
  , pattern INListRideOptionsIntentResponseCodeSuccess
  , pattern INListRideOptionsIntentResponseCodeFailure
  , pattern INListRideOptionsIntentResponseCodeFailureRequiringAppLaunch
  , pattern INListRideOptionsIntentResponseCodeFailureRequiringAppLaunchMustVerifyCredentials
  , pattern INListRideOptionsIntentResponseCodeFailureRequiringAppLaunchNoServiceInArea
  , pattern INListRideOptionsIntentResponseCodeFailureRequiringAppLaunchServiceTemporarilyUnavailable
  , pattern INListRideOptionsIntentResponseCodeFailureRequiringAppLaunchPreviousRideNeedsCompletion
  , pattern INListRideOptionsIntentResponseCodeFailurePreviousRideNeedsFeedback

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
init_ :: IsINListRideOptionsIntentResponse inListRideOptionsIntentResponse => inListRideOptionsIntentResponse -> IO RawId
init_ inListRideOptionsIntentResponse =
  sendOwnedMessage inListRideOptionsIntentResponse initSelector

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINListRideOptionsIntentResponse inListRideOptionsIntentResponse, IsNSUserActivity userActivity) => inListRideOptionsIntentResponse -> INListRideOptionsIntentResponseCode -> userActivity -> IO (Id INListRideOptionsIntentResponse)
initWithCode_userActivity inListRideOptionsIntentResponse code userActivity =
  sendOwnedMessage inListRideOptionsIntentResponse initWithCode_userActivitySelector code (toNSUserActivity userActivity)

-- | @- code@
code :: IsINListRideOptionsIntentResponse inListRideOptionsIntentResponse => inListRideOptionsIntentResponse -> IO INListRideOptionsIntentResponseCode
code inListRideOptionsIntentResponse =
  sendMessage inListRideOptionsIntentResponse codeSelector

-- | @- rideOptions@
rideOptions :: IsINListRideOptionsIntentResponse inListRideOptionsIntentResponse => inListRideOptionsIntentResponse -> IO (Id NSArray)
rideOptions inListRideOptionsIntentResponse =
  sendMessage inListRideOptionsIntentResponse rideOptionsSelector

-- | @- setRideOptions:@
setRideOptions :: (IsINListRideOptionsIntentResponse inListRideOptionsIntentResponse, IsNSArray value) => inListRideOptionsIntentResponse -> value -> IO ()
setRideOptions inListRideOptionsIntentResponse value =
  sendMessage inListRideOptionsIntentResponse setRideOptionsSelector (toNSArray value)

-- | @- paymentMethods@
paymentMethods :: IsINListRideOptionsIntentResponse inListRideOptionsIntentResponse => inListRideOptionsIntentResponse -> IO (Id NSArray)
paymentMethods inListRideOptionsIntentResponse =
  sendMessage inListRideOptionsIntentResponse paymentMethodsSelector

-- | @- setPaymentMethods:@
setPaymentMethods :: (IsINListRideOptionsIntentResponse inListRideOptionsIntentResponse, IsNSArray value) => inListRideOptionsIntentResponse -> value -> IO ()
setPaymentMethods inListRideOptionsIntentResponse value =
  sendMessage inListRideOptionsIntentResponse setPaymentMethodsSelector (toNSArray value)

-- | @- expirationDate@
expirationDate :: IsINListRideOptionsIntentResponse inListRideOptionsIntentResponse => inListRideOptionsIntentResponse -> IO (Id NSDate)
expirationDate inListRideOptionsIntentResponse =
  sendMessage inListRideOptionsIntentResponse expirationDateSelector

-- | @- setExpirationDate:@
setExpirationDate :: (IsINListRideOptionsIntentResponse inListRideOptionsIntentResponse, IsNSDate value) => inListRideOptionsIntentResponse -> value -> IO ()
setExpirationDate inListRideOptionsIntentResponse value =
  sendMessage inListRideOptionsIntentResponse setExpirationDateSelector (toNSDate value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCode:userActivity:@
initWithCode_userActivitySelector :: Selector '[INListRideOptionsIntentResponseCode, Id NSUserActivity] (Id INListRideOptionsIntentResponse)
initWithCode_userActivitySelector = mkSelector "initWithCode:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INListRideOptionsIntentResponseCode
codeSelector = mkSelector "code"

-- | @Selector@ for @rideOptions@
rideOptionsSelector :: Selector '[] (Id NSArray)
rideOptionsSelector = mkSelector "rideOptions"

-- | @Selector@ for @setRideOptions:@
setRideOptionsSelector :: Selector '[Id NSArray] ()
setRideOptionsSelector = mkSelector "setRideOptions:"

-- | @Selector@ for @paymentMethods@
paymentMethodsSelector :: Selector '[] (Id NSArray)
paymentMethodsSelector = mkSelector "paymentMethods"

-- | @Selector@ for @setPaymentMethods:@
setPaymentMethodsSelector :: Selector '[Id NSArray] ()
setPaymentMethodsSelector = mkSelector "setPaymentMethods:"

-- | @Selector@ for @expirationDate@
expirationDateSelector :: Selector '[] (Id NSDate)
expirationDateSelector = mkSelector "expirationDate"

-- | @Selector@ for @setExpirationDate:@
setExpirationDateSelector :: Selector '[Id NSDate] ()
setExpirationDateSelector = mkSelector "setExpirationDate:"

