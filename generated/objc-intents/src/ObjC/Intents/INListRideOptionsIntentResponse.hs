{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , initWithCode_userActivitySelector
  , codeSelector
  , rideOptionsSelector
  , setRideOptionsSelector
  , paymentMethodsSelector
  , setPaymentMethodsSelector
  , expirationDateSelector
  , setExpirationDateSelector

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
init_ :: IsINListRideOptionsIntentResponse inListRideOptionsIntentResponse => inListRideOptionsIntentResponse -> IO RawId
init_ inListRideOptionsIntentResponse  =
  fmap (RawId . castPtr) $ sendMsg inListRideOptionsIntentResponse (mkSelector "init") (retPtr retVoid) []

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINListRideOptionsIntentResponse inListRideOptionsIntentResponse, IsNSUserActivity userActivity) => inListRideOptionsIntentResponse -> INListRideOptionsIntentResponseCode -> userActivity -> IO (Id INListRideOptionsIntentResponse)
initWithCode_userActivity inListRideOptionsIntentResponse  code userActivity =
withObjCPtr userActivity $ \raw_userActivity ->
    sendMsg inListRideOptionsIntentResponse (mkSelector "initWithCode:userActivity:") (retPtr retVoid) [argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- code@
code :: IsINListRideOptionsIntentResponse inListRideOptionsIntentResponse => inListRideOptionsIntentResponse -> IO INListRideOptionsIntentResponseCode
code inListRideOptionsIntentResponse  =
  fmap (coerce :: CLong -> INListRideOptionsIntentResponseCode) $ sendMsg inListRideOptionsIntentResponse (mkSelector "code") retCLong []

-- | @- rideOptions@
rideOptions :: IsINListRideOptionsIntentResponse inListRideOptionsIntentResponse => inListRideOptionsIntentResponse -> IO (Id NSArray)
rideOptions inListRideOptionsIntentResponse  =
  sendMsg inListRideOptionsIntentResponse (mkSelector "rideOptions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRideOptions:@
setRideOptions :: (IsINListRideOptionsIntentResponse inListRideOptionsIntentResponse, IsNSArray value) => inListRideOptionsIntentResponse -> value -> IO ()
setRideOptions inListRideOptionsIntentResponse  value =
withObjCPtr value $ \raw_value ->
    sendMsg inListRideOptionsIntentResponse (mkSelector "setRideOptions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- paymentMethods@
paymentMethods :: IsINListRideOptionsIntentResponse inListRideOptionsIntentResponse => inListRideOptionsIntentResponse -> IO (Id NSArray)
paymentMethods inListRideOptionsIntentResponse  =
  sendMsg inListRideOptionsIntentResponse (mkSelector "paymentMethods") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPaymentMethods:@
setPaymentMethods :: (IsINListRideOptionsIntentResponse inListRideOptionsIntentResponse, IsNSArray value) => inListRideOptionsIntentResponse -> value -> IO ()
setPaymentMethods inListRideOptionsIntentResponse  value =
withObjCPtr value $ \raw_value ->
    sendMsg inListRideOptionsIntentResponse (mkSelector "setPaymentMethods:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- expirationDate@
expirationDate :: IsINListRideOptionsIntentResponse inListRideOptionsIntentResponse => inListRideOptionsIntentResponse -> IO (Id NSDate)
expirationDate inListRideOptionsIntentResponse  =
  sendMsg inListRideOptionsIntentResponse (mkSelector "expirationDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setExpirationDate:@
setExpirationDate :: (IsINListRideOptionsIntentResponse inListRideOptionsIntentResponse, IsNSDate value) => inListRideOptionsIntentResponse -> value -> IO ()
setExpirationDate inListRideOptionsIntentResponse  value =
withObjCPtr value $ \raw_value ->
    sendMsg inListRideOptionsIntentResponse (mkSelector "setExpirationDate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @Selector@ for @rideOptions@
rideOptionsSelector :: Selector
rideOptionsSelector = mkSelector "rideOptions"

-- | @Selector@ for @setRideOptions:@
setRideOptionsSelector :: Selector
setRideOptionsSelector = mkSelector "setRideOptions:"

-- | @Selector@ for @paymentMethods@
paymentMethodsSelector :: Selector
paymentMethodsSelector = mkSelector "paymentMethods"

-- | @Selector@ for @setPaymentMethods:@
setPaymentMethodsSelector :: Selector
setPaymentMethodsSelector = mkSelector "setPaymentMethods:"

-- | @Selector@ for @expirationDate@
expirationDateSelector :: Selector
expirationDateSelector = mkSelector "expirationDate"

-- | @Selector@ for @setExpirationDate:@
setExpirationDateSelector :: Selector
setExpirationDateSelector = mkSelector "setExpirationDate:"

