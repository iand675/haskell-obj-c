{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INRequestPaymentIntentResponse@.
module ObjC.Intents.INRequestPaymentIntentResponse
  ( INRequestPaymentIntentResponse
  , IsINRequestPaymentIntentResponse(..)
  , init_
  , initWithCode_userActivity
  , code
  , paymentRecord
  , setPaymentRecord
  , initSelector
  , initWithCode_userActivitySelector
  , codeSelector
  , paymentRecordSelector
  , setPaymentRecordSelector

  -- * Enum types
  , INRequestPaymentIntentResponseCode(INRequestPaymentIntentResponseCode)
  , pattern INRequestPaymentIntentResponseCodeUnspecified
  , pattern INRequestPaymentIntentResponseCodeReady
  , pattern INRequestPaymentIntentResponseCodeInProgress
  , pattern INRequestPaymentIntentResponseCodeSuccess
  , pattern INRequestPaymentIntentResponseCodeFailure
  , pattern INRequestPaymentIntentResponseCodeFailureRequiringAppLaunch
  , pattern INRequestPaymentIntentResponseCodeFailureCredentialsUnverified
  , pattern INRequestPaymentIntentResponseCodeFailurePaymentsAmountBelowMinimum
  , pattern INRequestPaymentIntentResponseCodeFailurePaymentsAmountAboveMaximum
  , pattern INRequestPaymentIntentResponseCodeFailurePaymentsCurrencyUnsupported
  , pattern INRequestPaymentIntentResponseCodeFailureNoBankAccount
  , pattern INRequestPaymentIntentResponseCodeFailureNotEligible
  , pattern INRequestPaymentIntentResponseCodeFailureTermsAndConditionsAcceptanceRequired

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
init_ :: IsINRequestPaymentIntentResponse inRequestPaymentIntentResponse => inRequestPaymentIntentResponse -> IO RawId
init_ inRequestPaymentIntentResponse  =
  fmap (RawId . castPtr) $ sendMsg inRequestPaymentIntentResponse (mkSelector "init") (retPtr retVoid) []

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINRequestPaymentIntentResponse inRequestPaymentIntentResponse, IsNSUserActivity userActivity) => inRequestPaymentIntentResponse -> INRequestPaymentIntentResponseCode -> userActivity -> IO (Id INRequestPaymentIntentResponse)
initWithCode_userActivity inRequestPaymentIntentResponse  code userActivity =
withObjCPtr userActivity $ \raw_userActivity ->
    sendMsg inRequestPaymentIntentResponse (mkSelector "initWithCode:userActivity:") (retPtr retVoid) [argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- code@
code :: IsINRequestPaymentIntentResponse inRequestPaymentIntentResponse => inRequestPaymentIntentResponse -> IO INRequestPaymentIntentResponseCode
code inRequestPaymentIntentResponse  =
  fmap (coerce :: CLong -> INRequestPaymentIntentResponseCode) $ sendMsg inRequestPaymentIntentResponse (mkSelector "code") retCLong []

-- | @- paymentRecord@
paymentRecord :: IsINRequestPaymentIntentResponse inRequestPaymentIntentResponse => inRequestPaymentIntentResponse -> IO (Id INPaymentRecord)
paymentRecord inRequestPaymentIntentResponse  =
  sendMsg inRequestPaymentIntentResponse (mkSelector "paymentRecord") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPaymentRecord:@
setPaymentRecord :: (IsINRequestPaymentIntentResponse inRequestPaymentIntentResponse, IsINPaymentRecord value) => inRequestPaymentIntentResponse -> value -> IO ()
setPaymentRecord inRequestPaymentIntentResponse  value =
withObjCPtr value $ \raw_value ->
    sendMsg inRequestPaymentIntentResponse (mkSelector "setPaymentRecord:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @Selector@ for @paymentRecord@
paymentRecordSelector :: Selector
paymentRecordSelector = mkSelector "paymentRecord"

-- | @Selector@ for @setPaymentRecord:@
setPaymentRecordSelector :: Selector
setPaymentRecordSelector = mkSelector "setPaymentRecord:"

