{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSendPaymentIntentResponse@.
module ObjC.Intents.INSendPaymentIntentResponse
  ( INSendPaymentIntentResponse
  , IsINSendPaymentIntentResponse(..)
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
  , INSendPaymentIntentResponseCode(INSendPaymentIntentResponseCode)
  , pattern INSendPaymentIntentResponseCodeUnspecified
  , pattern INSendPaymentIntentResponseCodeReady
  , pattern INSendPaymentIntentResponseCodeInProgress
  , pattern INSendPaymentIntentResponseCodeSuccess
  , pattern INSendPaymentIntentResponseCodeFailure
  , pattern INSendPaymentIntentResponseCodeFailureRequiringAppLaunch
  , pattern INSendPaymentIntentResponseCodeFailureCredentialsUnverified
  , pattern INSendPaymentIntentResponseCodeFailurePaymentsAmountBelowMinimum
  , pattern INSendPaymentIntentResponseCodeFailurePaymentsAmountAboveMaximum
  , pattern INSendPaymentIntentResponseCodeFailurePaymentsCurrencyUnsupported
  , pattern INSendPaymentIntentResponseCodeFailureInsufficientFunds
  , pattern INSendPaymentIntentResponseCodeFailureNoBankAccount
  , pattern INSendPaymentIntentResponseCodeFailureNotEligible
  , pattern INSendPaymentIntentResponseCodeFailureTermsAndConditionsAcceptanceRequired

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
init_ :: IsINSendPaymentIntentResponse inSendPaymentIntentResponse => inSendPaymentIntentResponse -> IO RawId
init_ inSendPaymentIntentResponse  =
  fmap (RawId . castPtr) $ sendMsg inSendPaymentIntentResponse (mkSelector "init") (retPtr retVoid) []

-- | @- initWithCode:userActivity:@
initWithCode_userActivity :: (IsINSendPaymentIntentResponse inSendPaymentIntentResponse, IsNSUserActivity userActivity) => inSendPaymentIntentResponse -> INSendPaymentIntentResponseCode -> userActivity -> IO (Id INSendPaymentIntentResponse)
initWithCode_userActivity inSendPaymentIntentResponse  code userActivity =
withObjCPtr userActivity $ \raw_userActivity ->
    sendMsg inSendPaymentIntentResponse (mkSelector "initWithCode:userActivity:") (retPtr retVoid) [argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- code@
code :: IsINSendPaymentIntentResponse inSendPaymentIntentResponse => inSendPaymentIntentResponse -> IO INSendPaymentIntentResponseCode
code inSendPaymentIntentResponse  =
  fmap (coerce :: CLong -> INSendPaymentIntentResponseCode) $ sendMsg inSendPaymentIntentResponse (mkSelector "code") retCLong []

-- | @- paymentRecord@
paymentRecord :: IsINSendPaymentIntentResponse inSendPaymentIntentResponse => inSendPaymentIntentResponse -> IO (Id INPaymentRecord)
paymentRecord inSendPaymentIntentResponse  =
  sendMsg inSendPaymentIntentResponse (mkSelector "paymentRecord") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPaymentRecord:@
setPaymentRecord :: (IsINSendPaymentIntentResponse inSendPaymentIntentResponse, IsINPaymentRecord value) => inSendPaymentIntentResponse -> value -> IO ()
setPaymentRecord inSendPaymentIntentResponse  value =
withObjCPtr value $ \raw_value ->
    sendMsg inSendPaymentIntentResponse (mkSelector "setPaymentRecord:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

