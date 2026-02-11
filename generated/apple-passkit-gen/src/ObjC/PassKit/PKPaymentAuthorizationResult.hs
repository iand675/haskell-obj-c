{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKPaymentAuthorizationResult@.
module ObjC.PassKit.PKPaymentAuthorizationResult
  ( PKPaymentAuthorizationResult
  , IsPKPaymentAuthorizationResult(..)
  , initWithStatus_errors
  , status
  , setStatus
  , errors
  , setErrors
  , orderDetails
  , setOrderDetails
  , initWithStatus_errorsSelector
  , statusSelector
  , setStatusSelector
  , errorsSelector
  , setErrorsSelector
  , orderDetailsSelector
  , setOrderDetailsSelector

  -- * Enum types
  , PKPaymentAuthorizationStatus(PKPaymentAuthorizationStatus)
  , pattern PKPaymentAuthorizationStatusSuccess
  , pattern PKPaymentAuthorizationStatusFailure
  , pattern PKPaymentAuthorizationStatusInvalidBillingPostalAddress
  , pattern PKPaymentAuthorizationStatusInvalidShippingPostalAddress
  , pattern PKPaymentAuthorizationStatusInvalidShippingContact
  , pattern PKPaymentAuthorizationStatusPINRequired
  , pattern PKPaymentAuthorizationStatusPINIncorrect
  , pattern PKPaymentAuthorizationStatusPINLockout

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

import ObjC.PassKit.Internal.Classes
import ObjC.PassKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithStatus:errors:@
initWithStatus_errors :: (IsPKPaymentAuthorizationResult pkPaymentAuthorizationResult, IsNSArray errors) => pkPaymentAuthorizationResult -> PKPaymentAuthorizationStatus -> errors -> IO (Id PKPaymentAuthorizationResult)
initWithStatus_errors pkPaymentAuthorizationResult  status errors =
  withObjCPtr errors $ \raw_errors ->
      sendMsg pkPaymentAuthorizationResult (mkSelector "initWithStatus:errors:") (retPtr retVoid) [argCLong (coerce status), argPtr (castPtr raw_errors :: Ptr ())] >>= ownedObject . castPtr

-- | @- status@
status :: IsPKPaymentAuthorizationResult pkPaymentAuthorizationResult => pkPaymentAuthorizationResult -> IO PKPaymentAuthorizationStatus
status pkPaymentAuthorizationResult  =
    fmap (coerce :: CLong -> PKPaymentAuthorizationStatus) $ sendMsg pkPaymentAuthorizationResult (mkSelector "status") retCLong []

-- | @- setStatus:@
setStatus :: IsPKPaymentAuthorizationResult pkPaymentAuthorizationResult => pkPaymentAuthorizationResult -> PKPaymentAuthorizationStatus -> IO ()
setStatus pkPaymentAuthorizationResult  value =
    sendMsg pkPaymentAuthorizationResult (mkSelector "setStatus:") retVoid [argCLong (coerce value)]

-- | @- errors@
errors :: IsPKPaymentAuthorizationResult pkPaymentAuthorizationResult => pkPaymentAuthorizationResult -> IO (Id NSArray)
errors pkPaymentAuthorizationResult  =
    sendMsg pkPaymentAuthorizationResult (mkSelector "errors") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setErrors:@
setErrors :: (IsPKPaymentAuthorizationResult pkPaymentAuthorizationResult, IsNSArray value) => pkPaymentAuthorizationResult -> value -> IO ()
setErrors pkPaymentAuthorizationResult  value =
  withObjCPtr value $ \raw_value ->
      sendMsg pkPaymentAuthorizationResult (mkSelector "setErrors:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- orderDetails@
orderDetails :: IsPKPaymentAuthorizationResult pkPaymentAuthorizationResult => pkPaymentAuthorizationResult -> IO (Id PKPaymentOrderDetails)
orderDetails pkPaymentAuthorizationResult  =
    sendMsg pkPaymentAuthorizationResult (mkSelector "orderDetails") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOrderDetails:@
setOrderDetails :: (IsPKPaymentAuthorizationResult pkPaymentAuthorizationResult, IsPKPaymentOrderDetails value) => pkPaymentAuthorizationResult -> value -> IO ()
setOrderDetails pkPaymentAuthorizationResult  value =
  withObjCPtr value $ \raw_value ->
      sendMsg pkPaymentAuthorizationResult (mkSelector "setOrderDetails:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithStatus:errors:@
initWithStatus_errorsSelector :: Selector
initWithStatus_errorsSelector = mkSelector "initWithStatus:errors:"

-- | @Selector@ for @status@
statusSelector :: Selector
statusSelector = mkSelector "status"

-- | @Selector@ for @setStatus:@
setStatusSelector :: Selector
setStatusSelector = mkSelector "setStatus:"

-- | @Selector@ for @errors@
errorsSelector :: Selector
errorsSelector = mkSelector "errors"

-- | @Selector@ for @setErrors:@
setErrorsSelector :: Selector
setErrorsSelector = mkSelector "setErrors:"

-- | @Selector@ for @orderDetails@
orderDetailsSelector :: Selector
orderDetailsSelector = mkSelector "orderDetails"

-- | @Selector@ for @setOrderDetails:@
setOrderDetailsSelector :: Selector
setOrderDetailsSelector = mkSelector "setOrderDetails:"

