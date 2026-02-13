{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , errorsSelector
  , initWithStatus_errorsSelector
  , orderDetailsSelector
  , setErrorsSelector
  , setOrderDetailsSelector
  , setStatusSelector
  , statusSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PassKit.Internal.Classes
import ObjC.PassKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithStatus:errors:@
initWithStatus_errors :: (IsPKPaymentAuthorizationResult pkPaymentAuthorizationResult, IsNSArray errors) => pkPaymentAuthorizationResult -> PKPaymentAuthorizationStatus -> errors -> IO (Id PKPaymentAuthorizationResult)
initWithStatus_errors pkPaymentAuthorizationResult status errors =
  sendOwnedMessage pkPaymentAuthorizationResult initWithStatus_errorsSelector status (toNSArray errors)

-- | @- status@
status :: IsPKPaymentAuthorizationResult pkPaymentAuthorizationResult => pkPaymentAuthorizationResult -> IO PKPaymentAuthorizationStatus
status pkPaymentAuthorizationResult =
  sendMessage pkPaymentAuthorizationResult statusSelector

-- | @- setStatus:@
setStatus :: IsPKPaymentAuthorizationResult pkPaymentAuthorizationResult => pkPaymentAuthorizationResult -> PKPaymentAuthorizationStatus -> IO ()
setStatus pkPaymentAuthorizationResult value =
  sendMessage pkPaymentAuthorizationResult setStatusSelector value

-- | @- errors@
errors :: IsPKPaymentAuthorizationResult pkPaymentAuthorizationResult => pkPaymentAuthorizationResult -> IO (Id NSArray)
errors pkPaymentAuthorizationResult =
  sendMessage pkPaymentAuthorizationResult errorsSelector

-- | @- setErrors:@
setErrors :: (IsPKPaymentAuthorizationResult pkPaymentAuthorizationResult, IsNSArray value) => pkPaymentAuthorizationResult -> value -> IO ()
setErrors pkPaymentAuthorizationResult value =
  sendMessage pkPaymentAuthorizationResult setErrorsSelector (toNSArray value)

-- | @- orderDetails@
orderDetails :: IsPKPaymentAuthorizationResult pkPaymentAuthorizationResult => pkPaymentAuthorizationResult -> IO (Id PKPaymentOrderDetails)
orderDetails pkPaymentAuthorizationResult =
  sendMessage pkPaymentAuthorizationResult orderDetailsSelector

-- | @- setOrderDetails:@
setOrderDetails :: (IsPKPaymentAuthorizationResult pkPaymentAuthorizationResult, IsPKPaymentOrderDetails value) => pkPaymentAuthorizationResult -> value -> IO ()
setOrderDetails pkPaymentAuthorizationResult value =
  sendMessage pkPaymentAuthorizationResult setOrderDetailsSelector (toPKPaymentOrderDetails value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithStatus:errors:@
initWithStatus_errorsSelector :: Selector '[PKPaymentAuthorizationStatus, Id NSArray] (Id PKPaymentAuthorizationResult)
initWithStatus_errorsSelector = mkSelector "initWithStatus:errors:"

-- | @Selector@ for @status@
statusSelector :: Selector '[] PKPaymentAuthorizationStatus
statusSelector = mkSelector "status"

-- | @Selector@ for @setStatus:@
setStatusSelector :: Selector '[PKPaymentAuthorizationStatus] ()
setStatusSelector = mkSelector "setStatus:"

-- | @Selector@ for @errors@
errorsSelector :: Selector '[] (Id NSArray)
errorsSelector = mkSelector "errors"

-- | @Selector@ for @setErrors:@
setErrorsSelector :: Selector '[Id NSArray] ()
setErrorsSelector = mkSelector "setErrors:"

-- | @Selector@ for @orderDetails@
orderDetailsSelector :: Selector '[] (Id PKPaymentOrderDetails)
orderDetailsSelector = mkSelector "orderDetails"

-- | @Selector@ for @setOrderDetails:@
setOrderDetailsSelector :: Selector '[Id PKPaymentOrderDetails] ()
setOrderDetailsSelector = mkSelector "setOrderDetails:"

