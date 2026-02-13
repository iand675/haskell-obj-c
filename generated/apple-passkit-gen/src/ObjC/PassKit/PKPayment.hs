{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKPayment@.
module ObjC.PassKit.PKPayment
  ( PKPayment
  , IsPKPayment(..)
  , token
  , billingContact
  , shippingContact
  , shippingMethod
  , billingContactSelector
  , shippingContactSelector
  , shippingMethodSelector
  , tokenSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PassKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- token@
token :: IsPKPayment pkPayment => pkPayment -> IO (Id PKPaymentToken)
token pkPayment =
  sendMessage pkPayment tokenSelector

-- | @- billingContact@
billingContact :: IsPKPayment pkPayment => pkPayment -> IO (Id PKContact)
billingContact pkPayment =
  sendMessage pkPayment billingContactSelector

-- | @- shippingContact@
shippingContact :: IsPKPayment pkPayment => pkPayment -> IO (Id PKContact)
shippingContact pkPayment =
  sendMessage pkPayment shippingContactSelector

-- | @- shippingMethod@
shippingMethod :: IsPKPayment pkPayment => pkPayment -> IO (Id PKShippingMethod)
shippingMethod pkPayment =
  sendMessage pkPayment shippingMethodSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @token@
tokenSelector :: Selector '[] (Id PKPaymentToken)
tokenSelector = mkSelector "token"

-- | @Selector@ for @billingContact@
billingContactSelector :: Selector '[] (Id PKContact)
billingContactSelector = mkSelector "billingContact"

-- | @Selector@ for @shippingContact@
shippingContactSelector :: Selector '[] (Id PKContact)
shippingContactSelector = mkSelector "shippingContact"

-- | @Selector@ for @shippingMethod@
shippingMethodSelector :: Selector '[] (Id PKShippingMethod)
shippingMethodSelector = mkSelector "shippingMethod"

