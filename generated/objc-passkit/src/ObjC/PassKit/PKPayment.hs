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
  , tokenSelector
  , billingContactSelector
  , shippingContactSelector
  , shippingMethodSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- token@
token :: IsPKPayment pkPayment => pkPayment -> IO (Id PKPaymentToken)
token pkPayment  =
  sendMsg pkPayment (mkSelector "token") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- billingContact@
billingContact :: IsPKPayment pkPayment => pkPayment -> IO (Id PKContact)
billingContact pkPayment  =
  sendMsg pkPayment (mkSelector "billingContact") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- shippingContact@
shippingContact :: IsPKPayment pkPayment => pkPayment -> IO (Id PKContact)
shippingContact pkPayment  =
  sendMsg pkPayment (mkSelector "shippingContact") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- shippingMethod@
shippingMethod :: IsPKPayment pkPayment => pkPayment -> IO (Id PKShippingMethod)
shippingMethod pkPayment  =
  sendMsg pkPayment (mkSelector "shippingMethod") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @token@
tokenSelector :: Selector
tokenSelector = mkSelector "token"

-- | @Selector@ for @billingContact@
billingContactSelector :: Selector
billingContactSelector = mkSelector "billingContact"

-- | @Selector@ for @shippingContact@
shippingContactSelector :: Selector
shippingContactSelector = mkSelector "shippingContact"

-- | @Selector@ for @shippingMethod@
shippingMethodSelector :: Selector
shippingMethodSelector = mkSelector "shippingMethod"

