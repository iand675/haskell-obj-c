{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKPaymentMethod@.
module ObjC.PassKit.PKPaymentMethod
  ( PKPaymentMethod
  , IsPKPaymentMethod(..)
  , displayName
  , network
  , type_
  , paymentPass
  , secureElementPass
  , displayNameSelector
  , networkSelector
  , paymentPassSelector
  , secureElementPassSelector
  , typeSelector

  -- * Enum types
  , PKPaymentMethodType(PKPaymentMethodType)
  , pattern PKPaymentMethodTypeUnknown
  , pattern PKPaymentMethodTypeDebit
  , pattern PKPaymentMethodTypeCredit
  , pattern PKPaymentMethodTypePrepaid
  , pattern PKPaymentMethodTypeStore
  , pattern PKPaymentMethodTypeEMoney

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

-- | @- displayName@
displayName :: IsPKPaymentMethod pkPaymentMethod => pkPaymentMethod -> IO (Id NSString)
displayName pkPaymentMethod =
  sendMessage pkPaymentMethod displayNameSelector

-- | @- network@
network :: IsPKPaymentMethod pkPaymentMethod => pkPaymentMethod -> IO (Id NSString)
network pkPaymentMethod =
  sendMessage pkPaymentMethod networkSelector

-- | @- type@
type_ :: IsPKPaymentMethod pkPaymentMethod => pkPaymentMethod -> IO PKPaymentMethodType
type_ pkPaymentMethod =
  sendMessage pkPaymentMethod typeSelector

-- | @- paymentPass@
paymentPass :: IsPKPaymentMethod pkPaymentMethod => pkPaymentMethod -> IO (Id PKPaymentPass)
paymentPass pkPaymentMethod =
  sendMessage pkPaymentMethod paymentPassSelector

-- | @- secureElementPass@
secureElementPass :: IsPKPaymentMethod pkPaymentMethod => pkPaymentMethod -> IO (Id PKSecureElementPass)
secureElementPass pkPaymentMethod =
  sendMessage pkPaymentMethod secureElementPassSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @displayName@
displayNameSelector :: Selector '[] (Id NSString)
displayNameSelector = mkSelector "displayName"

-- | @Selector@ for @network@
networkSelector :: Selector '[] (Id NSString)
networkSelector = mkSelector "network"

-- | @Selector@ for @type@
typeSelector :: Selector '[] PKPaymentMethodType
typeSelector = mkSelector "type"

-- | @Selector@ for @paymentPass@
paymentPassSelector :: Selector '[] (Id PKPaymentPass)
paymentPassSelector = mkSelector "paymentPass"

-- | @Selector@ for @secureElementPass@
secureElementPassSelector :: Selector '[] (Id PKSecureElementPass)
secureElementPassSelector = mkSelector "secureElementPass"

