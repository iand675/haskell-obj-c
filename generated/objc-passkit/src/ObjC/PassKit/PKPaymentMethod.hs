{-# LANGUAGE PatternSynonyms #-}
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
  , typeSelector
  , paymentPassSelector
  , secureElementPassSelector

  -- * Enum types
  , PKPaymentMethodType(PKPaymentMethodType)
  , pattern PKPaymentMethodTypeUnknown
  , pattern PKPaymentMethodTypeDebit
  , pattern PKPaymentMethodTypeCredit
  , pattern PKPaymentMethodTypePrepaid
  , pattern PKPaymentMethodTypeStore
  , pattern PKPaymentMethodTypeEMoney

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

-- | @- displayName@
displayName :: IsPKPaymentMethod pkPaymentMethod => pkPaymentMethod -> IO (Id NSString)
displayName pkPaymentMethod  =
  sendMsg pkPaymentMethod (mkSelector "displayName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- network@
network :: IsPKPaymentMethod pkPaymentMethod => pkPaymentMethod -> IO (Id NSString)
network pkPaymentMethod  =
  sendMsg pkPaymentMethod (mkSelector "network") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- type@
type_ :: IsPKPaymentMethod pkPaymentMethod => pkPaymentMethod -> IO PKPaymentMethodType
type_ pkPaymentMethod  =
  fmap (coerce :: CULong -> PKPaymentMethodType) $ sendMsg pkPaymentMethod (mkSelector "type") retCULong []

-- | @- paymentPass@
paymentPass :: IsPKPaymentMethod pkPaymentMethod => pkPaymentMethod -> IO (Id PKPaymentPass)
paymentPass pkPaymentMethod  =
  sendMsg pkPaymentMethod (mkSelector "paymentPass") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- secureElementPass@
secureElementPass :: IsPKPaymentMethod pkPaymentMethod => pkPaymentMethod -> IO (Id PKSecureElementPass)
secureElementPass pkPaymentMethod  =
  sendMsg pkPaymentMethod (mkSelector "secureElementPass") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @displayName@
displayNameSelector :: Selector
displayNameSelector = mkSelector "displayName"

-- | @Selector@ for @network@
networkSelector :: Selector
networkSelector = mkSelector "network"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @paymentPass@
paymentPassSelector :: Selector
paymentPassSelector = mkSelector "paymentPass"

-- | @Selector@ for @secureElementPass@
secureElementPassSelector :: Selector
secureElementPassSelector = mkSelector "secureElementPass"

