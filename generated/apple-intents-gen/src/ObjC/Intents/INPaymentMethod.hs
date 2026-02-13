{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INPaymentMethod@.
module ObjC.Intents.INPaymentMethod
  ( INPaymentMethod
  , IsINPaymentMethod(..)
  , init_
  , initWithType_name_identificationHint_icon
  , applePayPaymentMethod
  , type_
  , name
  , icon
  , identificationHint
  , applePayPaymentMethodSelector
  , iconSelector
  , identificationHintSelector
  , initSelector
  , initWithType_name_identificationHint_iconSelector
  , nameSelector
  , typeSelector

  -- * Enum types
  , INPaymentMethodType(INPaymentMethodType)
  , pattern INPaymentMethodTypeUnknown
  , pattern INPaymentMethodTypeChecking
  , pattern INPaymentMethodTypeSavings
  , pattern INPaymentMethodTypeBrokerage
  , pattern INPaymentMethodTypeDebit
  , pattern INPaymentMethodTypeCredit
  , pattern INPaymentMethodTypePrepaid
  , pattern INPaymentMethodTypeStore
  , pattern INPaymentMethodTypeApplePay

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
init_ :: IsINPaymentMethod inPaymentMethod => inPaymentMethod -> IO (Id INPaymentMethod)
init_ inPaymentMethod =
  sendOwnedMessage inPaymentMethod initSelector

-- | @- initWithType:name:identificationHint:icon:@
initWithType_name_identificationHint_icon :: (IsINPaymentMethod inPaymentMethod, IsNSString name, IsNSString identificationHint, IsINImage icon) => inPaymentMethod -> INPaymentMethodType -> name -> identificationHint -> icon -> IO (Id INPaymentMethod)
initWithType_name_identificationHint_icon inPaymentMethod type_ name identificationHint icon =
  sendOwnedMessage inPaymentMethod initWithType_name_identificationHint_iconSelector type_ (toNSString name) (toNSString identificationHint) (toINImage icon)

-- | @+ applePayPaymentMethod@
applePayPaymentMethod :: IO (Id INPaymentMethod)
applePayPaymentMethod  =
  do
    cls' <- getRequiredClass "INPaymentMethod"
    sendClassMessage cls' applePayPaymentMethodSelector

-- | @- type@
type_ :: IsINPaymentMethod inPaymentMethod => inPaymentMethod -> IO INPaymentMethodType
type_ inPaymentMethod =
  sendMessage inPaymentMethod typeSelector

-- | @- name@
name :: IsINPaymentMethod inPaymentMethod => inPaymentMethod -> IO (Id NSString)
name inPaymentMethod =
  sendMessage inPaymentMethod nameSelector

-- | @- icon@
icon :: IsINPaymentMethod inPaymentMethod => inPaymentMethod -> IO (Id INImage)
icon inPaymentMethod =
  sendMessage inPaymentMethod iconSelector

-- | @- identificationHint@
identificationHint :: IsINPaymentMethod inPaymentMethod => inPaymentMethod -> IO (Id NSString)
identificationHint inPaymentMethod =
  sendMessage inPaymentMethod identificationHintSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INPaymentMethod)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithType:name:identificationHint:icon:@
initWithType_name_identificationHint_iconSelector :: Selector '[INPaymentMethodType, Id NSString, Id NSString, Id INImage] (Id INPaymentMethod)
initWithType_name_identificationHint_iconSelector = mkSelector "initWithType:name:identificationHint:icon:"

-- | @Selector@ for @applePayPaymentMethod@
applePayPaymentMethodSelector :: Selector '[] (Id INPaymentMethod)
applePayPaymentMethodSelector = mkSelector "applePayPaymentMethod"

-- | @Selector@ for @type@
typeSelector :: Selector '[] INPaymentMethodType
typeSelector = mkSelector "type"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @icon@
iconSelector :: Selector '[] (Id INImage)
iconSelector = mkSelector "icon"

-- | @Selector@ for @identificationHint@
identificationHintSelector :: Selector '[] (Id NSString)
identificationHintSelector = mkSelector "identificationHint"

