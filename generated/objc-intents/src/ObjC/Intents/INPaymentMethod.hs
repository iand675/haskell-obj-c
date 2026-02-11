{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , initWithType_name_identificationHint_iconSelector
  , applePayPaymentMethodSelector
  , typeSelector
  , nameSelector
  , iconSelector
  , identificationHintSelector

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
init_ :: IsINPaymentMethod inPaymentMethod => inPaymentMethod -> IO (Id INPaymentMethod)
init_ inPaymentMethod  =
  sendMsg inPaymentMethod (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithType:name:identificationHint:icon:@
initWithType_name_identificationHint_icon :: (IsINPaymentMethod inPaymentMethod, IsNSString name, IsNSString identificationHint, IsINImage icon) => inPaymentMethod -> INPaymentMethodType -> name -> identificationHint -> icon -> IO (Id INPaymentMethod)
initWithType_name_identificationHint_icon inPaymentMethod  type_ name identificationHint icon =
withObjCPtr name $ \raw_name ->
  withObjCPtr identificationHint $ \raw_identificationHint ->
    withObjCPtr icon $ \raw_icon ->
        sendMsg inPaymentMethod (mkSelector "initWithType:name:identificationHint:icon:") (retPtr retVoid) [argCLong (coerce type_), argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_identificationHint :: Ptr ()), argPtr (castPtr raw_icon :: Ptr ())] >>= ownedObject . castPtr

-- | @+ applePayPaymentMethod@
applePayPaymentMethod :: IO (Id INPaymentMethod)
applePayPaymentMethod  =
  do
    cls' <- getRequiredClass "INPaymentMethod"
    sendClassMsg cls' (mkSelector "applePayPaymentMethod") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- type@
type_ :: IsINPaymentMethod inPaymentMethod => inPaymentMethod -> IO INPaymentMethodType
type_ inPaymentMethod  =
  fmap (coerce :: CLong -> INPaymentMethodType) $ sendMsg inPaymentMethod (mkSelector "type") retCLong []

-- | @- name@
name :: IsINPaymentMethod inPaymentMethod => inPaymentMethod -> IO (Id NSString)
name inPaymentMethod  =
  sendMsg inPaymentMethod (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- icon@
icon :: IsINPaymentMethod inPaymentMethod => inPaymentMethod -> IO (Id INImage)
icon inPaymentMethod  =
  sendMsg inPaymentMethod (mkSelector "icon") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- identificationHint@
identificationHint :: IsINPaymentMethod inPaymentMethod => inPaymentMethod -> IO (Id NSString)
identificationHint inPaymentMethod  =
  sendMsg inPaymentMethod (mkSelector "identificationHint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithType:name:identificationHint:icon:@
initWithType_name_identificationHint_iconSelector :: Selector
initWithType_name_identificationHint_iconSelector = mkSelector "initWithType:name:identificationHint:icon:"

-- | @Selector@ for @applePayPaymentMethod@
applePayPaymentMethodSelector :: Selector
applePayPaymentMethodSelector = mkSelector "applePayPaymentMethod"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @icon@
iconSelector :: Selector
iconSelector = mkSelector "icon"

-- | @Selector@ for @identificationHint@
identificationHintSelector :: Selector
identificationHintSelector = mkSelector "identificationHint"

