{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INBalanceAmount@.
module ObjC.Intents.INBalanceAmount
  ( INBalanceAmount
  , IsINBalanceAmount(..)
  , init_
  , initWithAmount_balanceType
  , initWithAmount_currencyCode
  , amount
  , balanceType
  , currencyCode
  , initSelector
  , initWithAmount_balanceTypeSelector
  , initWithAmount_currencyCodeSelector
  , amountSelector
  , balanceTypeSelector
  , currencyCodeSelector

  -- * Enum types
  , INBalanceType(INBalanceType)
  , pattern INBalanceTypeUnknown
  , pattern INBalanceTypeMoney
  , pattern INBalanceTypePoints
  , pattern INBalanceTypeMiles

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
init_ :: IsINBalanceAmount inBalanceAmount => inBalanceAmount -> IO RawId
init_ inBalanceAmount  =
  fmap (RawId . castPtr) $ sendMsg inBalanceAmount (mkSelector "init") (retPtr retVoid) []

-- | @- initWithAmount:balanceType:@
initWithAmount_balanceType :: (IsINBalanceAmount inBalanceAmount, IsNSDecimalNumber amount) => inBalanceAmount -> amount -> INBalanceType -> IO (Id INBalanceAmount)
initWithAmount_balanceType inBalanceAmount  amount balanceType =
withObjCPtr amount $ \raw_amount ->
    sendMsg inBalanceAmount (mkSelector "initWithAmount:balanceType:") (retPtr retVoid) [argPtr (castPtr raw_amount :: Ptr ()), argCLong (coerce balanceType)] >>= ownedObject . castPtr

-- | @- initWithAmount:currencyCode:@
initWithAmount_currencyCode :: (IsINBalanceAmount inBalanceAmount, IsNSDecimalNumber amount, IsNSString currencyCode) => inBalanceAmount -> amount -> currencyCode -> IO (Id INBalanceAmount)
initWithAmount_currencyCode inBalanceAmount  amount currencyCode =
withObjCPtr amount $ \raw_amount ->
  withObjCPtr currencyCode $ \raw_currencyCode ->
      sendMsg inBalanceAmount (mkSelector "initWithAmount:currencyCode:") (retPtr retVoid) [argPtr (castPtr raw_amount :: Ptr ()), argPtr (castPtr raw_currencyCode :: Ptr ())] >>= ownedObject . castPtr

-- | @- amount@
amount :: IsINBalanceAmount inBalanceAmount => inBalanceAmount -> IO (Id NSDecimalNumber)
amount inBalanceAmount  =
  sendMsg inBalanceAmount (mkSelector "amount") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- balanceType@
balanceType :: IsINBalanceAmount inBalanceAmount => inBalanceAmount -> IO INBalanceType
balanceType inBalanceAmount  =
  fmap (coerce :: CLong -> INBalanceType) $ sendMsg inBalanceAmount (mkSelector "balanceType") retCLong []

-- | @- currencyCode@
currencyCode :: IsINBalanceAmount inBalanceAmount => inBalanceAmount -> IO (Id NSString)
currencyCode inBalanceAmount  =
  sendMsg inBalanceAmount (mkSelector "currencyCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithAmount:balanceType:@
initWithAmount_balanceTypeSelector :: Selector
initWithAmount_balanceTypeSelector = mkSelector "initWithAmount:balanceType:"

-- | @Selector@ for @initWithAmount:currencyCode:@
initWithAmount_currencyCodeSelector :: Selector
initWithAmount_currencyCodeSelector = mkSelector "initWithAmount:currencyCode:"

-- | @Selector@ for @amount@
amountSelector :: Selector
amountSelector = mkSelector "amount"

-- | @Selector@ for @balanceType@
balanceTypeSelector :: Selector
balanceTypeSelector = mkSelector "balanceType"

-- | @Selector@ for @currencyCode@
currencyCodeSelector :: Selector
currencyCodeSelector = mkSelector "currencyCode"

