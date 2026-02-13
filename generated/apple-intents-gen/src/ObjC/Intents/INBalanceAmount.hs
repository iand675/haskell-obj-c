{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , amountSelector
  , balanceTypeSelector
  , currencyCodeSelector
  , initSelector
  , initWithAmount_balanceTypeSelector
  , initWithAmount_currencyCodeSelector

  -- * Enum types
  , INBalanceType(INBalanceType)
  , pattern INBalanceTypeUnknown
  , pattern INBalanceTypeMoney
  , pattern INBalanceTypePoints
  , pattern INBalanceTypeMiles

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
init_ :: IsINBalanceAmount inBalanceAmount => inBalanceAmount -> IO RawId
init_ inBalanceAmount =
  sendOwnedMessage inBalanceAmount initSelector

-- | @- initWithAmount:balanceType:@
initWithAmount_balanceType :: (IsINBalanceAmount inBalanceAmount, IsNSDecimalNumber amount) => inBalanceAmount -> amount -> INBalanceType -> IO (Id INBalanceAmount)
initWithAmount_balanceType inBalanceAmount amount balanceType =
  sendOwnedMessage inBalanceAmount initWithAmount_balanceTypeSelector (toNSDecimalNumber amount) balanceType

-- | @- initWithAmount:currencyCode:@
initWithAmount_currencyCode :: (IsINBalanceAmount inBalanceAmount, IsNSDecimalNumber amount, IsNSString currencyCode) => inBalanceAmount -> amount -> currencyCode -> IO (Id INBalanceAmount)
initWithAmount_currencyCode inBalanceAmount amount currencyCode =
  sendOwnedMessage inBalanceAmount initWithAmount_currencyCodeSelector (toNSDecimalNumber amount) (toNSString currencyCode)

-- | @- amount@
amount :: IsINBalanceAmount inBalanceAmount => inBalanceAmount -> IO (Id NSDecimalNumber)
amount inBalanceAmount =
  sendMessage inBalanceAmount amountSelector

-- | @- balanceType@
balanceType :: IsINBalanceAmount inBalanceAmount => inBalanceAmount -> IO INBalanceType
balanceType inBalanceAmount =
  sendMessage inBalanceAmount balanceTypeSelector

-- | @- currencyCode@
currencyCode :: IsINBalanceAmount inBalanceAmount => inBalanceAmount -> IO (Id NSString)
currencyCode inBalanceAmount =
  sendMessage inBalanceAmount currencyCodeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithAmount:balanceType:@
initWithAmount_balanceTypeSelector :: Selector '[Id NSDecimalNumber, INBalanceType] (Id INBalanceAmount)
initWithAmount_balanceTypeSelector = mkSelector "initWithAmount:balanceType:"

-- | @Selector@ for @initWithAmount:currencyCode:@
initWithAmount_currencyCodeSelector :: Selector '[Id NSDecimalNumber, Id NSString] (Id INBalanceAmount)
initWithAmount_currencyCodeSelector = mkSelector "initWithAmount:currencyCode:"

-- | @Selector@ for @amount@
amountSelector :: Selector '[] (Id NSDecimalNumber)
amountSelector = mkSelector "amount"

-- | @Selector@ for @balanceType@
balanceTypeSelector :: Selector '[] INBalanceType
balanceTypeSelector = mkSelector "balanceType"

-- | @Selector@ for @currencyCode@
currencyCodeSelector :: Selector '[] (Id NSString)
currencyCodeSelector = mkSelector "currencyCode"

