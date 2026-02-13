{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INPaymentAmount@.
module ObjC.Intents.INPaymentAmount
  ( INPaymentAmount
  , IsINPaymentAmount(..)
  , init_
  , initWithAmountType_amount
  , amount
  , amountType
  , amountSelector
  , amountTypeSelector
  , initSelector
  , initWithAmountType_amountSelector

  -- * Enum types
  , INAmountType(INAmountType)
  , pattern INAmountTypeUnknown
  , pattern INAmountTypeMinimumDue
  , pattern INAmountTypeAmountDue
  , pattern INAmountTypeCurrentBalance
  , pattern INAmountTypeMaximumTransferAmount
  , pattern INAmountTypeMinimumTransferAmount
  , pattern INAmountTypeStatementBalance

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
init_ :: IsINPaymentAmount inPaymentAmount => inPaymentAmount -> IO (Id INPaymentAmount)
init_ inPaymentAmount =
  sendOwnedMessage inPaymentAmount initSelector

-- | @- initWithAmountType:amount:@
initWithAmountType_amount :: (IsINPaymentAmount inPaymentAmount, IsINCurrencyAmount amount) => inPaymentAmount -> INAmountType -> amount -> IO (Id INPaymentAmount)
initWithAmountType_amount inPaymentAmount amountType amount =
  sendOwnedMessage inPaymentAmount initWithAmountType_amountSelector amountType (toINCurrencyAmount amount)

-- | @- amount@
amount :: IsINPaymentAmount inPaymentAmount => inPaymentAmount -> IO (Id INCurrencyAmount)
amount inPaymentAmount =
  sendMessage inPaymentAmount amountSelector

-- | @- amountType@
amountType :: IsINPaymentAmount inPaymentAmount => inPaymentAmount -> IO INAmountType
amountType inPaymentAmount =
  sendMessage inPaymentAmount amountTypeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INPaymentAmount)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithAmountType:amount:@
initWithAmountType_amountSelector :: Selector '[INAmountType, Id INCurrencyAmount] (Id INPaymentAmount)
initWithAmountType_amountSelector = mkSelector "initWithAmountType:amount:"

-- | @Selector@ for @amount@
amountSelector :: Selector '[] (Id INCurrencyAmount)
amountSelector = mkSelector "amount"

-- | @Selector@ for @amountType@
amountTypeSelector :: Selector '[] INAmountType
amountTypeSelector = mkSelector "amountType"

