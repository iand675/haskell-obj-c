{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKStoredValuePassBalance@.
module ObjC.PassKit.PKStoredValuePassBalance
  ( PKStoredValuePassBalance
  , IsPKStoredValuePassBalance(..)
  , init_
  , new
  , isEqualToBalance
  , amount
  , currencyCode
  , balanceType
  , expiryDate
  , amountSelector
  , balanceTypeSelector
  , currencyCodeSelector
  , expiryDateSelector
  , initSelector
  , isEqualToBalanceSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PassKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPKStoredValuePassBalance pkStoredValuePassBalance => pkStoredValuePassBalance -> IO (Id PKStoredValuePassBalance)
init_ pkStoredValuePassBalance =
  sendOwnedMessage pkStoredValuePassBalance initSelector

-- | @+ new@
new :: IO (Id PKStoredValuePassBalance)
new  =
  do
    cls' <- getRequiredClass "PKStoredValuePassBalance"
    sendOwnedClassMessage cls' newSelector

-- | @- isEqualToBalance:@
isEqualToBalance :: (IsPKStoredValuePassBalance pkStoredValuePassBalance, IsPKStoredValuePassBalance balance) => pkStoredValuePassBalance -> balance -> IO Bool
isEqualToBalance pkStoredValuePassBalance balance =
  sendMessage pkStoredValuePassBalance isEqualToBalanceSelector (toPKStoredValuePassBalance balance)

-- | @- amount@
amount :: IsPKStoredValuePassBalance pkStoredValuePassBalance => pkStoredValuePassBalance -> IO (Id NSDecimalNumber)
amount pkStoredValuePassBalance =
  sendMessage pkStoredValuePassBalance amountSelector

-- | @- currencyCode@
currencyCode :: IsPKStoredValuePassBalance pkStoredValuePassBalance => pkStoredValuePassBalance -> IO (Id NSString)
currencyCode pkStoredValuePassBalance =
  sendMessage pkStoredValuePassBalance currencyCodeSelector

-- | @- balanceType@
balanceType :: IsPKStoredValuePassBalance pkStoredValuePassBalance => pkStoredValuePassBalance -> IO (Id NSString)
balanceType pkStoredValuePassBalance =
  sendMessage pkStoredValuePassBalance balanceTypeSelector

-- | @- expiryDate@
expiryDate :: IsPKStoredValuePassBalance pkStoredValuePassBalance => pkStoredValuePassBalance -> IO (Id NSDate)
expiryDate pkStoredValuePassBalance =
  sendMessage pkStoredValuePassBalance expiryDateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PKStoredValuePassBalance)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PKStoredValuePassBalance)
newSelector = mkSelector "new"

-- | @Selector@ for @isEqualToBalance:@
isEqualToBalanceSelector :: Selector '[Id PKStoredValuePassBalance] Bool
isEqualToBalanceSelector = mkSelector "isEqualToBalance:"

-- | @Selector@ for @amount@
amountSelector :: Selector '[] (Id NSDecimalNumber)
amountSelector = mkSelector "amount"

-- | @Selector@ for @currencyCode@
currencyCodeSelector :: Selector '[] (Id NSString)
currencyCodeSelector = mkSelector "currencyCode"

-- | @Selector@ for @balanceType@
balanceTypeSelector :: Selector '[] (Id NSString)
balanceTypeSelector = mkSelector "balanceType"

-- | @Selector@ for @expiryDate@
expiryDateSelector :: Selector '[] (Id NSDate)
expiryDateSelector = mkSelector "expiryDate"

