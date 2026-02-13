{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INPaymentAccount@.
module ObjC.Intents.INPaymentAccount
  ( INPaymentAccount
  , IsINPaymentAccount(..)
  , init_
  , initWithNickname_number_accountType_organizationName_balance_secondaryBalance
  , initWithNickname_number_accountType_organizationName
  , nickname
  , accountNumber
  , accountType
  , organizationName
  , balance
  , secondaryBalance
  , accountNumberSelector
  , accountTypeSelector
  , balanceSelector
  , initSelector
  , initWithNickname_number_accountType_organizationNameSelector
  , initWithNickname_number_accountType_organizationName_balance_secondaryBalanceSelector
  , nicknameSelector
  , organizationNameSelector
  , secondaryBalanceSelector

  -- * Enum types
  , INAccountType(INAccountType)
  , pattern INAccountTypeUnknown
  , pattern INAccountTypeChecking
  , pattern INAccountTypeCredit
  , pattern INAccountTypeDebit
  , pattern INAccountTypeInvestment
  , pattern INAccountTypeMortgage
  , pattern INAccountTypePrepaid
  , pattern INAccountTypeSaving

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
init_ :: IsINPaymentAccount inPaymentAccount => inPaymentAccount -> IO (Id INPaymentAccount)
init_ inPaymentAccount =
  sendOwnedMessage inPaymentAccount initSelector

-- | @- initWithNickname:number:accountType:organizationName:balance:secondaryBalance:@
initWithNickname_number_accountType_organizationName_balance_secondaryBalance :: (IsINPaymentAccount inPaymentAccount, IsINSpeakableString nickname, IsNSString number, IsINSpeakableString organizationName, IsINBalanceAmount balance, IsINBalanceAmount secondaryBalance) => inPaymentAccount -> nickname -> number -> INAccountType -> organizationName -> balance -> secondaryBalance -> IO (Id INPaymentAccount)
initWithNickname_number_accountType_organizationName_balance_secondaryBalance inPaymentAccount nickname number accountType organizationName balance secondaryBalance =
  sendOwnedMessage inPaymentAccount initWithNickname_number_accountType_organizationName_balance_secondaryBalanceSelector (toINSpeakableString nickname) (toNSString number) accountType (toINSpeakableString organizationName) (toINBalanceAmount balance) (toINBalanceAmount secondaryBalance)

-- | @- initWithNickname:number:accountType:organizationName:@
initWithNickname_number_accountType_organizationName :: (IsINPaymentAccount inPaymentAccount, IsINSpeakableString nickname, IsNSString number, IsINSpeakableString organizationName) => inPaymentAccount -> nickname -> number -> INAccountType -> organizationName -> IO (Id INPaymentAccount)
initWithNickname_number_accountType_organizationName inPaymentAccount nickname number accountType organizationName =
  sendOwnedMessage inPaymentAccount initWithNickname_number_accountType_organizationNameSelector (toINSpeakableString nickname) (toNSString number) accountType (toINSpeakableString organizationName)

-- | @- nickname@
nickname :: IsINPaymentAccount inPaymentAccount => inPaymentAccount -> IO (Id INSpeakableString)
nickname inPaymentAccount =
  sendMessage inPaymentAccount nicknameSelector

-- | @- accountNumber@
accountNumber :: IsINPaymentAccount inPaymentAccount => inPaymentAccount -> IO (Id NSString)
accountNumber inPaymentAccount =
  sendMessage inPaymentAccount accountNumberSelector

-- | @- accountType@
accountType :: IsINPaymentAccount inPaymentAccount => inPaymentAccount -> IO INAccountType
accountType inPaymentAccount =
  sendMessage inPaymentAccount accountTypeSelector

-- | @- organizationName@
organizationName :: IsINPaymentAccount inPaymentAccount => inPaymentAccount -> IO (Id INSpeakableString)
organizationName inPaymentAccount =
  sendMessage inPaymentAccount organizationNameSelector

-- | @- balance@
balance :: IsINPaymentAccount inPaymentAccount => inPaymentAccount -> IO (Id INBalanceAmount)
balance inPaymentAccount =
  sendMessage inPaymentAccount balanceSelector

-- | @- secondaryBalance@
secondaryBalance :: IsINPaymentAccount inPaymentAccount => inPaymentAccount -> IO (Id INBalanceAmount)
secondaryBalance inPaymentAccount =
  sendMessage inPaymentAccount secondaryBalanceSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INPaymentAccount)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithNickname:number:accountType:organizationName:balance:secondaryBalance:@
initWithNickname_number_accountType_organizationName_balance_secondaryBalanceSelector :: Selector '[Id INSpeakableString, Id NSString, INAccountType, Id INSpeakableString, Id INBalanceAmount, Id INBalanceAmount] (Id INPaymentAccount)
initWithNickname_number_accountType_organizationName_balance_secondaryBalanceSelector = mkSelector "initWithNickname:number:accountType:organizationName:balance:secondaryBalance:"

-- | @Selector@ for @initWithNickname:number:accountType:organizationName:@
initWithNickname_number_accountType_organizationNameSelector :: Selector '[Id INSpeakableString, Id NSString, INAccountType, Id INSpeakableString] (Id INPaymentAccount)
initWithNickname_number_accountType_organizationNameSelector = mkSelector "initWithNickname:number:accountType:organizationName:"

-- | @Selector@ for @nickname@
nicknameSelector :: Selector '[] (Id INSpeakableString)
nicknameSelector = mkSelector "nickname"

-- | @Selector@ for @accountNumber@
accountNumberSelector :: Selector '[] (Id NSString)
accountNumberSelector = mkSelector "accountNumber"

-- | @Selector@ for @accountType@
accountTypeSelector :: Selector '[] INAccountType
accountTypeSelector = mkSelector "accountType"

-- | @Selector@ for @organizationName@
organizationNameSelector :: Selector '[] (Id INSpeakableString)
organizationNameSelector = mkSelector "organizationName"

-- | @Selector@ for @balance@
balanceSelector :: Selector '[] (Id INBalanceAmount)
balanceSelector = mkSelector "balance"

-- | @Selector@ for @secondaryBalance@
secondaryBalanceSelector :: Selector '[] (Id INBalanceAmount)
secondaryBalanceSelector = mkSelector "secondaryBalance"

