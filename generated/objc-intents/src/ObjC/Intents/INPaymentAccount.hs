{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , initWithNickname_number_accountType_organizationName_balance_secondaryBalanceSelector
  , initWithNickname_number_accountType_organizationNameSelector
  , nicknameSelector
  , accountNumberSelector
  , accountTypeSelector
  , organizationNameSelector
  , balanceSelector
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
init_ :: IsINPaymentAccount inPaymentAccount => inPaymentAccount -> IO (Id INPaymentAccount)
init_ inPaymentAccount  =
  sendMsg inPaymentAccount (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithNickname:number:accountType:organizationName:balance:secondaryBalance:@
initWithNickname_number_accountType_organizationName_balance_secondaryBalance :: (IsINPaymentAccount inPaymentAccount, IsINSpeakableString nickname, IsNSString number, IsINSpeakableString organizationName, IsINBalanceAmount balance, IsINBalanceAmount secondaryBalance) => inPaymentAccount -> nickname -> number -> INAccountType -> organizationName -> balance -> secondaryBalance -> IO (Id INPaymentAccount)
initWithNickname_number_accountType_organizationName_balance_secondaryBalance inPaymentAccount  nickname number accountType organizationName balance secondaryBalance =
withObjCPtr nickname $ \raw_nickname ->
  withObjCPtr number $ \raw_number ->
    withObjCPtr organizationName $ \raw_organizationName ->
      withObjCPtr balance $ \raw_balance ->
        withObjCPtr secondaryBalance $ \raw_secondaryBalance ->
            sendMsg inPaymentAccount (mkSelector "initWithNickname:number:accountType:organizationName:balance:secondaryBalance:") (retPtr retVoid) [argPtr (castPtr raw_nickname :: Ptr ()), argPtr (castPtr raw_number :: Ptr ()), argCLong (coerce accountType), argPtr (castPtr raw_organizationName :: Ptr ()), argPtr (castPtr raw_balance :: Ptr ()), argPtr (castPtr raw_secondaryBalance :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithNickname:number:accountType:organizationName:@
initWithNickname_number_accountType_organizationName :: (IsINPaymentAccount inPaymentAccount, IsINSpeakableString nickname, IsNSString number, IsINSpeakableString organizationName) => inPaymentAccount -> nickname -> number -> INAccountType -> organizationName -> IO (Id INPaymentAccount)
initWithNickname_number_accountType_organizationName inPaymentAccount  nickname number accountType organizationName =
withObjCPtr nickname $ \raw_nickname ->
  withObjCPtr number $ \raw_number ->
    withObjCPtr organizationName $ \raw_organizationName ->
        sendMsg inPaymentAccount (mkSelector "initWithNickname:number:accountType:organizationName:") (retPtr retVoid) [argPtr (castPtr raw_nickname :: Ptr ()), argPtr (castPtr raw_number :: Ptr ()), argCLong (coerce accountType), argPtr (castPtr raw_organizationName :: Ptr ())] >>= ownedObject . castPtr

-- | @- nickname@
nickname :: IsINPaymentAccount inPaymentAccount => inPaymentAccount -> IO (Id INSpeakableString)
nickname inPaymentAccount  =
  sendMsg inPaymentAccount (mkSelector "nickname") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- accountNumber@
accountNumber :: IsINPaymentAccount inPaymentAccount => inPaymentAccount -> IO (Id NSString)
accountNumber inPaymentAccount  =
  sendMsg inPaymentAccount (mkSelector "accountNumber") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- accountType@
accountType :: IsINPaymentAccount inPaymentAccount => inPaymentAccount -> IO INAccountType
accountType inPaymentAccount  =
  fmap (coerce :: CLong -> INAccountType) $ sendMsg inPaymentAccount (mkSelector "accountType") retCLong []

-- | @- organizationName@
organizationName :: IsINPaymentAccount inPaymentAccount => inPaymentAccount -> IO (Id INSpeakableString)
organizationName inPaymentAccount  =
  sendMsg inPaymentAccount (mkSelector "organizationName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- balance@
balance :: IsINPaymentAccount inPaymentAccount => inPaymentAccount -> IO (Id INBalanceAmount)
balance inPaymentAccount  =
  sendMsg inPaymentAccount (mkSelector "balance") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- secondaryBalance@
secondaryBalance :: IsINPaymentAccount inPaymentAccount => inPaymentAccount -> IO (Id INBalanceAmount)
secondaryBalance inPaymentAccount  =
  sendMsg inPaymentAccount (mkSelector "secondaryBalance") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithNickname:number:accountType:organizationName:balance:secondaryBalance:@
initWithNickname_number_accountType_organizationName_balance_secondaryBalanceSelector :: Selector
initWithNickname_number_accountType_organizationName_balance_secondaryBalanceSelector = mkSelector "initWithNickname:number:accountType:organizationName:balance:secondaryBalance:"

-- | @Selector@ for @initWithNickname:number:accountType:organizationName:@
initWithNickname_number_accountType_organizationNameSelector :: Selector
initWithNickname_number_accountType_organizationNameSelector = mkSelector "initWithNickname:number:accountType:organizationName:"

-- | @Selector@ for @nickname@
nicknameSelector :: Selector
nicknameSelector = mkSelector "nickname"

-- | @Selector@ for @accountNumber@
accountNumberSelector :: Selector
accountNumberSelector = mkSelector "accountNumber"

-- | @Selector@ for @accountType@
accountTypeSelector :: Selector
accountTypeSelector = mkSelector "accountType"

-- | @Selector@ for @organizationName@
organizationNameSelector :: Selector
organizationNameSelector = mkSelector "organizationName"

-- | @Selector@ for @balance@
balanceSelector :: Selector
balanceSelector = mkSelector "balance"

-- | @Selector@ for @secondaryBalance@
secondaryBalanceSelector :: Selector
secondaryBalanceSelector = mkSelector "secondaryBalance"

