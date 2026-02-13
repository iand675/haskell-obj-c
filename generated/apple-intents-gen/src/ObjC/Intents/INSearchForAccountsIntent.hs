{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSearchForAccountsIntent@.
module ObjC.Intents.INSearchForAccountsIntent
  ( INSearchForAccountsIntent
  , IsINSearchForAccountsIntent(..)
  , initWithAccountNickname_accountType_organizationName_requestedBalanceType
  , accountNickname
  , accountType
  , organizationName
  , requestedBalanceType
  , accountNicknameSelector
  , accountTypeSelector
  , initWithAccountNickname_accountType_organizationName_requestedBalanceTypeSelector
  , organizationNameSelector
  , requestedBalanceTypeSelector

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

-- | @- initWithAccountNickname:accountType:organizationName:requestedBalanceType:@
initWithAccountNickname_accountType_organizationName_requestedBalanceType :: (IsINSearchForAccountsIntent inSearchForAccountsIntent, IsINSpeakableString accountNickname, IsINSpeakableString organizationName) => inSearchForAccountsIntent -> accountNickname -> INAccountType -> organizationName -> INBalanceType -> IO (Id INSearchForAccountsIntent)
initWithAccountNickname_accountType_organizationName_requestedBalanceType inSearchForAccountsIntent accountNickname accountType organizationName requestedBalanceType =
  sendOwnedMessage inSearchForAccountsIntent initWithAccountNickname_accountType_organizationName_requestedBalanceTypeSelector (toINSpeakableString accountNickname) accountType (toINSpeakableString organizationName) requestedBalanceType

-- | @- accountNickname@
accountNickname :: IsINSearchForAccountsIntent inSearchForAccountsIntent => inSearchForAccountsIntent -> IO (Id INSpeakableString)
accountNickname inSearchForAccountsIntent =
  sendMessage inSearchForAccountsIntent accountNicknameSelector

-- | @- accountType@
accountType :: IsINSearchForAccountsIntent inSearchForAccountsIntent => inSearchForAccountsIntent -> IO INAccountType
accountType inSearchForAccountsIntent =
  sendMessage inSearchForAccountsIntent accountTypeSelector

-- | @- organizationName@
organizationName :: IsINSearchForAccountsIntent inSearchForAccountsIntent => inSearchForAccountsIntent -> IO (Id INSpeakableString)
organizationName inSearchForAccountsIntent =
  sendMessage inSearchForAccountsIntent organizationNameSelector

-- | @- requestedBalanceType@
requestedBalanceType :: IsINSearchForAccountsIntent inSearchForAccountsIntent => inSearchForAccountsIntent -> IO INBalanceType
requestedBalanceType inSearchForAccountsIntent =
  sendMessage inSearchForAccountsIntent requestedBalanceTypeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithAccountNickname:accountType:organizationName:requestedBalanceType:@
initWithAccountNickname_accountType_organizationName_requestedBalanceTypeSelector :: Selector '[Id INSpeakableString, INAccountType, Id INSpeakableString, INBalanceType] (Id INSearchForAccountsIntent)
initWithAccountNickname_accountType_organizationName_requestedBalanceTypeSelector = mkSelector "initWithAccountNickname:accountType:organizationName:requestedBalanceType:"

-- | @Selector@ for @accountNickname@
accountNicknameSelector :: Selector '[] (Id INSpeakableString)
accountNicknameSelector = mkSelector "accountNickname"

-- | @Selector@ for @accountType@
accountTypeSelector :: Selector '[] INAccountType
accountTypeSelector = mkSelector "accountType"

-- | @Selector@ for @organizationName@
organizationNameSelector :: Selector '[] (Id INSpeakableString)
organizationNameSelector = mkSelector "organizationName"

-- | @Selector@ for @requestedBalanceType@
requestedBalanceTypeSelector :: Selector '[] INBalanceType
requestedBalanceTypeSelector = mkSelector "requestedBalanceType"

