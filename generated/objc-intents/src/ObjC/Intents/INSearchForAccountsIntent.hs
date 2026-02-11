{-# LANGUAGE PatternSynonyms #-}
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
  , initWithAccountNickname_accountType_organizationName_requestedBalanceTypeSelector
  , accountNicknameSelector
  , accountTypeSelector
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

-- | @- initWithAccountNickname:accountType:organizationName:requestedBalanceType:@
initWithAccountNickname_accountType_organizationName_requestedBalanceType :: (IsINSearchForAccountsIntent inSearchForAccountsIntent, IsINSpeakableString accountNickname, IsINSpeakableString organizationName) => inSearchForAccountsIntent -> accountNickname -> INAccountType -> organizationName -> INBalanceType -> IO (Id INSearchForAccountsIntent)
initWithAccountNickname_accountType_organizationName_requestedBalanceType inSearchForAccountsIntent  accountNickname accountType organizationName requestedBalanceType =
withObjCPtr accountNickname $ \raw_accountNickname ->
  withObjCPtr organizationName $ \raw_organizationName ->
      sendMsg inSearchForAccountsIntent (mkSelector "initWithAccountNickname:accountType:organizationName:requestedBalanceType:") (retPtr retVoid) [argPtr (castPtr raw_accountNickname :: Ptr ()), argCLong (coerce accountType), argPtr (castPtr raw_organizationName :: Ptr ()), argCLong (coerce requestedBalanceType)] >>= ownedObject . castPtr

-- | @- accountNickname@
accountNickname :: IsINSearchForAccountsIntent inSearchForAccountsIntent => inSearchForAccountsIntent -> IO (Id INSpeakableString)
accountNickname inSearchForAccountsIntent  =
  sendMsg inSearchForAccountsIntent (mkSelector "accountNickname") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- accountType@
accountType :: IsINSearchForAccountsIntent inSearchForAccountsIntent => inSearchForAccountsIntent -> IO INAccountType
accountType inSearchForAccountsIntent  =
  fmap (coerce :: CLong -> INAccountType) $ sendMsg inSearchForAccountsIntent (mkSelector "accountType") retCLong []

-- | @- organizationName@
organizationName :: IsINSearchForAccountsIntent inSearchForAccountsIntent => inSearchForAccountsIntent -> IO (Id INSpeakableString)
organizationName inSearchForAccountsIntent  =
  sendMsg inSearchForAccountsIntent (mkSelector "organizationName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- requestedBalanceType@
requestedBalanceType :: IsINSearchForAccountsIntent inSearchForAccountsIntent => inSearchForAccountsIntent -> IO INBalanceType
requestedBalanceType inSearchForAccountsIntent  =
  fmap (coerce :: CLong -> INBalanceType) $ sendMsg inSearchForAccountsIntent (mkSelector "requestedBalanceType") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithAccountNickname:accountType:organizationName:requestedBalanceType:@
initWithAccountNickname_accountType_organizationName_requestedBalanceTypeSelector :: Selector
initWithAccountNickname_accountType_organizationName_requestedBalanceTypeSelector = mkSelector "initWithAccountNickname:accountType:organizationName:requestedBalanceType:"

-- | @Selector@ for @accountNickname@
accountNicknameSelector :: Selector
accountNicknameSelector = mkSelector "accountNickname"

-- | @Selector@ for @accountType@
accountTypeSelector :: Selector
accountTypeSelector = mkSelector "accountType"

-- | @Selector@ for @organizationName@
organizationNameSelector :: Selector
organizationNameSelector = mkSelector "organizationName"

-- | @Selector@ for @requestedBalanceType@
requestedBalanceTypeSelector :: Selector
requestedBalanceTypeSelector = mkSelector "requestedBalanceType"

