{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ACAccountStore@.
module ObjC.Accounts.ACAccountStore
  ( ACAccountStore
  , IsACAccountStore(..)
  , accountWithIdentifier
  , accountTypeWithAccountTypeIdentifier
  , accountsWithAccountType
  , saveAccount_withCompletionHandler
  , requestAccessToAccountsWithType_withCompletionHandler
  , requestAccessToAccountsWithType_options_completion
  , renewCredentialsForAccount_completion
  , removeAccount_withCompletionHandler
  , accounts
  , accountWithIdentifierSelector
  , accountTypeWithAccountTypeIdentifierSelector
  , accountsWithAccountTypeSelector
  , saveAccount_withCompletionHandlerSelector
  , requestAccessToAccountsWithType_withCompletionHandlerSelector
  , requestAccessToAccountsWithType_options_completionSelector
  , renewCredentialsForAccount_completionSelector
  , removeAccount_withCompletionHandlerSelector
  , accountsSelector


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

import ObjC.Accounts.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- accountWithIdentifier:@
accountWithIdentifier :: (IsACAccountStore acAccountStore, IsNSString identifier) => acAccountStore -> identifier -> IO (Id ACAccount)
accountWithIdentifier acAccountStore  identifier =
withObjCPtr identifier $ \raw_identifier ->
    sendMsg acAccountStore (mkSelector "accountWithIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ())] >>= retainedObject . castPtr

-- | @- accountTypeWithAccountTypeIdentifier:@
accountTypeWithAccountTypeIdentifier :: (IsACAccountStore acAccountStore, IsNSString typeIdentifier) => acAccountStore -> typeIdentifier -> IO (Id ACAccountType)
accountTypeWithAccountTypeIdentifier acAccountStore  typeIdentifier =
withObjCPtr typeIdentifier $ \raw_typeIdentifier ->
    sendMsg acAccountStore (mkSelector "accountTypeWithAccountTypeIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_typeIdentifier :: Ptr ())] >>= retainedObject . castPtr

-- | @- accountsWithAccountType:@
accountsWithAccountType :: (IsACAccountStore acAccountStore, IsACAccountType accountType) => acAccountStore -> accountType -> IO (Id NSArray)
accountsWithAccountType acAccountStore  accountType =
withObjCPtr accountType $ \raw_accountType ->
    sendMsg acAccountStore (mkSelector "accountsWithAccountType:") (retPtr retVoid) [argPtr (castPtr raw_accountType :: Ptr ())] >>= retainedObject . castPtr

-- | @- saveAccount:withCompletionHandler:@
saveAccount_withCompletionHandler :: (IsACAccountStore acAccountStore, IsACAccount account) => acAccountStore -> account -> Ptr () -> IO ()
saveAccount_withCompletionHandler acAccountStore  account completionHandler =
withObjCPtr account $ \raw_account ->
    sendMsg acAccountStore (mkSelector "saveAccount:withCompletionHandler:") retVoid [argPtr (castPtr raw_account :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- requestAccessToAccountsWithType:withCompletionHandler:@
requestAccessToAccountsWithType_withCompletionHandler :: (IsACAccountStore acAccountStore, IsACAccountType accountType) => acAccountStore -> accountType -> Ptr () -> IO ()
requestAccessToAccountsWithType_withCompletionHandler acAccountStore  accountType handler =
withObjCPtr accountType $ \raw_accountType ->
    sendMsg acAccountStore (mkSelector "requestAccessToAccountsWithType:withCompletionHandler:") retVoid [argPtr (castPtr raw_accountType :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

-- | @- requestAccessToAccountsWithType:options:completion:@
requestAccessToAccountsWithType_options_completion :: (IsACAccountStore acAccountStore, IsACAccountType accountType, IsNSDictionary options) => acAccountStore -> accountType -> options -> Ptr () -> IO ()
requestAccessToAccountsWithType_options_completion acAccountStore  accountType options completion =
withObjCPtr accountType $ \raw_accountType ->
  withObjCPtr options $ \raw_options ->
      sendMsg acAccountStore (mkSelector "requestAccessToAccountsWithType:options:completion:") retVoid [argPtr (castPtr raw_accountType :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- renewCredentialsForAccount:completion:@
renewCredentialsForAccount_completion :: (IsACAccountStore acAccountStore, IsACAccount account) => acAccountStore -> account -> Ptr () -> IO ()
renewCredentialsForAccount_completion acAccountStore  account completionHandler =
withObjCPtr account $ \raw_account ->
    sendMsg acAccountStore (mkSelector "renewCredentialsForAccount:completion:") retVoid [argPtr (castPtr raw_account :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- removeAccount:withCompletionHandler:@
removeAccount_withCompletionHandler :: (IsACAccountStore acAccountStore, IsACAccount account) => acAccountStore -> account -> Ptr () -> IO ()
removeAccount_withCompletionHandler acAccountStore  account completionHandler =
withObjCPtr account $ \raw_account ->
    sendMsg acAccountStore (mkSelector "removeAccount:withCompletionHandler:") retVoid [argPtr (castPtr raw_account :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- accounts@
accounts :: IsACAccountStore acAccountStore => acAccountStore -> IO (Id NSArray)
accounts acAccountStore  =
  sendMsg acAccountStore (mkSelector "accounts") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @accountWithIdentifier:@
accountWithIdentifierSelector :: Selector
accountWithIdentifierSelector = mkSelector "accountWithIdentifier:"

-- | @Selector@ for @accountTypeWithAccountTypeIdentifier:@
accountTypeWithAccountTypeIdentifierSelector :: Selector
accountTypeWithAccountTypeIdentifierSelector = mkSelector "accountTypeWithAccountTypeIdentifier:"

-- | @Selector@ for @accountsWithAccountType:@
accountsWithAccountTypeSelector :: Selector
accountsWithAccountTypeSelector = mkSelector "accountsWithAccountType:"

-- | @Selector@ for @saveAccount:withCompletionHandler:@
saveAccount_withCompletionHandlerSelector :: Selector
saveAccount_withCompletionHandlerSelector = mkSelector "saveAccount:withCompletionHandler:"

-- | @Selector@ for @requestAccessToAccountsWithType:withCompletionHandler:@
requestAccessToAccountsWithType_withCompletionHandlerSelector :: Selector
requestAccessToAccountsWithType_withCompletionHandlerSelector = mkSelector "requestAccessToAccountsWithType:withCompletionHandler:"

-- | @Selector@ for @requestAccessToAccountsWithType:options:completion:@
requestAccessToAccountsWithType_options_completionSelector :: Selector
requestAccessToAccountsWithType_options_completionSelector = mkSelector "requestAccessToAccountsWithType:options:completion:"

-- | @Selector@ for @renewCredentialsForAccount:completion:@
renewCredentialsForAccount_completionSelector :: Selector
renewCredentialsForAccount_completionSelector = mkSelector "renewCredentialsForAccount:completion:"

-- | @Selector@ for @removeAccount:withCompletionHandler:@
removeAccount_withCompletionHandlerSelector :: Selector
removeAccount_withCompletionHandlerSelector = mkSelector "removeAccount:withCompletionHandler:"

-- | @Selector@ for @accounts@
accountsSelector :: Selector
accountsSelector = mkSelector "accounts"

