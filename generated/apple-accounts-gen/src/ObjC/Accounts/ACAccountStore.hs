{-# LANGUAGE DataKinds #-}
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
  , accountTypeWithAccountTypeIdentifierSelector
  , accountWithIdentifierSelector
  , accountsSelector
  , accountsWithAccountTypeSelector
  , removeAccount_withCompletionHandlerSelector
  , renewCredentialsForAccount_completionSelector
  , requestAccessToAccountsWithType_options_completionSelector
  , requestAccessToAccountsWithType_withCompletionHandlerSelector
  , saveAccount_withCompletionHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Accounts.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- accountWithIdentifier:@
accountWithIdentifier :: (IsACAccountStore acAccountStore, IsNSString identifier) => acAccountStore -> identifier -> IO (Id ACAccount)
accountWithIdentifier acAccountStore identifier =
  sendMessage acAccountStore accountWithIdentifierSelector (toNSString identifier)

-- | @- accountTypeWithAccountTypeIdentifier:@
accountTypeWithAccountTypeIdentifier :: (IsACAccountStore acAccountStore, IsNSString typeIdentifier) => acAccountStore -> typeIdentifier -> IO (Id ACAccountType)
accountTypeWithAccountTypeIdentifier acAccountStore typeIdentifier =
  sendMessage acAccountStore accountTypeWithAccountTypeIdentifierSelector (toNSString typeIdentifier)

-- | @- accountsWithAccountType:@
accountsWithAccountType :: (IsACAccountStore acAccountStore, IsACAccountType accountType) => acAccountStore -> accountType -> IO (Id NSArray)
accountsWithAccountType acAccountStore accountType =
  sendMessage acAccountStore accountsWithAccountTypeSelector (toACAccountType accountType)

-- | @- saveAccount:withCompletionHandler:@
saveAccount_withCompletionHandler :: (IsACAccountStore acAccountStore, IsACAccount account) => acAccountStore -> account -> Ptr () -> IO ()
saveAccount_withCompletionHandler acAccountStore account completionHandler =
  sendMessage acAccountStore saveAccount_withCompletionHandlerSelector (toACAccount account) completionHandler

-- | @- requestAccessToAccountsWithType:withCompletionHandler:@
requestAccessToAccountsWithType_withCompletionHandler :: (IsACAccountStore acAccountStore, IsACAccountType accountType) => acAccountStore -> accountType -> Ptr () -> IO ()
requestAccessToAccountsWithType_withCompletionHandler acAccountStore accountType handler =
  sendMessage acAccountStore requestAccessToAccountsWithType_withCompletionHandlerSelector (toACAccountType accountType) handler

-- | @- requestAccessToAccountsWithType:options:completion:@
requestAccessToAccountsWithType_options_completion :: (IsACAccountStore acAccountStore, IsACAccountType accountType, IsNSDictionary options) => acAccountStore -> accountType -> options -> Ptr () -> IO ()
requestAccessToAccountsWithType_options_completion acAccountStore accountType options completion =
  sendMessage acAccountStore requestAccessToAccountsWithType_options_completionSelector (toACAccountType accountType) (toNSDictionary options) completion

-- | @- renewCredentialsForAccount:completion:@
renewCredentialsForAccount_completion :: (IsACAccountStore acAccountStore, IsACAccount account) => acAccountStore -> account -> Ptr () -> IO ()
renewCredentialsForAccount_completion acAccountStore account completionHandler =
  sendMessage acAccountStore renewCredentialsForAccount_completionSelector (toACAccount account) completionHandler

-- | @- removeAccount:withCompletionHandler:@
removeAccount_withCompletionHandler :: (IsACAccountStore acAccountStore, IsACAccount account) => acAccountStore -> account -> Ptr () -> IO ()
removeAccount_withCompletionHandler acAccountStore account completionHandler =
  sendMessage acAccountStore removeAccount_withCompletionHandlerSelector (toACAccount account) completionHandler

-- | @- accounts@
accounts :: IsACAccountStore acAccountStore => acAccountStore -> IO (Id NSArray)
accounts acAccountStore =
  sendMessage acAccountStore accountsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @accountWithIdentifier:@
accountWithIdentifierSelector :: Selector '[Id NSString] (Id ACAccount)
accountWithIdentifierSelector = mkSelector "accountWithIdentifier:"

-- | @Selector@ for @accountTypeWithAccountTypeIdentifier:@
accountTypeWithAccountTypeIdentifierSelector :: Selector '[Id NSString] (Id ACAccountType)
accountTypeWithAccountTypeIdentifierSelector = mkSelector "accountTypeWithAccountTypeIdentifier:"

-- | @Selector@ for @accountsWithAccountType:@
accountsWithAccountTypeSelector :: Selector '[Id ACAccountType] (Id NSArray)
accountsWithAccountTypeSelector = mkSelector "accountsWithAccountType:"

-- | @Selector@ for @saveAccount:withCompletionHandler:@
saveAccount_withCompletionHandlerSelector :: Selector '[Id ACAccount, Ptr ()] ()
saveAccount_withCompletionHandlerSelector = mkSelector "saveAccount:withCompletionHandler:"

-- | @Selector@ for @requestAccessToAccountsWithType:withCompletionHandler:@
requestAccessToAccountsWithType_withCompletionHandlerSelector :: Selector '[Id ACAccountType, Ptr ()] ()
requestAccessToAccountsWithType_withCompletionHandlerSelector = mkSelector "requestAccessToAccountsWithType:withCompletionHandler:"

-- | @Selector@ for @requestAccessToAccountsWithType:options:completion:@
requestAccessToAccountsWithType_options_completionSelector :: Selector '[Id ACAccountType, Id NSDictionary, Ptr ()] ()
requestAccessToAccountsWithType_options_completionSelector = mkSelector "requestAccessToAccountsWithType:options:completion:"

-- | @Selector@ for @renewCredentialsForAccount:completion:@
renewCredentialsForAccount_completionSelector :: Selector '[Id ACAccount, Ptr ()] ()
renewCredentialsForAccount_completionSelector = mkSelector "renewCredentialsForAccount:completion:"

-- | @Selector@ for @removeAccount:withCompletionHandler:@
removeAccount_withCompletionHandlerSelector :: Selector '[Id ACAccount, Ptr ()] ()
removeAccount_withCompletionHandlerSelector = mkSelector "removeAccount:withCompletionHandler:"

-- | @Selector@ for @accounts@
accountsSelector :: Selector '[] (Id NSArray)
accountsSelector = mkSelector "accounts"

