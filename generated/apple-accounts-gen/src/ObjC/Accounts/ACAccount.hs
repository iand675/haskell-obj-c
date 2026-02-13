{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ACAccount@.
module ObjC.Accounts.ACAccount
  ( ACAccount
  , IsACAccount(..)
  , initWithAccountType
  , identifier
  , accountType
  , setAccountType
  , accountDescription
  , setAccountDescription
  , username
  , setUsername
  , userFullName
  , credential
  , setCredential
  , accountDescriptionSelector
  , accountTypeSelector
  , credentialSelector
  , identifierSelector
  , initWithAccountTypeSelector
  , setAccountDescriptionSelector
  , setAccountTypeSelector
  , setCredentialSelector
  , setUsernameSelector
  , userFullNameSelector
  , usernameSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Accounts.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithAccountType:@
initWithAccountType :: (IsACAccount acAccount, IsACAccountType type_) => acAccount -> type_ -> IO (Id ACAccount)
initWithAccountType acAccount type_ =
  sendOwnedMessage acAccount initWithAccountTypeSelector (toACAccountType type_)

-- | @- identifier@
identifier :: IsACAccount acAccount => acAccount -> IO (Id NSString)
identifier acAccount =
  sendMessage acAccount identifierSelector

-- | @- accountType@
accountType :: IsACAccount acAccount => acAccount -> IO (Id ACAccountType)
accountType acAccount =
  sendMessage acAccount accountTypeSelector

-- | @- setAccountType:@
setAccountType :: (IsACAccount acAccount, IsACAccountType value) => acAccount -> value -> IO ()
setAccountType acAccount value =
  sendMessage acAccount setAccountTypeSelector (toACAccountType value)

-- | @- accountDescription@
accountDescription :: IsACAccount acAccount => acAccount -> IO (Id NSString)
accountDescription acAccount =
  sendMessage acAccount accountDescriptionSelector

-- | @- setAccountDescription:@
setAccountDescription :: (IsACAccount acAccount, IsNSString value) => acAccount -> value -> IO ()
setAccountDescription acAccount value =
  sendMessage acAccount setAccountDescriptionSelector (toNSString value)

-- | @- username@
username :: IsACAccount acAccount => acAccount -> IO (Id NSString)
username acAccount =
  sendMessage acAccount usernameSelector

-- | @- setUsername:@
setUsername :: (IsACAccount acAccount, IsNSString value) => acAccount -> value -> IO ()
setUsername acAccount value =
  sendMessage acAccount setUsernameSelector (toNSString value)

-- | @- userFullName@
userFullName :: IsACAccount acAccount => acAccount -> IO (Id NSString)
userFullName acAccount =
  sendMessage acAccount userFullNameSelector

-- | @- credential@
credential :: IsACAccount acAccount => acAccount -> IO (Id ACAccountCredential)
credential acAccount =
  sendMessage acAccount credentialSelector

-- | @- setCredential:@
setCredential :: (IsACAccount acAccount, IsACAccountCredential value) => acAccount -> value -> IO ()
setCredential acAccount value =
  sendMessage acAccount setCredentialSelector (toACAccountCredential value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithAccountType:@
initWithAccountTypeSelector :: Selector '[Id ACAccountType] (Id ACAccount)
initWithAccountTypeSelector = mkSelector "initWithAccountType:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @accountType@
accountTypeSelector :: Selector '[] (Id ACAccountType)
accountTypeSelector = mkSelector "accountType"

-- | @Selector@ for @setAccountType:@
setAccountTypeSelector :: Selector '[Id ACAccountType] ()
setAccountTypeSelector = mkSelector "setAccountType:"

-- | @Selector@ for @accountDescription@
accountDescriptionSelector :: Selector '[] (Id NSString)
accountDescriptionSelector = mkSelector "accountDescription"

-- | @Selector@ for @setAccountDescription:@
setAccountDescriptionSelector :: Selector '[Id NSString] ()
setAccountDescriptionSelector = mkSelector "setAccountDescription:"

-- | @Selector@ for @username@
usernameSelector :: Selector '[] (Id NSString)
usernameSelector = mkSelector "username"

-- | @Selector@ for @setUsername:@
setUsernameSelector :: Selector '[Id NSString] ()
setUsernameSelector = mkSelector "setUsername:"

-- | @Selector@ for @userFullName@
userFullNameSelector :: Selector '[] (Id NSString)
userFullNameSelector = mkSelector "userFullName"

-- | @Selector@ for @credential@
credentialSelector :: Selector '[] (Id ACAccountCredential)
credentialSelector = mkSelector "credential"

-- | @Selector@ for @setCredential:@
setCredentialSelector :: Selector '[Id ACAccountCredential] ()
setCredentialSelector = mkSelector "setCredential:"

