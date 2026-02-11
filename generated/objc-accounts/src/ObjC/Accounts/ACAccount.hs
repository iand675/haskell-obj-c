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
  , initWithAccountTypeSelector
  , identifierSelector
  , accountTypeSelector
  , setAccountTypeSelector
  , accountDescriptionSelector
  , setAccountDescriptionSelector
  , usernameSelector
  , setUsernameSelector
  , userFullNameSelector
  , credentialSelector
  , setCredentialSelector


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

-- | @- initWithAccountType:@
initWithAccountType :: (IsACAccount acAccount, IsACAccountType type_) => acAccount -> type_ -> IO (Id ACAccount)
initWithAccountType acAccount  type_ =
withObjCPtr type_ $ \raw_type_ ->
    sendMsg acAccount (mkSelector "initWithAccountType:") (retPtr retVoid) [argPtr (castPtr raw_type_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- identifier@
identifier :: IsACAccount acAccount => acAccount -> IO (Id NSString)
identifier acAccount  =
  sendMsg acAccount (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- accountType@
accountType :: IsACAccount acAccount => acAccount -> IO (Id ACAccountType)
accountType acAccount  =
  sendMsg acAccount (mkSelector "accountType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAccountType:@
setAccountType :: (IsACAccount acAccount, IsACAccountType value) => acAccount -> value -> IO ()
setAccountType acAccount  value =
withObjCPtr value $ \raw_value ->
    sendMsg acAccount (mkSelector "setAccountType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- accountDescription@
accountDescription :: IsACAccount acAccount => acAccount -> IO (Id NSString)
accountDescription acAccount  =
  sendMsg acAccount (mkSelector "accountDescription") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAccountDescription:@
setAccountDescription :: (IsACAccount acAccount, IsNSString value) => acAccount -> value -> IO ()
setAccountDescription acAccount  value =
withObjCPtr value $ \raw_value ->
    sendMsg acAccount (mkSelector "setAccountDescription:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- username@
username :: IsACAccount acAccount => acAccount -> IO (Id NSString)
username acAccount  =
  sendMsg acAccount (mkSelector "username") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUsername:@
setUsername :: (IsACAccount acAccount, IsNSString value) => acAccount -> value -> IO ()
setUsername acAccount  value =
withObjCPtr value $ \raw_value ->
    sendMsg acAccount (mkSelector "setUsername:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- userFullName@
userFullName :: IsACAccount acAccount => acAccount -> IO (Id NSString)
userFullName acAccount  =
  sendMsg acAccount (mkSelector "userFullName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- credential@
credential :: IsACAccount acAccount => acAccount -> IO (Id ACAccountCredential)
credential acAccount  =
  sendMsg acAccount (mkSelector "credential") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCredential:@
setCredential :: (IsACAccount acAccount, IsACAccountCredential value) => acAccount -> value -> IO ()
setCredential acAccount  value =
withObjCPtr value $ \raw_value ->
    sendMsg acAccount (mkSelector "setCredential:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithAccountType:@
initWithAccountTypeSelector :: Selector
initWithAccountTypeSelector = mkSelector "initWithAccountType:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @accountType@
accountTypeSelector :: Selector
accountTypeSelector = mkSelector "accountType"

-- | @Selector@ for @setAccountType:@
setAccountTypeSelector :: Selector
setAccountTypeSelector = mkSelector "setAccountType:"

-- | @Selector@ for @accountDescription@
accountDescriptionSelector :: Selector
accountDescriptionSelector = mkSelector "accountDescription"

-- | @Selector@ for @setAccountDescription:@
setAccountDescriptionSelector :: Selector
setAccountDescriptionSelector = mkSelector "setAccountDescription:"

-- | @Selector@ for @username@
usernameSelector :: Selector
usernameSelector = mkSelector "username"

-- | @Selector@ for @setUsername:@
setUsernameSelector :: Selector
setUsernameSelector = mkSelector "setUsername:"

-- | @Selector@ for @userFullName@
userFullNameSelector :: Selector
userFullNameSelector = mkSelector "userFullName"

-- | @Selector@ for @credential@
credentialSelector :: Selector
credentialSelector = mkSelector "credential"

-- | @Selector@ for @setCredential:@
setCredentialSelector :: Selector
setCredentialSelector = mkSelector "setCredential:"

