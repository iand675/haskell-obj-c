{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAuthorizationPublicKeyCredentialLargeBlobAssertionInput@.
module ObjC.AuthenticationServices.ASAuthorizationPublicKeyCredentialLargeBlobAssertionInput
  ( ASAuthorizationPublicKeyCredentialLargeBlobAssertionInput
  , IsASAuthorizationPublicKeyCredentialLargeBlobAssertionInput(..)
  , new
  , init_
  , initWithOperation
  , operation
  , dataToWrite
  , setDataToWrite
  , dataToWriteSelector
  , initSelector
  , initWithOperationSelector
  , newSelector
  , operationSelector
  , setDataToWriteSelector

  -- * Enum types
  , ASAuthorizationPublicKeyCredentialLargeBlobAssertionOperation(ASAuthorizationPublicKeyCredentialLargeBlobAssertionOperation)
  , pattern ASAuthorizationPublicKeyCredentialLargeBlobAssertionOperationRead
  , pattern ASAuthorizationPublicKeyCredentialLargeBlobAssertionOperationWrite

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.AuthenticationServices.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id ASAuthorizationPublicKeyCredentialLargeBlobAssertionInput)
new  =
  do
    cls' <- getRequiredClass "ASAuthorizationPublicKeyCredentialLargeBlobAssertionInput"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsASAuthorizationPublicKeyCredentialLargeBlobAssertionInput asAuthorizationPublicKeyCredentialLargeBlobAssertionInput => asAuthorizationPublicKeyCredentialLargeBlobAssertionInput -> IO (Id ASAuthorizationPublicKeyCredentialLargeBlobAssertionInput)
init_ asAuthorizationPublicKeyCredentialLargeBlobAssertionInput =
  sendOwnedMessage asAuthorizationPublicKeyCredentialLargeBlobAssertionInput initSelector

-- | @- initWithOperation:@
initWithOperation :: IsASAuthorizationPublicKeyCredentialLargeBlobAssertionInput asAuthorizationPublicKeyCredentialLargeBlobAssertionInput => asAuthorizationPublicKeyCredentialLargeBlobAssertionInput -> ASAuthorizationPublicKeyCredentialLargeBlobAssertionOperation -> IO (Id ASAuthorizationPublicKeyCredentialLargeBlobAssertionInput)
initWithOperation asAuthorizationPublicKeyCredentialLargeBlobAssertionInput operation =
  sendOwnedMessage asAuthorizationPublicKeyCredentialLargeBlobAssertionInput initWithOperationSelector operation

-- | @- operation@
operation :: IsASAuthorizationPublicKeyCredentialLargeBlobAssertionInput asAuthorizationPublicKeyCredentialLargeBlobAssertionInput => asAuthorizationPublicKeyCredentialLargeBlobAssertionInput -> IO ASAuthorizationPublicKeyCredentialLargeBlobAssertionOperation
operation asAuthorizationPublicKeyCredentialLargeBlobAssertionInput =
  sendMessage asAuthorizationPublicKeyCredentialLargeBlobAssertionInput operationSelector

-- | @- dataToWrite@
dataToWrite :: IsASAuthorizationPublicKeyCredentialLargeBlobAssertionInput asAuthorizationPublicKeyCredentialLargeBlobAssertionInput => asAuthorizationPublicKeyCredentialLargeBlobAssertionInput -> IO (Id NSData)
dataToWrite asAuthorizationPublicKeyCredentialLargeBlobAssertionInput =
  sendMessage asAuthorizationPublicKeyCredentialLargeBlobAssertionInput dataToWriteSelector

-- | @- setDataToWrite:@
setDataToWrite :: (IsASAuthorizationPublicKeyCredentialLargeBlobAssertionInput asAuthorizationPublicKeyCredentialLargeBlobAssertionInput, IsNSData value) => asAuthorizationPublicKeyCredentialLargeBlobAssertionInput -> value -> IO ()
setDataToWrite asAuthorizationPublicKeyCredentialLargeBlobAssertionInput value =
  sendMessage asAuthorizationPublicKeyCredentialLargeBlobAssertionInput setDataToWriteSelector (toNSData value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id ASAuthorizationPublicKeyCredentialLargeBlobAssertionInput)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id ASAuthorizationPublicKeyCredentialLargeBlobAssertionInput)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithOperation:@
initWithOperationSelector :: Selector '[ASAuthorizationPublicKeyCredentialLargeBlobAssertionOperation] (Id ASAuthorizationPublicKeyCredentialLargeBlobAssertionInput)
initWithOperationSelector = mkSelector "initWithOperation:"

-- | @Selector@ for @operation@
operationSelector :: Selector '[] ASAuthorizationPublicKeyCredentialLargeBlobAssertionOperation
operationSelector = mkSelector "operation"

-- | @Selector@ for @dataToWrite@
dataToWriteSelector :: Selector '[] (Id NSData)
dataToWriteSelector = mkSelector "dataToWrite"

-- | @Selector@ for @setDataToWrite:@
setDataToWriteSelector :: Selector '[Id NSData] ()
setDataToWriteSelector = mkSelector "setDataToWrite:"

