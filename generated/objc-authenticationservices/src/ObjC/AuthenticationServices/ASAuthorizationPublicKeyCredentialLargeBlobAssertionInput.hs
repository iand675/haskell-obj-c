{-# LANGUAGE PatternSynonyms #-}
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
  , newSelector
  , initSelector
  , initWithOperationSelector
  , operationSelector
  , dataToWriteSelector
  , setDataToWriteSelector

  -- * Enum types
  , ASAuthorizationPublicKeyCredentialLargeBlobAssertionOperation(ASAuthorizationPublicKeyCredentialLargeBlobAssertionOperation)
  , pattern ASAuthorizationPublicKeyCredentialLargeBlobAssertionOperationRead
  , pattern ASAuthorizationPublicKeyCredentialLargeBlobAssertionOperationWrite

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

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.AuthenticationServices.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id ASAuthorizationPublicKeyCredentialLargeBlobAssertionInput)
new  =
  do
    cls' <- getRequiredClass "ASAuthorizationPublicKeyCredentialLargeBlobAssertionInput"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsASAuthorizationPublicKeyCredentialLargeBlobAssertionInput asAuthorizationPublicKeyCredentialLargeBlobAssertionInput => asAuthorizationPublicKeyCredentialLargeBlobAssertionInput -> IO (Id ASAuthorizationPublicKeyCredentialLargeBlobAssertionInput)
init_ asAuthorizationPublicKeyCredentialLargeBlobAssertionInput  =
  sendMsg asAuthorizationPublicKeyCredentialLargeBlobAssertionInput (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithOperation:@
initWithOperation :: IsASAuthorizationPublicKeyCredentialLargeBlobAssertionInput asAuthorizationPublicKeyCredentialLargeBlobAssertionInput => asAuthorizationPublicKeyCredentialLargeBlobAssertionInput -> ASAuthorizationPublicKeyCredentialLargeBlobAssertionOperation -> IO (Id ASAuthorizationPublicKeyCredentialLargeBlobAssertionInput)
initWithOperation asAuthorizationPublicKeyCredentialLargeBlobAssertionInput  operation =
  sendMsg asAuthorizationPublicKeyCredentialLargeBlobAssertionInput (mkSelector "initWithOperation:") (retPtr retVoid) [argCLong (coerce operation)] >>= ownedObject . castPtr

-- | @- operation@
operation :: IsASAuthorizationPublicKeyCredentialLargeBlobAssertionInput asAuthorizationPublicKeyCredentialLargeBlobAssertionInput => asAuthorizationPublicKeyCredentialLargeBlobAssertionInput -> IO ASAuthorizationPublicKeyCredentialLargeBlobAssertionOperation
operation asAuthorizationPublicKeyCredentialLargeBlobAssertionInput  =
  fmap (coerce :: CLong -> ASAuthorizationPublicKeyCredentialLargeBlobAssertionOperation) $ sendMsg asAuthorizationPublicKeyCredentialLargeBlobAssertionInput (mkSelector "operation") retCLong []

-- | @- dataToWrite@
dataToWrite :: IsASAuthorizationPublicKeyCredentialLargeBlobAssertionInput asAuthorizationPublicKeyCredentialLargeBlobAssertionInput => asAuthorizationPublicKeyCredentialLargeBlobAssertionInput -> IO (Id NSData)
dataToWrite asAuthorizationPublicKeyCredentialLargeBlobAssertionInput  =
  sendMsg asAuthorizationPublicKeyCredentialLargeBlobAssertionInput (mkSelector "dataToWrite") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDataToWrite:@
setDataToWrite :: (IsASAuthorizationPublicKeyCredentialLargeBlobAssertionInput asAuthorizationPublicKeyCredentialLargeBlobAssertionInput, IsNSData value) => asAuthorizationPublicKeyCredentialLargeBlobAssertionInput -> value -> IO ()
setDataToWrite asAuthorizationPublicKeyCredentialLargeBlobAssertionInput  value =
withObjCPtr value $ \raw_value ->
    sendMsg asAuthorizationPublicKeyCredentialLargeBlobAssertionInput (mkSelector "setDataToWrite:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithOperation:@
initWithOperationSelector :: Selector
initWithOperationSelector = mkSelector "initWithOperation:"

-- | @Selector@ for @operation@
operationSelector :: Selector
operationSelector = mkSelector "operation"

-- | @Selector@ for @dataToWrite@
dataToWriteSelector :: Selector
dataToWriteSelector = mkSelector "dataToWrite"

-- | @Selector@ for @setDataToWrite:@
setDataToWriteSelector :: Selector
setDataToWriteSelector = mkSelector "setDataToWrite:"

