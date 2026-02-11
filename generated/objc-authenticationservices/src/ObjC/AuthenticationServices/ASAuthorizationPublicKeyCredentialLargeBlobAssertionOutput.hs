{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAuthorizationPublicKeyCredentialLargeBlobAssertionOutput@.
module ObjC.AuthenticationServices.ASAuthorizationPublicKeyCredentialLargeBlobAssertionOutput
  ( ASAuthorizationPublicKeyCredentialLargeBlobAssertionOutput
  , IsASAuthorizationPublicKeyCredentialLargeBlobAssertionOutput(..)
  , new
  , init_
  , readData
  , didWrite
  , newSelector
  , initSelector
  , readDataSelector
  , didWriteSelector


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
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id ASAuthorizationPublicKeyCredentialLargeBlobAssertionOutput)
new  =
  do
    cls' <- getRequiredClass "ASAuthorizationPublicKeyCredentialLargeBlobAssertionOutput"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsASAuthorizationPublicKeyCredentialLargeBlobAssertionOutput asAuthorizationPublicKeyCredentialLargeBlobAssertionOutput => asAuthorizationPublicKeyCredentialLargeBlobAssertionOutput -> IO (Id ASAuthorizationPublicKeyCredentialLargeBlobAssertionOutput)
init_ asAuthorizationPublicKeyCredentialLargeBlobAssertionOutput  =
  sendMsg asAuthorizationPublicKeyCredentialLargeBlobAssertionOutput (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- readData@
readData :: IsASAuthorizationPublicKeyCredentialLargeBlobAssertionOutput asAuthorizationPublicKeyCredentialLargeBlobAssertionOutput => asAuthorizationPublicKeyCredentialLargeBlobAssertionOutput -> IO (Id NSData)
readData asAuthorizationPublicKeyCredentialLargeBlobAssertionOutput  =
  sendMsg asAuthorizationPublicKeyCredentialLargeBlobAssertionOutput (mkSelector "readData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- didWrite@
didWrite :: IsASAuthorizationPublicKeyCredentialLargeBlobAssertionOutput asAuthorizationPublicKeyCredentialLargeBlobAssertionOutput => asAuthorizationPublicKeyCredentialLargeBlobAssertionOutput -> IO Bool
didWrite asAuthorizationPublicKeyCredentialLargeBlobAssertionOutput  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg asAuthorizationPublicKeyCredentialLargeBlobAssertionOutput (mkSelector "didWrite") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @readData@
readDataSelector :: Selector
readDataSelector = mkSelector "readData"

-- | @Selector@ for @didWrite@
didWriteSelector :: Selector
didWriteSelector = mkSelector "didWrite"

