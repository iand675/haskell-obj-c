{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAuthorizationPublicKeyCredentialPRFAssertionInputValues@.
module ObjC.AuthenticationServices.ASAuthorizationPublicKeyCredentialPRFAssertionInputValues
  ( ASAuthorizationPublicKeyCredentialPRFAssertionInputValues
  , IsASAuthorizationPublicKeyCredentialPRFAssertionInputValues(..)
  , initWithSaltInput1_saltInput2
  , saltInput1
  , saltInput2
  , initWithSaltInput1_saltInput2Selector
  , saltInput1Selector
  , saltInput2Selector


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

-- | @- initWithSaltInput1:saltInput2:@
initWithSaltInput1_saltInput2 :: (IsASAuthorizationPublicKeyCredentialPRFAssertionInputValues asAuthorizationPublicKeyCredentialPRFAssertionInputValues, IsNSData saltInput1, IsNSData saltInput2) => asAuthorizationPublicKeyCredentialPRFAssertionInputValues -> saltInput1 -> saltInput2 -> IO (Id ASAuthorizationPublicKeyCredentialPRFAssertionInputValues)
initWithSaltInput1_saltInput2 asAuthorizationPublicKeyCredentialPRFAssertionInputValues  saltInput1 saltInput2 =
withObjCPtr saltInput1 $ \raw_saltInput1 ->
  withObjCPtr saltInput2 $ \raw_saltInput2 ->
      sendMsg asAuthorizationPublicKeyCredentialPRFAssertionInputValues (mkSelector "initWithSaltInput1:saltInput2:") (retPtr retVoid) [argPtr (castPtr raw_saltInput1 :: Ptr ()), argPtr (castPtr raw_saltInput2 :: Ptr ())] >>= ownedObject . castPtr

-- | @- saltInput1@
saltInput1 :: IsASAuthorizationPublicKeyCredentialPRFAssertionInputValues asAuthorizationPublicKeyCredentialPRFAssertionInputValues => asAuthorizationPublicKeyCredentialPRFAssertionInputValues -> IO (Id NSData)
saltInput1 asAuthorizationPublicKeyCredentialPRFAssertionInputValues  =
  sendMsg asAuthorizationPublicKeyCredentialPRFAssertionInputValues (mkSelector "saltInput1") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- saltInput2@
saltInput2 :: IsASAuthorizationPublicKeyCredentialPRFAssertionInputValues asAuthorizationPublicKeyCredentialPRFAssertionInputValues => asAuthorizationPublicKeyCredentialPRFAssertionInputValues -> IO (Id NSData)
saltInput2 asAuthorizationPublicKeyCredentialPRFAssertionInputValues  =
  sendMsg asAuthorizationPublicKeyCredentialPRFAssertionInputValues (mkSelector "saltInput2") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSaltInput1:saltInput2:@
initWithSaltInput1_saltInput2Selector :: Selector
initWithSaltInput1_saltInput2Selector = mkSelector "initWithSaltInput1:saltInput2:"

-- | @Selector@ for @saltInput1@
saltInput1Selector :: Selector
saltInput1Selector = mkSelector "saltInput1"

-- | @Selector@ for @saltInput2@
saltInput2Selector :: Selector
saltInput2Selector = mkSelector "saltInput2"

