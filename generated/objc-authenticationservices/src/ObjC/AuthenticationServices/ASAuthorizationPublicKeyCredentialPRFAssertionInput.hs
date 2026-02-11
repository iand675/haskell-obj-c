{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAuthorizationPublicKeyCredentialPRFAssertionInput@.
module ObjC.AuthenticationServices.ASAuthorizationPublicKeyCredentialPRFAssertionInput
  ( ASAuthorizationPublicKeyCredentialPRFAssertionInput
  , IsASAuthorizationPublicKeyCredentialPRFAssertionInput(..)
  , initWithInputValues_perCredentialInputValues
  , inputValues
  , perCredentialInputValues
  , initWithInputValues_perCredentialInputValuesSelector
  , inputValuesSelector
  , perCredentialInputValuesSelector


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

-- | @- initWithInputValues:perCredentialInputValues:@
initWithInputValues_perCredentialInputValues :: (IsASAuthorizationPublicKeyCredentialPRFAssertionInput asAuthorizationPublicKeyCredentialPRFAssertionInput, IsASAuthorizationPublicKeyCredentialPRFAssertionInputValues inputValues, IsNSDictionary perCredentialInputValues) => asAuthorizationPublicKeyCredentialPRFAssertionInput -> inputValues -> perCredentialInputValues -> IO (Id ASAuthorizationPublicKeyCredentialPRFAssertionInput)
initWithInputValues_perCredentialInputValues asAuthorizationPublicKeyCredentialPRFAssertionInput  inputValues perCredentialInputValues =
withObjCPtr inputValues $ \raw_inputValues ->
  withObjCPtr perCredentialInputValues $ \raw_perCredentialInputValues ->
      sendMsg asAuthorizationPublicKeyCredentialPRFAssertionInput (mkSelector "initWithInputValues:perCredentialInputValues:") (retPtr retVoid) [argPtr (castPtr raw_inputValues :: Ptr ()), argPtr (castPtr raw_perCredentialInputValues :: Ptr ())] >>= ownedObject . castPtr

-- | @- inputValues@
inputValues :: IsASAuthorizationPublicKeyCredentialPRFAssertionInput asAuthorizationPublicKeyCredentialPRFAssertionInput => asAuthorizationPublicKeyCredentialPRFAssertionInput -> IO (Id ASAuthorizationPublicKeyCredentialPRFAssertionInputValues)
inputValues asAuthorizationPublicKeyCredentialPRFAssertionInput  =
  sendMsg asAuthorizationPublicKeyCredentialPRFAssertionInput (mkSelector "inputValues") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- perCredentialInputValues@
perCredentialInputValues :: IsASAuthorizationPublicKeyCredentialPRFAssertionInput asAuthorizationPublicKeyCredentialPRFAssertionInput => asAuthorizationPublicKeyCredentialPRFAssertionInput -> IO (Id NSDictionary)
perCredentialInputValues asAuthorizationPublicKeyCredentialPRFAssertionInput  =
  sendMsg asAuthorizationPublicKeyCredentialPRFAssertionInput (mkSelector "perCredentialInputValues") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithInputValues:perCredentialInputValues:@
initWithInputValues_perCredentialInputValuesSelector :: Selector
initWithInputValues_perCredentialInputValuesSelector = mkSelector "initWithInputValues:perCredentialInputValues:"

-- | @Selector@ for @inputValues@
inputValuesSelector :: Selector
inputValuesSelector = mkSelector "inputValues"

-- | @Selector@ for @perCredentialInputValues@
perCredentialInputValuesSelector :: Selector
perCredentialInputValuesSelector = mkSelector "perCredentialInputValues"

