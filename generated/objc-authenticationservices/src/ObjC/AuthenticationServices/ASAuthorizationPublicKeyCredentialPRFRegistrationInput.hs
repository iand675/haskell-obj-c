{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAuthorizationPublicKeyCredentialPRFRegistrationInput@.
module ObjC.AuthenticationServices.ASAuthorizationPublicKeyCredentialPRFRegistrationInput
  ( ASAuthorizationPublicKeyCredentialPRFRegistrationInput
  , IsASAuthorizationPublicKeyCredentialPRFRegistrationInput(..)
  , checkForSupport
  , initWithInputValues
  , shouldCheckForSupport
  , inputValues
  , checkForSupportSelector
  , initWithInputValuesSelector
  , shouldCheckForSupportSelector
  , inputValuesSelector


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

-- | @+ checkForSupport@
checkForSupport :: IO (Id ASAuthorizationPublicKeyCredentialPRFRegistrationInput)
checkForSupport  =
  do
    cls' <- getRequiredClass "ASAuthorizationPublicKeyCredentialPRFRegistrationInput"
    sendClassMsg cls' (mkSelector "checkForSupport") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- initWithInputValues:@
initWithInputValues :: (IsASAuthorizationPublicKeyCredentialPRFRegistrationInput asAuthorizationPublicKeyCredentialPRFRegistrationInput, IsASAuthorizationPublicKeyCredentialPRFAssertionInputValues inputValues) => asAuthorizationPublicKeyCredentialPRFRegistrationInput -> inputValues -> IO (Id ASAuthorizationPublicKeyCredentialPRFRegistrationInput)
initWithInputValues asAuthorizationPublicKeyCredentialPRFRegistrationInput  inputValues =
withObjCPtr inputValues $ \raw_inputValues ->
    sendMsg asAuthorizationPublicKeyCredentialPRFRegistrationInput (mkSelector "initWithInputValues:") (retPtr retVoid) [argPtr (castPtr raw_inputValues :: Ptr ())] >>= ownedObject . castPtr

-- | @- shouldCheckForSupport@
shouldCheckForSupport :: IsASAuthorizationPublicKeyCredentialPRFRegistrationInput asAuthorizationPublicKeyCredentialPRFRegistrationInput => asAuthorizationPublicKeyCredentialPRFRegistrationInput -> IO Bool
shouldCheckForSupport asAuthorizationPublicKeyCredentialPRFRegistrationInput  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg asAuthorizationPublicKeyCredentialPRFRegistrationInput (mkSelector "shouldCheckForSupport") retCULong []

-- | @- inputValues@
inputValues :: IsASAuthorizationPublicKeyCredentialPRFRegistrationInput asAuthorizationPublicKeyCredentialPRFRegistrationInput => asAuthorizationPublicKeyCredentialPRFRegistrationInput -> IO (Id ASAuthorizationPublicKeyCredentialPRFAssertionInputValues)
inputValues asAuthorizationPublicKeyCredentialPRFRegistrationInput  =
  sendMsg asAuthorizationPublicKeyCredentialPRFRegistrationInput (mkSelector "inputValues") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @checkForSupport@
checkForSupportSelector :: Selector
checkForSupportSelector = mkSelector "checkForSupport"

-- | @Selector@ for @initWithInputValues:@
initWithInputValuesSelector :: Selector
initWithInputValuesSelector = mkSelector "initWithInputValues:"

-- | @Selector@ for @shouldCheckForSupport@
shouldCheckForSupportSelector :: Selector
shouldCheckForSupportSelector = mkSelector "shouldCheckForSupport"

-- | @Selector@ for @inputValues@
inputValuesSelector :: Selector
inputValuesSelector = mkSelector "inputValues"

