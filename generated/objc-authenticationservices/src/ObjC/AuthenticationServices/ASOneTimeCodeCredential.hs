{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASOneTimeCodeCredential@.
module ObjC.AuthenticationServices.ASOneTimeCodeCredential
  ( ASOneTimeCodeCredential
  , IsASOneTimeCodeCredential(..)
  , init_
  , credentialWithCode
  , initWithCode
  , code
  , initSelector
  , credentialWithCodeSelector
  , initWithCodeSelector
  , codeSelector


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

-- | @- init@
init_ :: IsASOneTimeCodeCredential asOneTimeCodeCredential => asOneTimeCodeCredential -> IO (Id ASOneTimeCodeCredential)
init_ asOneTimeCodeCredential  =
  sendMsg asOneTimeCodeCredential (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Creates and initializes a new ASOneTimeCodeCredential object.
--
-- @code@ — the one time code.
--
-- ObjC selector: @+ credentialWithCode:@
credentialWithCode :: IsNSString code => code -> IO (Id ASOneTimeCodeCredential)
credentialWithCode code =
  do
    cls' <- getRequiredClass "ASOneTimeCodeCredential"
    withObjCPtr code $ \raw_code ->
      sendClassMsg cls' (mkSelector "credentialWithCode:") (retPtr retVoid) [argPtr (castPtr raw_code :: Ptr ())] >>= retainedObject . castPtr

-- | Initializes an ASOneTimeCodeCredential object.
--
-- @code@ — the one time code.
--
-- ObjC selector: @- initWithCode:@
initWithCode :: (IsASOneTimeCodeCredential asOneTimeCodeCredential, IsNSString code) => asOneTimeCodeCredential -> code -> IO (Id ASOneTimeCodeCredential)
initWithCode asOneTimeCodeCredential  code =
withObjCPtr code $ \raw_code ->
    sendMsg asOneTimeCodeCredential (mkSelector "initWithCode:") (retPtr retVoid) [argPtr (castPtr raw_code :: Ptr ())] >>= ownedObject . castPtr

-- | The code of this credential.
--
-- Returns: The code string.
--
-- ObjC selector: @- code@
code :: IsASOneTimeCodeCredential asOneTimeCodeCredential => asOneTimeCodeCredential -> IO (Id NSString)
code asOneTimeCodeCredential  =
  sendMsg asOneTimeCodeCredential (mkSelector "code") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @credentialWithCode:@
credentialWithCodeSelector :: Selector
credentialWithCodeSelector = mkSelector "credentialWithCode:"

-- | @Selector@ for @initWithCode:@
initWithCodeSelector :: Selector
initWithCodeSelector = mkSelector "initWithCode:"

-- | @Selector@ for @code@
codeSelector :: Selector
codeSelector = mkSelector "code"

