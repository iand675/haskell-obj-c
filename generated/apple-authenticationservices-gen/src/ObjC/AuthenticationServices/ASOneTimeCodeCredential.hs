{-# LANGUAGE DataKinds #-}
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
  , codeSelector
  , credentialWithCodeSelector
  , initSelector
  , initWithCodeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsASOneTimeCodeCredential asOneTimeCodeCredential => asOneTimeCodeCredential -> IO (Id ASOneTimeCodeCredential)
init_ asOneTimeCodeCredential =
  sendOwnedMessage asOneTimeCodeCredential initSelector

-- | Creates and initializes a new ASOneTimeCodeCredential object.
--
-- @code@ — the one time code.
--
-- ObjC selector: @+ credentialWithCode:@
credentialWithCode :: IsNSString code => code -> IO (Id ASOneTimeCodeCredential)
credentialWithCode code =
  do
    cls' <- getRequiredClass "ASOneTimeCodeCredential"
    sendClassMessage cls' credentialWithCodeSelector (toNSString code)

-- | Initializes an ASOneTimeCodeCredential object.
--
-- @code@ — the one time code.
--
-- ObjC selector: @- initWithCode:@
initWithCode :: (IsASOneTimeCodeCredential asOneTimeCodeCredential, IsNSString code) => asOneTimeCodeCredential -> code -> IO (Id ASOneTimeCodeCredential)
initWithCode asOneTimeCodeCredential code =
  sendOwnedMessage asOneTimeCodeCredential initWithCodeSelector (toNSString code)

-- | The code of this credential.
--
-- Returns: The code string.
--
-- ObjC selector: @- code@
code :: IsASOneTimeCodeCredential asOneTimeCodeCredential => asOneTimeCodeCredential -> IO (Id NSString)
code asOneTimeCodeCredential =
  sendMessage asOneTimeCodeCredential codeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id ASOneTimeCodeCredential)
initSelector = mkSelector "init"

-- | @Selector@ for @credentialWithCode:@
credentialWithCodeSelector :: Selector '[Id NSString] (Id ASOneTimeCodeCredential)
credentialWithCodeSelector = mkSelector "credentialWithCode:"

-- | @Selector@ for @initWithCode:@
initWithCodeSelector :: Selector '[Id NSString] (Id ASOneTimeCodeCredential)
initWithCodeSelector = mkSelector "initWithCode:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] (Id NSString)
codeSelector = mkSelector "code"

