{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A helper class to provide static utility methods for quick access to settings related to credential providers.
--
-- Generated bindings for @ASSettingsHelper@.
module ObjC.AuthenticationServices.ASSettingsHelper
  ( ASSettingsHelper
  , IsASSettingsHelper(..)
  , openCredentialProviderAppSettingsWithCompletionHandler
  , openVerificationCodeAppSettingsWithCompletionHandler
  , requestToTurnOnCredentialProviderExtensionWithCompletionHandler
  , init_
  , new
  , openCredentialProviderAppSettingsWithCompletionHandlerSelector
  , openVerificationCodeAppSettingsWithCompletionHandlerSelector
  , requestToTurnOnCredentialProviderExtensionWithCompletionHandlerSelector
  , initSelector
  , newSelector


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

-- | Calling this method will open the Settings app and navigate directly to the AutoFill provider settings.
--
-- ObjC selector: @+ openCredentialProviderAppSettingsWithCompletionHandler:@
openCredentialProviderAppSettingsWithCompletionHandler :: Ptr () -> IO ()
openCredentialProviderAppSettingsWithCompletionHandler completionHandler =
  do
    cls' <- getRequiredClass "ASSettingsHelper"
    sendClassMsg cls' (mkSelector "openCredentialProviderAppSettingsWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | Calling this method will open the Settings app and navigate directly to the Verification Code provider settings.
--
-- ObjC selector: @+ openVerificationCodeAppSettingsWithCompletionHandler:@
openVerificationCodeAppSettingsWithCompletionHandler :: Ptr () -> IO ()
openVerificationCodeAppSettingsWithCompletionHandler completionHandler =
  do
    cls' <- getRequiredClass "ASSettingsHelper"
    sendClassMsg cls' (mkSelector "openVerificationCodeAppSettingsWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | Call this method from your containing app to request to turn on a contained Credential Provider Extension. If the extension is not currently enabled, a prompt will be shown to allow it to be turned on. The completion handler is called with YES or NO depending on whether the credential provider is enabled. You need to wait 10 seconds in order to make additional request to this API.
--
-- ObjC selector: @+ requestToTurnOnCredentialProviderExtensionWithCompletionHandler:@
requestToTurnOnCredentialProviderExtensionWithCompletionHandler :: Ptr () -> IO ()
requestToTurnOnCredentialProviderExtensionWithCompletionHandler completionHandler =
  do
    cls' <- getRequiredClass "ASSettingsHelper"
    sendClassMsg cls' (mkSelector "requestToTurnOnCredentialProviderExtensionWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- init@
init_ :: IsASSettingsHelper asSettingsHelper => asSettingsHelper -> IO (Id ASSettingsHelper)
init_ asSettingsHelper  =
  sendMsg asSettingsHelper (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id ASSettingsHelper)
new  =
  do
    cls' <- getRequiredClass "ASSettingsHelper"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @openCredentialProviderAppSettingsWithCompletionHandler:@
openCredentialProviderAppSettingsWithCompletionHandlerSelector :: Selector
openCredentialProviderAppSettingsWithCompletionHandlerSelector = mkSelector "openCredentialProviderAppSettingsWithCompletionHandler:"

-- | @Selector@ for @openVerificationCodeAppSettingsWithCompletionHandler:@
openVerificationCodeAppSettingsWithCompletionHandlerSelector :: Selector
openVerificationCodeAppSettingsWithCompletionHandlerSelector = mkSelector "openVerificationCodeAppSettingsWithCompletionHandler:"

-- | @Selector@ for @requestToTurnOnCredentialProviderExtensionWithCompletionHandler:@
requestToTurnOnCredentialProviderExtensionWithCompletionHandlerSelector :: Selector
requestToTurnOnCredentialProviderExtensionWithCompletionHandlerSelector = mkSelector "requestToTurnOnCredentialProviderExtensionWithCompletionHandler:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

