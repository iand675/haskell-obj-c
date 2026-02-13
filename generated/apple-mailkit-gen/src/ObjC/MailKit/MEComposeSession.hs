{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An instance of this class is associated with the lifecycle of a single mail compose window. This object associates the actions performed by the user in a mail compose window to a unique session. An instance of this class is passed to the methods in @MEComposeSessionHandler.@
--
-- Generated bindings for @MEComposeSession@.
module ObjC.MailKit.MEComposeSession
  ( MEComposeSession
  , IsMEComposeSession(..)
  , new
  , init_
  , reloadSession
  , sessionID
  , mailMessage
  , composeContext
  , composeContextSelector
  , initSelector
  , mailMessageSelector
  , newSelector
  , reloadSessionSelector
  , sessionIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MailKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id MEComposeSession)
new  =
  do
    cls' <- getRequiredClass "MEComposeSession"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsMEComposeSession meComposeSession => meComposeSession -> IO (Id MEComposeSession)
init_ meComposeSession =
  sendOwnedMessage meComposeSession initSelector

-- | Requests Mail to refresh compose session with new information that the extension has.
--
-- Extensions can use this call this method to regenerate @MEAddressAnnotation@ instances to replace those that were previously generated for this session. This will result in invocations to @-[MEComposeSessionHandler@ @session:annotateAddressesWithCompletionHandler:].@
--
-- ObjC selector: @- reloadSession@
reloadSession :: IsMEComposeSession meComposeSession => meComposeSession -> IO ()
reloadSession meComposeSession =
  sendMessage meComposeSession reloadSessionSelector

-- | A unique identifier for the session.
--
-- ObjC selector: @- sessionID@
sessionID :: IsMEComposeSession meComposeSession => meComposeSession -> IO (Id NSUUID)
sessionID meComposeSession =
  sendMessage meComposeSession sessionIDSelector

-- | An instance of @MEMessage@ that represents properties of the mail message that author is composing in this @MEComposeSession@
--
-- ObjC selector: @- mailMessage@
mailMessage :: IsMEComposeSession meComposeSession => meComposeSession -> IO (Id MEMessage)
mailMessage meComposeSession =
  sendMessage meComposeSession mailMessageSelector

-- | An instance of @MEComposeContext@ that provides additional information about the compose session.
--
-- ObjC selector: @- composeContext@
composeContext :: IsMEComposeSession meComposeSession => meComposeSession -> IO (Id MEComposeContext)
composeContext meComposeSession =
  sendMessage meComposeSession composeContextSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MEComposeSession)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MEComposeSession)
initSelector = mkSelector "init"

-- | @Selector@ for @reloadSession@
reloadSessionSelector :: Selector '[] ()
reloadSessionSelector = mkSelector "reloadSession"

-- | @Selector@ for @sessionID@
sessionIDSelector :: Selector '[] (Id NSUUID)
sessionIDSelector = mkSelector "sessionID"

-- | @Selector@ for @mailMessage@
mailMessageSelector :: Selector '[] (Id MEMessage)
mailMessageSelector = mkSelector "mailMessage"

-- | @Selector@ for @composeContext@
composeContextSelector :: Selector '[] (Id MEComposeContext)
composeContextSelector = mkSelector "composeContext"

