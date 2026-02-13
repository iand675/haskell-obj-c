{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object encapsulating additional information about the message being composed.
--
-- Generated bindings for @MEComposeContext@.
module ObjC.MailKit.MEComposeContext
  ( MEComposeContext
  , IsMEComposeContext(..)
  , contextID
  , originalMessage
  , action
  , isEncrypted
  , shouldEncrypt
  , isSigned
  , shouldSign
  , actionSelector
  , contextIDSelector
  , isEncryptedSelector
  , isSignedSelector
  , originalMessageSelector
  , shouldEncryptSelector
  , shouldSignSelector

  -- * Enum types
  , MEComposeUserAction(MEComposeUserAction)
  , pattern MEComposeUserActionNewMessage
  , pattern MEComposeUserActionReply
  , pattern MEComposeUserActionReplyAll
  , pattern MEComposeUserActionForward

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MailKit.Internal.Classes
import ObjC.MailKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | A unique identifier for the compose context.
--
-- ObjC selector: @- contextID@
contextID :: IsMEComposeContext meComposeContext => meComposeContext -> IO (Id NSUUID)
contextID meComposeContext =
  sendMessage meComposeContext contextIDSelector

-- | The original email message on which user performed an action It is @nil@ for @MEComposeUserActionNewMessage@ actions.
--
-- ObjC selector: @- originalMessage@
originalMessage :: IsMEComposeContext meComposeContext => meComposeContext -> IO (Id MEMessage)
originalMessage meComposeContext =
  sendMessage meComposeContext originalMessageSelector

-- | Indicates the action performed by the user that created this compose context.
--
-- ObjC selector: @- action@
action :: IsMEComposeContext meComposeContext => meComposeContext -> IO MEComposeUserAction
action meComposeContext =
  sendMessage meComposeContext actionSelector

-- | Boolean that indicates the message is encrypted by a Message Security extension.
--
-- ObjC selector: @- isEncrypted@
isEncrypted :: IsMEComposeContext meComposeContext => meComposeContext -> IO Bool
isEncrypted meComposeContext =
  sendMessage meComposeContext isEncryptedSelector

-- | Boolean that indicates if the user wants to encrypt the message.
--
-- ObjC selector: @- shouldEncrypt@
shouldEncrypt :: IsMEComposeContext meComposeContext => meComposeContext -> IO Bool
shouldEncrypt meComposeContext =
  sendMessage meComposeContext shouldEncryptSelector

-- | Boolean that indicates the message is signed by a Message Security extension.
--
-- ObjC selector: @- isSigned@
isSigned :: IsMEComposeContext meComposeContext => meComposeContext -> IO Bool
isSigned meComposeContext =
  sendMessage meComposeContext isSignedSelector

-- | A Boolean that indicates if the user wants to sign the message.
--
-- ObjC selector: @- shouldSign@
shouldSign :: IsMEComposeContext meComposeContext => meComposeContext -> IO Bool
shouldSign meComposeContext =
  sendMessage meComposeContext shouldSignSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @contextID@
contextIDSelector :: Selector '[] (Id NSUUID)
contextIDSelector = mkSelector "contextID"

-- | @Selector@ for @originalMessage@
originalMessageSelector :: Selector '[] (Id MEMessage)
originalMessageSelector = mkSelector "originalMessage"

-- | @Selector@ for @action@
actionSelector :: Selector '[] MEComposeUserAction
actionSelector = mkSelector "action"

-- | @Selector@ for @isEncrypted@
isEncryptedSelector :: Selector '[] Bool
isEncryptedSelector = mkSelector "isEncrypted"

-- | @Selector@ for @shouldEncrypt@
shouldEncryptSelector :: Selector '[] Bool
shouldEncryptSelector = mkSelector "shouldEncrypt"

-- | @Selector@ for @isSigned@
isSignedSelector :: Selector '[] Bool
isSignedSelector = mkSelector "isSigned"

-- | @Selector@ for @shouldSign@
shouldSignSelector :: Selector '[] Bool
shouldSignSelector = mkSelector "shouldSign"

