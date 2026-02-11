{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INEditMessageIntent@.
module ObjC.Intents.INEditMessageIntent
  ( INEditMessageIntent
  , IsINEditMessageIntent(..)
  , initWithMessageIdentifier_editedContent
  , messageIdentifier
  , editedContent
  , initWithMessageIdentifier_editedContentSelector
  , messageIdentifierSelector
  , editedContentSelector


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

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithMessageIdentifier:editedContent:@
initWithMessageIdentifier_editedContent :: (IsINEditMessageIntent inEditMessageIntent, IsNSString messageIdentifier, IsNSString editedContent) => inEditMessageIntent -> messageIdentifier -> editedContent -> IO (Id INEditMessageIntent)
initWithMessageIdentifier_editedContent inEditMessageIntent  messageIdentifier editedContent =
withObjCPtr messageIdentifier $ \raw_messageIdentifier ->
  withObjCPtr editedContent $ \raw_editedContent ->
      sendMsg inEditMessageIntent (mkSelector "initWithMessageIdentifier:editedContent:") (retPtr retVoid) [argPtr (castPtr raw_messageIdentifier :: Ptr ()), argPtr (castPtr raw_editedContent :: Ptr ())] >>= ownedObject . castPtr

-- | @- messageIdentifier@
messageIdentifier :: IsINEditMessageIntent inEditMessageIntent => inEditMessageIntent -> IO (Id NSString)
messageIdentifier inEditMessageIntent  =
  sendMsg inEditMessageIntent (mkSelector "messageIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- editedContent@
editedContent :: IsINEditMessageIntent inEditMessageIntent => inEditMessageIntent -> IO (Id NSString)
editedContent inEditMessageIntent  =
  sendMsg inEditMessageIntent (mkSelector "editedContent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithMessageIdentifier:editedContent:@
initWithMessageIdentifier_editedContentSelector :: Selector
initWithMessageIdentifier_editedContentSelector = mkSelector "initWithMessageIdentifier:editedContent:"

-- | @Selector@ for @messageIdentifier@
messageIdentifierSelector :: Selector
messageIdentifierSelector = mkSelector "messageIdentifier"

-- | @Selector@ for @editedContent@
editedContentSelector :: Selector
editedContentSelector = mkSelector "editedContent"

