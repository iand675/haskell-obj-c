{-# LANGUAGE DataKinds #-}
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
  , editedContentSelector
  , initWithMessageIdentifier_editedContentSelector
  , messageIdentifierSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithMessageIdentifier:editedContent:@
initWithMessageIdentifier_editedContent :: (IsINEditMessageIntent inEditMessageIntent, IsNSString messageIdentifier, IsNSString editedContent) => inEditMessageIntent -> messageIdentifier -> editedContent -> IO (Id INEditMessageIntent)
initWithMessageIdentifier_editedContent inEditMessageIntent messageIdentifier editedContent =
  sendOwnedMessage inEditMessageIntent initWithMessageIdentifier_editedContentSelector (toNSString messageIdentifier) (toNSString editedContent)

-- | @- messageIdentifier@
messageIdentifier :: IsINEditMessageIntent inEditMessageIntent => inEditMessageIntent -> IO (Id NSString)
messageIdentifier inEditMessageIntent =
  sendMessage inEditMessageIntent messageIdentifierSelector

-- | @- editedContent@
editedContent :: IsINEditMessageIntent inEditMessageIntent => inEditMessageIntent -> IO (Id NSString)
editedContent inEditMessageIntent =
  sendMessage inEditMessageIntent editedContentSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithMessageIdentifier:editedContent:@
initWithMessageIdentifier_editedContentSelector :: Selector '[Id NSString, Id NSString] (Id INEditMessageIntent)
initWithMessageIdentifier_editedContentSelector = mkSelector "initWithMessageIdentifier:editedContent:"

-- | @Selector@ for @messageIdentifier@
messageIdentifierSelector :: Selector '[] (Id NSString)
messageIdentifierSelector = mkSelector "messageIdentifier"

-- | @Selector@ for @editedContent@
editedContentSelector :: Selector '[] (Id NSString)
editedContentSelector = mkSelector "editedContent"

