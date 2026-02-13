{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ILMessageCommunication@.
module ObjC.IdentityLookup.ILMessageCommunication
  ( ILMessageCommunication
  , IsILMessageCommunication(..)
  , isEqualToMessageCommunication
  , init_
  , messageBody
  , initSelector
  , isEqualToMessageCommunicationSelector
  , messageBodySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.IdentityLookup.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- isEqualToMessageCommunication:@
isEqualToMessageCommunication :: (IsILMessageCommunication ilMessageCommunication, IsILMessageCommunication communication) => ilMessageCommunication -> communication -> IO Bool
isEqualToMessageCommunication ilMessageCommunication communication =
  sendMessage ilMessageCommunication isEqualToMessageCommunicationSelector (toILMessageCommunication communication)

-- | @- init@
init_ :: IsILMessageCommunication ilMessageCommunication => ilMessageCommunication -> IO (Id ILMessageCommunication)
init_ ilMessageCommunication =
  sendOwnedMessage ilMessageCommunication initSelector

-- | @- messageBody@
messageBody :: IsILMessageCommunication ilMessageCommunication => ilMessageCommunication -> IO (Id NSString)
messageBody ilMessageCommunication =
  sendMessage ilMessageCommunication messageBodySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isEqualToMessageCommunication:@
isEqualToMessageCommunicationSelector :: Selector '[Id ILMessageCommunication] Bool
isEqualToMessageCommunicationSelector = mkSelector "isEqualToMessageCommunication:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id ILMessageCommunication)
initSelector = mkSelector "init"

-- | @Selector@ for @messageBody@
messageBodySelector :: Selector '[] (Id NSString)
messageBodySelector = mkSelector "messageBody"

