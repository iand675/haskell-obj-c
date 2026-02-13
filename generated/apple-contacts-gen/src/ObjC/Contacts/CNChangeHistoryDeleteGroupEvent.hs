{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A group was deleted
--
-- Generated bindings for @CNChangeHistoryDeleteGroupEvent@.
module ObjC.Contacts.CNChangeHistoryDeleteGroupEvent
  ( CNChangeHistoryDeleteGroupEvent
  , IsCNChangeHistoryDeleteGroupEvent(..)
  , groupIdentifier
  , groupIdentifierSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Contacts.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- groupIdentifier@
groupIdentifier :: IsCNChangeHistoryDeleteGroupEvent cnChangeHistoryDeleteGroupEvent => cnChangeHistoryDeleteGroupEvent -> IO (Id NSString)
groupIdentifier cnChangeHistoryDeleteGroupEvent =
  sendMessage cnChangeHistoryDeleteGroupEvent groupIdentifierSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @groupIdentifier@
groupIdentifierSelector :: Selector '[] (Id NSString)
groupIdentifierSelector = mkSelector "groupIdentifier"

