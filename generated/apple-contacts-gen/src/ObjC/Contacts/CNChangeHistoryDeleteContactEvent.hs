{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A contact was removed
--
-- Generated bindings for @CNChangeHistoryDeleteContactEvent@.
module ObjC.Contacts.CNChangeHistoryDeleteContactEvent
  ( CNChangeHistoryDeleteContactEvent
  , IsCNChangeHistoryDeleteContactEvent(..)
  , contactIdentifier
  , contactIdentifierSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Contacts.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- contactIdentifier@
contactIdentifier :: IsCNChangeHistoryDeleteContactEvent cnChangeHistoryDeleteContactEvent => cnChangeHistoryDeleteContactEvent -> IO (Id NSString)
contactIdentifier cnChangeHistoryDeleteContactEvent =
  sendMessage cnChangeHistoryDeleteContactEvent contactIdentifierSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @contactIdentifier@
contactIdentifierSelector :: Selector '[] (Id NSString)
contactIdentifierSelector = mkSelector "contactIdentifier"

