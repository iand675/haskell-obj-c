{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A contact was added
--
-- Generated bindings for @CNChangeHistoryAddContactEvent@.
module ObjC.Contacts.CNChangeHistoryAddContactEvent
  ( CNChangeHistoryAddContactEvent
  , IsCNChangeHistoryAddContactEvent(..)
  , contact
  , containerIdentifier
  , contactSelector
  , containerIdentifierSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Contacts.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- contact@
contact :: IsCNChangeHistoryAddContactEvent cnChangeHistoryAddContactEvent => cnChangeHistoryAddContactEvent -> IO (Id CNContact)
contact cnChangeHistoryAddContactEvent =
  sendMessage cnChangeHistoryAddContactEvent contactSelector

-- | @- containerIdentifier@
containerIdentifier :: IsCNChangeHistoryAddContactEvent cnChangeHistoryAddContactEvent => cnChangeHistoryAddContactEvent -> IO (Id NSString)
containerIdentifier cnChangeHistoryAddContactEvent =
  sendMessage cnChangeHistoryAddContactEvent containerIdentifierSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @contact@
contactSelector :: Selector '[] (Id CNContact)
contactSelector = mkSelector "contact"

-- | @Selector@ for @containerIdentifier@
containerIdentifierSelector :: Selector '[] (Id NSString)
containerIdentifierSelector = mkSelector "containerIdentifier"

