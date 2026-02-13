{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A group was added
--
-- Generated bindings for @CNChangeHistoryAddGroupEvent@.
module ObjC.Contacts.CNChangeHistoryAddGroupEvent
  ( CNChangeHistoryAddGroupEvent
  , IsCNChangeHistoryAddGroupEvent(..)
  , group
  , containerIdentifier
  , containerIdentifierSelector
  , groupSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Contacts.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- group@
group :: IsCNChangeHistoryAddGroupEvent cnChangeHistoryAddGroupEvent => cnChangeHistoryAddGroupEvent -> IO (Id CNGroup)
group cnChangeHistoryAddGroupEvent =
  sendMessage cnChangeHistoryAddGroupEvent groupSelector

-- | @- containerIdentifier@
containerIdentifier :: IsCNChangeHistoryAddGroupEvent cnChangeHistoryAddGroupEvent => cnChangeHistoryAddGroupEvent -> IO (Id NSString)
containerIdentifier cnChangeHistoryAddGroupEvent =
  sendMessage cnChangeHistoryAddGroupEvent containerIdentifierSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @group@
groupSelector :: Selector '[] (Id CNGroup)
groupSelector = mkSelector "group"

-- | @Selector@ for @containerIdentifier@
containerIdentifierSelector :: Selector '[] (Id NSString)
containerIdentifierSelector = mkSelector "containerIdentifier"

