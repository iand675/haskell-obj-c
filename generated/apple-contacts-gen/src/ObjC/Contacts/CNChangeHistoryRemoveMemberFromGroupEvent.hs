{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A contact was removed from a group
--
-- Generated bindings for @CNChangeHistoryRemoveMemberFromGroupEvent@.
module ObjC.Contacts.CNChangeHistoryRemoveMemberFromGroupEvent
  ( CNChangeHistoryRemoveMemberFromGroupEvent
  , IsCNChangeHistoryRemoveMemberFromGroupEvent(..)
  , member
  , group
  , groupSelector
  , memberSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Contacts.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- member@
member :: IsCNChangeHistoryRemoveMemberFromGroupEvent cnChangeHistoryRemoveMemberFromGroupEvent => cnChangeHistoryRemoveMemberFromGroupEvent -> IO (Id CNContact)
member cnChangeHistoryRemoveMemberFromGroupEvent =
  sendMessage cnChangeHistoryRemoveMemberFromGroupEvent memberSelector

-- | @- group@
group :: IsCNChangeHistoryRemoveMemberFromGroupEvent cnChangeHistoryRemoveMemberFromGroupEvent => cnChangeHistoryRemoveMemberFromGroupEvent -> IO (Id CNGroup)
group cnChangeHistoryRemoveMemberFromGroupEvent =
  sendMessage cnChangeHistoryRemoveMemberFromGroupEvent groupSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @member@
memberSelector :: Selector '[] (Id CNContact)
memberSelector = mkSelector "member"

-- | @Selector@ for @group@
groupSelector :: Selector '[] (Id CNGroup)
groupSelector = mkSelector "group"

