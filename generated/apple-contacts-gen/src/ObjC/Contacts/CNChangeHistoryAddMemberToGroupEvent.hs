{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A contact was added to a group
--
-- Generated bindings for @CNChangeHistoryAddMemberToGroupEvent@.
module ObjC.Contacts.CNChangeHistoryAddMemberToGroupEvent
  ( CNChangeHistoryAddMemberToGroupEvent
  , IsCNChangeHistoryAddMemberToGroupEvent(..)
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
member :: IsCNChangeHistoryAddMemberToGroupEvent cnChangeHistoryAddMemberToGroupEvent => cnChangeHistoryAddMemberToGroupEvent -> IO (Id CNContact)
member cnChangeHistoryAddMemberToGroupEvent =
  sendMessage cnChangeHistoryAddMemberToGroupEvent memberSelector

-- | @- group@
group :: IsCNChangeHistoryAddMemberToGroupEvent cnChangeHistoryAddMemberToGroupEvent => cnChangeHistoryAddMemberToGroupEvent -> IO (Id CNGroup)
group cnChangeHistoryAddMemberToGroupEvent =
  sendMessage cnChangeHistoryAddMemberToGroupEvent groupSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @member@
memberSelector :: Selector '[] (Id CNContact)
memberSelector = mkSelector "member"

-- | @Selector@ for @group@
groupSelector :: Selector '[] (Id CNGroup)
groupSelector = mkSelector "group"

