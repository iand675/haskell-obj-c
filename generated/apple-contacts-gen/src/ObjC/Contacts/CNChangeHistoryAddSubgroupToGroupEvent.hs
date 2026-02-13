{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A subgroup was added to a group
--
-- Generated bindings for @CNChangeHistoryAddSubgroupToGroupEvent@.
module ObjC.Contacts.CNChangeHistoryAddSubgroupToGroupEvent
  ( CNChangeHistoryAddSubgroupToGroupEvent
  , IsCNChangeHistoryAddSubgroupToGroupEvent(..)
  , subgroup
  , group
  , groupSelector
  , subgroupSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Contacts.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- subgroup@
subgroup :: IsCNChangeHistoryAddSubgroupToGroupEvent cnChangeHistoryAddSubgroupToGroupEvent => cnChangeHistoryAddSubgroupToGroupEvent -> IO (Id CNGroup)
subgroup cnChangeHistoryAddSubgroupToGroupEvent =
  sendMessage cnChangeHistoryAddSubgroupToGroupEvent subgroupSelector

-- | @- group@
group :: IsCNChangeHistoryAddSubgroupToGroupEvent cnChangeHistoryAddSubgroupToGroupEvent => cnChangeHistoryAddSubgroupToGroupEvent -> IO (Id CNGroup)
group cnChangeHistoryAddSubgroupToGroupEvent =
  sendMessage cnChangeHistoryAddSubgroupToGroupEvent groupSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @subgroup@
subgroupSelector :: Selector '[] (Id CNGroup)
subgroupSelector = mkSelector "subgroup"

-- | @Selector@ for @group@
groupSelector :: Selector '[] (Id CNGroup)
groupSelector = mkSelector "group"

