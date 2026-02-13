{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A subgroup was removed from a group
--
-- Generated bindings for @CNChangeHistoryRemoveSubgroupFromGroupEvent@.
module ObjC.Contacts.CNChangeHistoryRemoveSubgroupFromGroupEvent
  ( CNChangeHistoryRemoveSubgroupFromGroupEvent
  , IsCNChangeHistoryRemoveSubgroupFromGroupEvent(..)
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
subgroup :: IsCNChangeHistoryRemoveSubgroupFromGroupEvent cnChangeHistoryRemoveSubgroupFromGroupEvent => cnChangeHistoryRemoveSubgroupFromGroupEvent -> IO (Id CNGroup)
subgroup cnChangeHistoryRemoveSubgroupFromGroupEvent =
  sendMessage cnChangeHistoryRemoveSubgroupFromGroupEvent subgroupSelector

-- | @- group@
group :: IsCNChangeHistoryRemoveSubgroupFromGroupEvent cnChangeHistoryRemoveSubgroupFromGroupEvent => cnChangeHistoryRemoveSubgroupFromGroupEvent -> IO (Id CNGroup)
group cnChangeHistoryRemoveSubgroupFromGroupEvent =
  sendMessage cnChangeHistoryRemoveSubgroupFromGroupEvent groupSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @subgroup@
subgroupSelector :: Selector '[] (Id CNGroup)
subgroupSelector = mkSelector "subgroup"

-- | @Selector@ for @group@
groupSelector :: Selector '[] (Id CNGroup)
groupSelector = mkSelector "group"

