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
  , subgroupSelector
  , groupSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Contacts.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- subgroup@
subgroup :: IsCNChangeHistoryRemoveSubgroupFromGroupEvent cnChangeHistoryRemoveSubgroupFromGroupEvent => cnChangeHistoryRemoveSubgroupFromGroupEvent -> IO (Id CNGroup)
subgroup cnChangeHistoryRemoveSubgroupFromGroupEvent  =
  sendMsg cnChangeHistoryRemoveSubgroupFromGroupEvent (mkSelector "subgroup") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- group@
group :: IsCNChangeHistoryRemoveSubgroupFromGroupEvent cnChangeHistoryRemoveSubgroupFromGroupEvent => cnChangeHistoryRemoveSubgroupFromGroupEvent -> IO (Id CNGroup)
group cnChangeHistoryRemoveSubgroupFromGroupEvent  =
  sendMsg cnChangeHistoryRemoveSubgroupFromGroupEvent (mkSelector "group") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @subgroup@
subgroupSelector :: Selector
subgroupSelector = mkSelector "subgroup"

-- | @Selector@ for @group@
groupSelector :: Selector
groupSelector = mkSelector "group"

