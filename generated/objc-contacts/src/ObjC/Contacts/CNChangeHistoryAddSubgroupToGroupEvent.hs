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
subgroup :: IsCNChangeHistoryAddSubgroupToGroupEvent cnChangeHistoryAddSubgroupToGroupEvent => cnChangeHistoryAddSubgroupToGroupEvent -> IO (Id CNGroup)
subgroup cnChangeHistoryAddSubgroupToGroupEvent  =
  sendMsg cnChangeHistoryAddSubgroupToGroupEvent (mkSelector "subgroup") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- group@
group :: IsCNChangeHistoryAddSubgroupToGroupEvent cnChangeHistoryAddSubgroupToGroupEvent => cnChangeHistoryAddSubgroupToGroupEvent -> IO (Id CNGroup)
group cnChangeHistoryAddSubgroupToGroupEvent  =
  sendMsg cnChangeHistoryAddSubgroupToGroupEvent (mkSelector "group") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @subgroup@
subgroupSelector :: Selector
subgroupSelector = mkSelector "subgroup"

-- | @Selector@ for @group@
groupSelector :: Selector
groupSelector = mkSelector "group"

