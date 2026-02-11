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
  , memberSelector
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

-- | @- member@
member :: IsCNChangeHistoryAddMemberToGroupEvent cnChangeHistoryAddMemberToGroupEvent => cnChangeHistoryAddMemberToGroupEvent -> IO (Id CNContact)
member cnChangeHistoryAddMemberToGroupEvent  =
  sendMsg cnChangeHistoryAddMemberToGroupEvent (mkSelector "member") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- group@
group :: IsCNChangeHistoryAddMemberToGroupEvent cnChangeHistoryAddMemberToGroupEvent => cnChangeHistoryAddMemberToGroupEvent -> IO (Id CNGroup)
group cnChangeHistoryAddMemberToGroupEvent  =
  sendMsg cnChangeHistoryAddMemberToGroupEvent (mkSelector "group") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @member@
memberSelector :: Selector
memberSelector = mkSelector "member"

-- | @Selector@ for @group@
groupSelector :: Selector
groupSelector = mkSelector "group"

