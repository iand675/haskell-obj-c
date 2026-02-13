{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A group was updated
--
-- Generated bindings for @CNChangeHistoryUpdateGroupEvent@.
module ObjC.Contacts.CNChangeHistoryUpdateGroupEvent
  ( CNChangeHistoryUpdateGroupEvent
  , IsCNChangeHistoryUpdateGroupEvent(..)
  , group
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
group :: IsCNChangeHistoryUpdateGroupEvent cnChangeHistoryUpdateGroupEvent => cnChangeHistoryUpdateGroupEvent -> IO (Id CNGroup)
group cnChangeHistoryUpdateGroupEvent =
  sendMessage cnChangeHistoryUpdateGroupEvent groupSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @group@
groupSelector :: Selector '[] (Id CNGroup)
groupSelector = mkSelector "group"

