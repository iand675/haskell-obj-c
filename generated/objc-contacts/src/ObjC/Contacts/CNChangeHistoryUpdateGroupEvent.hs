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

-- | @- group@
group :: IsCNChangeHistoryUpdateGroupEvent cnChangeHistoryUpdateGroupEvent => cnChangeHistoryUpdateGroupEvent -> IO (Id CNGroup)
group cnChangeHistoryUpdateGroupEvent  =
  sendMsg cnChangeHistoryUpdateGroupEvent (mkSelector "group") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @group@
groupSelector :: Selector
groupSelector = mkSelector "group"

