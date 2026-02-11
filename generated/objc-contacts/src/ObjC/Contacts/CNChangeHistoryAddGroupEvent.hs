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
  , groupSelector
  , containerIdentifierSelector


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
group :: IsCNChangeHistoryAddGroupEvent cnChangeHistoryAddGroupEvent => cnChangeHistoryAddGroupEvent -> IO (Id CNGroup)
group cnChangeHistoryAddGroupEvent  =
  sendMsg cnChangeHistoryAddGroupEvent (mkSelector "group") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- containerIdentifier@
containerIdentifier :: IsCNChangeHistoryAddGroupEvent cnChangeHistoryAddGroupEvent => cnChangeHistoryAddGroupEvent -> IO (Id NSString)
containerIdentifier cnChangeHistoryAddGroupEvent  =
  sendMsg cnChangeHistoryAddGroupEvent (mkSelector "containerIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @group@
groupSelector :: Selector
groupSelector = mkSelector "group"

-- | @Selector@ for @containerIdentifier@
containerIdentifierSelector :: Selector
containerIdentifierSelector = mkSelector "containerIdentifier"

