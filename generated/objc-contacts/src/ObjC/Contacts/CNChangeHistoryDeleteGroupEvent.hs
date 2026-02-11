{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A group was deleted
--
-- Generated bindings for @CNChangeHistoryDeleteGroupEvent@.
module ObjC.Contacts.CNChangeHistoryDeleteGroupEvent
  ( CNChangeHistoryDeleteGroupEvent
  , IsCNChangeHistoryDeleteGroupEvent(..)
  , groupIdentifier
  , groupIdentifierSelector


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

-- | @- groupIdentifier@
groupIdentifier :: IsCNChangeHistoryDeleteGroupEvent cnChangeHistoryDeleteGroupEvent => cnChangeHistoryDeleteGroupEvent -> IO (Id NSString)
groupIdentifier cnChangeHistoryDeleteGroupEvent  =
  sendMsg cnChangeHistoryDeleteGroupEvent (mkSelector "groupIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @groupIdentifier@
groupIdentifierSelector :: Selector
groupIdentifierSelector = mkSelector "groupIdentifier"

