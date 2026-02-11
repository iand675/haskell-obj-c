{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A contact was removed
--
-- Generated bindings for @CNChangeHistoryDeleteContactEvent@.
module ObjC.Contacts.CNChangeHistoryDeleteContactEvent
  ( CNChangeHistoryDeleteContactEvent
  , IsCNChangeHistoryDeleteContactEvent(..)
  , contactIdentifier
  , contactIdentifierSelector


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

-- | @- contactIdentifier@
contactIdentifier :: IsCNChangeHistoryDeleteContactEvent cnChangeHistoryDeleteContactEvent => cnChangeHistoryDeleteContactEvent -> IO (Id NSString)
contactIdentifier cnChangeHistoryDeleteContactEvent  =
  sendMsg cnChangeHistoryDeleteContactEvent (mkSelector "contactIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @contactIdentifier@
contactIdentifierSelector :: Selector
contactIdentifierSelector = mkSelector "contactIdentifier"

