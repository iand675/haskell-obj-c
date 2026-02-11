{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A contact was added
--
-- Generated bindings for @CNChangeHistoryAddContactEvent@.
module ObjC.Contacts.CNChangeHistoryAddContactEvent
  ( CNChangeHistoryAddContactEvent
  , IsCNChangeHistoryAddContactEvent(..)
  , contact
  , containerIdentifier
  , contactSelector
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

-- | @- contact@
contact :: IsCNChangeHistoryAddContactEvent cnChangeHistoryAddContactEvent => cnChangeHistoryAddContactEvent -> IO (Id CNContact)
contact cnChangeHistoryAddContactEvent  =
  sendMsg cnChangeHistoryAddContactEvent (mkSelector "contact") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- containerIdentifier@
containerIdentifier :: IsCNChangeHistoryAddContactEvent cnChangeHistoryAddContactEvent => cnChangeHistoryAddContactEvent -> IO (Id NSString)
containerIdentifier cnChangeHistoryAddContactEvent  =
  sendMsg cnChangeHistoryAddContactEvent (mkSelector "containerIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @contact@
contactSelector :: Selector
contactSelector = mkSelector "contact"

-- | @Selector@ for @containerIdentifier@
containerIdentifierSelector :: Selector
containerIdentifierSelector = mkSelector "containerIdentifier"

