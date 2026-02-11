{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A contact was updated
--
-- Generated bindings for @CNChangeHistoryUpdateContactEvent@.
module ObjC.Contacts.CNChangeHistoryUpdateContactEvent
  ( CNChangeHistoryUpdateContactEvent
  , IsCNChangeHistoryUpdateContactEvent(..)
  , contact
  , contactSelector


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
contact :: IsCNChangeHistoryUpdateContactEvent cnChangeHistoryUpdateContactEvent => cnChangeHistoryUpdateContactEvent -> IO (Id CNContact)
contact cnChangeHistoryUpdateContactEvent  =
  sendMsg cnChangeHistoryUpdateContactEvent (mkSelector "contact") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @contact@
contactSelector :: Selector
contactSelector = mkSelector "contact"

