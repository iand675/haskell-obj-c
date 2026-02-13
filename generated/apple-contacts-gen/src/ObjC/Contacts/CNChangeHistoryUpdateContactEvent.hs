{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Contacts.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- contact@
contact :: IsCNChangeHistoryUpdateContactEvent cnChangeHistoryUpdateContactEvent => cnChangeHistoryUpdateContactEvent -> IO (Id CNContact)
contact cnChangeHistoryUpdateContactEvent =
  sendMessage cnChangeHistoryUpdateContactEvent contactSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @contact@
contactSelector :: Selector '[] (Id CNContact)
contactSelector = mkSelector "contact"

