{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CNChangeHistoryEvent@.
module ObjC.Contacts.CNChangeHistoryEvent
  ( CNChangeHistoryEvent
  , IsCNChangeHistoryEvent(..)
  , acceptEventVisitor
  , acceptEventVisitorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Contacts.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- acceptEventVisitor:@
acceptEventVisitor :: IsCNChangeHistoryEvent cnChangeHistoryEvent => cnChangeHistoryEvent -> RawId -> IO ()
acceptEventVisitor cnChangeHistoryEvent visitor =
  sendMessage cnChangeHistoryEvent acceptEventVisitorSelector visitor

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @acceptEventVisitor:@
acceptEventVisitorSelector :: Selector '[RawId] ()
acceptEventVisitorSelector = mkSelector "acceptEventVisitor:"

