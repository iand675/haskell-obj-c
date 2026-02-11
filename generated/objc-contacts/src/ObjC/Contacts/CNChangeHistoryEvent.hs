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

-- | @- acceptEventVisitor:@
acceptEventVisitor :: IsCNChangeHistoryEvent cnChangeHistoryEvent => cnChangeHistoryEvent -> RawId -> IO ()
acceptEventVisitor cnChangeHistoryEvent  visitor =
  sendMsg cnChangeHistoryEvent (mkSelector "acceptEventVisitor:") retVoid [argPtr (castPtr (unRawId visitor) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @acceptEventVisitor:@
acceptEventVisitorSelector :: Selector
acceptEventVisitorSelector = mkSelector "acceptEventVisitor:"

