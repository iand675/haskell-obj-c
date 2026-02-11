{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | OSLogEntryBoundary
--
-- This entry represents metadata that partitions sequences of other entries.
--
-- For example, this kind of entry is used for boot boundaries. The data here are currently informational and carried in the composedMessage property.
--
-- Generated bindings for @OSLogEntryBoundary@.
module ObjC.OSLog.OSLogEntryBoundary
  ( OSLogEntryBoundary
  , IsOSLogEntryBoundary(..)


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

import ObjC.OSLog.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

