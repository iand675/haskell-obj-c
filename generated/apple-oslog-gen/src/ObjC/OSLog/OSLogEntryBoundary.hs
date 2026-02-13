{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.OSLog.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

