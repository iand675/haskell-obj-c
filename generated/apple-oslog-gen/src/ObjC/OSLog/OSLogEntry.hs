{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | OSLogEntry
--
-- A single entry from the unified logging system.
--
-- Generated bindings for @OSLogEntry@.
module ObjC.OSLog.OSLogEntry
  ( OSLogEntry
  , IsOSLogEntry(..)
  , composedMessage
  , date
  , storeCategory
  , composedMessageSelector
  , dateSelector
  , storeCategorySelector

  -- * Enum types
  , OSLogEntryStoreCategory(OSLogEntryStoreCategory)
  , pattern OSLogEntryStoreCategoryUndefined
  , pattern OSLogEntryStoreCategoryMetadata
  , pattern OSLogEntryStoreCategoryShortTerm
  , pattern OSLogEntryStoreCategoryLongTermAuto
  , pattern OSLogEntryStoreCategoryLongTerm1
  , pattern OSLogEntryStoreCategoryLongTerm3
  , pattern OSLogEntryStoreCategoryLongTerm7
  , pattern OSLogEntryStoreCategoryLongTerm14
  , pattern OSLogEntryStoreCategoryLongTerm30

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.OSLog.Internal.Classes
import ObjC.OSLog.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | composedMessage
--
-- The fully formatted message for the entry.
--
-- ObjC selector: @- composedMessage@
composedMessage :: IsOSLogEntry osLogEntry => osLogEntry -> IO (Id NSString)
composedMessage osLogEntry =
  sendMessage osLogEntry composedMessageSelector

-- | date
--
-- The timestamp of the entry.
--
-- ObjC selector: @- date@
date :: IsOSLogEntry osLogEntry => osLogEntry -> IO (Id NSDate)
date osLogEntry =
  sendMessage osLogEntry dateSelector

-- | storeCategory
--
-- This entry's storage tag. See OSLogEntryStoreCategory.
--
-- ObjC selector: @- storeCategory@
storeCategory :: IsOSLogEntry osLogEntry => osLogEntry -> IO OSLogEntryStoreCategory
storeCategory osLogEntry =
  sendMessage osLogEntry storeCategorySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @composedMessage@
composedMessageSelector :: Selector '[] (Id NSString)
composedMessageSelector = mkSelector "composedMessage"

-- | @Selector@ for @date@
dateSelector :: Selector '[] (Id NSDate)
dateSelector = mkSelector "date"

-- | @Selector@ for @storeCategory@
storeCategorySelector :: Selector '[] OSLogEntryStoreCategory
storeCategorySelector = mkSelector "storeCategory"

