{-# LANGUAGE PatternSynonyms #-}
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
import ObjC.OSLog.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | composedMessage
--
-- The fully formatted message for the entry.
--
-- ObjC selector: @- composedMessage@
composedMessage :: IsOSLogEntry osLogEntry => osLogEntry -> IO (Id NSString)
composedMessage osLogEntry  =
  sendMsg osLogEntry (mkSelector "composedMessage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | date
--
-- The timestamp of the entry.
--
-- ObjC selector: @- date@
date :: IsOSLogEntry osLogEntry => osLogEntry -> IO (Id NSDate)
date osLogEntry  =
  sendMsg osLogEntry (mkSelector "date") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | storeCategory
--
-- This entry's storage tag. See OSLogEntryStoreCategory.
--
-- ObjC selector: @- storeCategory@
storeCategory :: IsOSLogEntry osLogEntry => osLogEntry -> IO OSLogEntryStoreCategory
storeCategory osLogEntry  =
  fmap (coerce :: CLong -> OSLogEntryStoreCategory) $ sendMsg osLogEntry (mkSelector "storeCategory") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @composedMessage@
composedMessageSelector :: Selector
composedMessageSelector = mkSelector "composedMessage"

-- | @Selector@ for @date@
dateSelector :: Selector
dateSelector = mkSelector "date"

-- | @Selector@ for @storeCategory@
storeCategorySelector :: Selector
storeCategorySelector = mkSelector "storeCategory"

