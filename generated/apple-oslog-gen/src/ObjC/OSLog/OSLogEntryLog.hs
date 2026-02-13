{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | OSLogEntryLog
--
-- Entries made by the os_log API.
--
-- Generated bindings for @OSLogEntryLog@.
module ObjC.OSLog.OSLogEntryLog
  ( OSLogEntryLog
  , IsOSLogEntryLog(..)
  , level
  , levelSelector

  -- * Enum types
  , OSLogEntryLogLevel(OSLogEntryLogLevel)
  , pattern OSLogEntryLogLevelUndefined
  , pattern OSLogEntryLogLevelDebug
  , pattern OSLogEntryLogLevelInfo
  , pattern OSLogEntryLogLevelNotice
  , pattern OSLogEntryLogLevelError
  , pattern OSLogEntryLogLevelFault

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

-- | level
--
-- The level of the entry, e.g., info, debug.
--
-- ObjC selector: @- level@
level :: IsOSLogEntryLog osLogEntryLog => osLogEntryLog -> IO OSLogEntryLogLevel
level osLogEntryLog =
  sendMessage osLogEntryLog levelSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @level@
levelSelector :: Selector '[] OSLogEntryLogLevel
levelSelector = mkSelector "level"

