{-# LANGUAGE PatternSynonyms #-}
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

-- | level
--
-- The level of the entry, e.g., info, debug.
--
-- ObjC selector: @- level@
level :: IsOSLogEntryLog osLogEntryLog => osLogEntryLog -> IO OSLogEntryLogLevel
level osLogEntryLog  =
  fmap (coerce :: CLong -> OSLogEntryLogLevel) $ sendMsg osLogEntryLog (mkSelector "level") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @level@
levelSelector :: Selector
levelSelector = mkSelector "level"

