{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | OSLogEntrySignpost
--
-- Entries made by the os_signpost API.
--
-- Generated bindings for @OSLogEntrySignpost@.
module ObjC.OSLog.OSLogEntrySignpost
  ( OSLogEntrySignpost
  , IsOSLogEntrySignpost(..)
  , signpostIdentifier
  , signpostName
  , signpostType
  , signpostIdentifierSelector
  , signpostNameSelector
  , signpostTypeSelector

  -- * Enum types
  , OSLogEntrySignpostType(OSLogEntrySignpostType)
  , pattern OSLogEntrySignpostTypeUndefined
  , pattern OSLogEntrySignpostTypeIntervalBegin
  , pattern OSLogEntrySignpostTypeIntervalEnd
  , pattern OSLogEntrySignpostTypeEvent

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

-- | signpostIdentifier
--
-- The signpost ID associated with this entry.
--
-- ObjC selector: @- signpostIdentifier@
signpostIdentifier :: IsOSLogEntrySignpost osLogEntrySignpost => osLogEntrySignpost -> IO CULong
signpostIdentifier osLogEntrySignpost =
  sendMessage osLogEntrySignpost signpostIdentifierSelector

-- | signpostName
--
-- The signpost name associated with this entry.
--
-- ObjC selector: @- signpostName@
signpostName :: IsOSLogEntrySignpost osLogEntrySignpost => osLogEntrySignpost -> IO (Id NSString)
signpostName osLogEntrySignpost =
  sendMessage osLogEntrySignpost signpostNameSelector

-- | signpostType
--
-- The signpost type associated with this entry.
--
-- ObjC selector: @- signpostType@
signpostType :: IsOSLogEntrySignpost osLogEntrySignpost => osLogEntrySignpost -> IO OSLogEntrySignpostType
signpostType osLogEntrySignpost =
  sendMessage osLogEntrySignpost signpostTypeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @signpostIdentifier@
signpostIdentifierSelector :: Selector '[] CULong
signpostIdentifierSelector = mkSelector "signpostIdentifier"

-- | @Selector@ for @signpostName@
signpostNameSelector :: Selector '[] (Id NSString)
signpostNameSelector = mkSelector "signpostName"

-- | @Selector@ for @signpostType@
signpostTypeSelector :: Selector '[] OSLogEntrySignpostType
signpostTypeSelector = mkSelector "signpostType"

