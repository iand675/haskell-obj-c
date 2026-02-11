{-# LANGUAGE PatternSynonyms #-}
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

-- | signpostIdentifier
--
-- The signpost ID associated with this entry.
--
-- ObjC selector: @- signpostIdentifier@
signpostIdentifier :: IsOSLogEntrySignpost osLogEntrySignpost => osLogEntrySignpost -> IO CULong
signpostIdentifier osLogEntrySignpost  =
  sendMsg osLogEntrySignpost (mkSelector "signpostIdentifier") retCULong []

-- | signpostName
--
-- The signpost name associated with this entry.
--
-- ObjC selector: @- signpostName@
signpostName :: IsOSLogEntrySignpost osLogEntrySignpost => osLogEntrySignpost -> IO (Id NSString)
signpostName osLogEntrySignpost  =
  sendMsg osLogEntrySignpost (mkSelector "signpostName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | signpostType
--
-- The signpost type associated with this entry.
--
-- ObjC selector: @- signpostType@
signpostType :: IsOSLogEntrySignpost osLogEntrySignpost => osLogEntrySignpost -> IO OSLogEntrySignpostType
signpostType osLogEntrySignpost  =
  fmap (coerce :: CLong -> OSLogEntrySignpostType) $ sendMsg osLogEntrySignpost (mkSelector "signpostType") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @signpostIdentifier@
signpostIdentifierSelector :: Selector
signpostIdentifierSelector = mkSelector "signpostIdentifier"

-- | @Selector@ for @signpostName@
signpostNameSelector :: Selector
signpostNameSelector = mkSelector "signpostName"

-- | @Selector@ for @signpostType@
signpostTypeSelector :: Selector
signpostTypeSelector = mkSelector "signpostType"

