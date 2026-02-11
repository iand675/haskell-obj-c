{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSGarbageCollector@.
module ObjC.Foundation.NSGarbageCollector
  ( NSGarbageCollector
  , IsNSGarbageCollector(..)
  , defaultCollector
  , isCollecting
  , disable
  , enable
  , isEnabled
  , collectIfNeeded
  , collectExhaustively
  , disableCollectorForPointer
  , enableCollectorForPointer
  , zone
  , defaultCollectorSelector
  , isCollectingSelector
  , disableSelector
  , enableSelector
  , isEnabledSelector
  , collectIfNeededSelector
  , collectExhaustivelySelector
  , disableCollectorForPointerSelector
  , enableCollectorForPointerSelector
  , zoneSelector


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

import ObjC.Foundation.Internal.Classes

-- | @+ defaultCollector@
defaultCollector :: IO RawId
defaultCollector  =
  do
    cls' <- getRequiredClass "NSGarbageCollector"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "defaultCollector") (retPtr retVoid) []

-- | @- isCollecting@
isCollecting :: IsNSGarbageCollector nsGarbageCollector => nsGarbageCollector -> IO Bool
isCollecting nsGarbageCollector  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsGarbageCollector (mkSelector "isCollecting") retCULong []

-- | @- disable@
disable :: IsNSGarbageCollector nsGarbageCollector => nsGarbageCollector -> IO ()
disable nsGarbageCollector  =
  sendMsg nsGarbageCollector (mkSelector "disable") retVoid []

-- | @- enable@
enable :: IsNSGarbageCollector nsGarbageCollector => nsGarbageCollector -> IO ()
enable nsGarbageCollector  =
  sendMsg nsGarbageCollector (mkSelector "enable") retVoid []

-- | @- isEnabled@
isEnabled :: IsNSGarbageCollector nsGarbageCollector => nsGarbageCollector -> IO Bool
isEnabled nsGarbageCollector  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsGarbageCollector (mkSelector "isEnabled") retCULong []

-- | @- collectIfNeeded@
collectIfNeeded :: IsNSGarbageCollector nsGarbageCollector => nsGarbageCollector -> IO ()
collectIfNeeded nsGarbageCollector  =
  sendMsg nsGarbageCollector (mkSelector "collectIfNeeded") retVoid []

-- | @- collectExhaustively@
collectExhaustively :: IsNSGarbageCollector nsGarbageCollector => nsGarbageCollector -> IO ()
collectExhaustively nsGarbageCollector  =
  sendMsg nsGarbageCollector (mkSelector "collectExhaustively") retVoid []

-- | @- disableCollectorForPointer:@
disableCollectorForPointer :: IsNSGarbageCollector nsGarbageCollector => nsGarbageCollector -> Const (Ptr ()) -> IO ()
disableCollectorForPointer nsGarbageCollector  ptr =
  sendMsg nsGarbageCollector (mkSelector "disableCollectorForPointer:") retVoid [argPtr (unConst ptr)]

-- | @- enableCollectorForPointer:@
enableCollectorForPointer :: IsNSGarbageCollector nsGarbageCollector => nsGarbageCollector -> Const (Ptr ()) -> IO ()
enableCollectorForPointer nsGarbageCollector  ptr =
  sendMsg nsGarbageCollector (mkSelector "enableCollectorForPointer:") retVoid [argPtr (unConst ptr)]

-- | @- zone@
zone :: IsNSGarbageCollector nsGarbageCollector => nsGarbageCollector -> IO (Ptr ())
zone nsGarbageCollector  =
  fmap castPtr $ sendMsg nsGarbageCollector (mkSelector "zone") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @defaultCollector@
defaultCollectorSelector :: Selector
defaultCollectorSelector = mkSelector "defaultCollector"

-- | @Selector@ for @isCollecting@
isCollectingSelector :: Selector
isCollectingSelector = mkSelector "isCollecting"

-- | @Selector@ for @disable@
disableSelector :: Selector
disableSelector = mkSelector "disable"

-- | @Selector@ for @enable@
enableSelector :: Selector
enableSelector = mkSelector "enable"

-- | @Selector@ for @isEnabled@
isEnabledSelector :: Selector
isEnabledSelector = mkSelector "isEnabled"

-- | @Selector@ for @collectIfNeeded@
collectIfNeededSelector :: Selector
collectIfNeededSelector = mkSelector "collectIfNeeded"

-- | @Selector@ for @collectExhaustively@
collectExhaustivelySelector :: Selector
collectExhaustivelySelector = mkSelector "collectExhaustively"

-- | @Selector@ for @disableCollectorForPointer:@
disableCollectorForPointerSelector :: Selector
disableCollectorForPointerSelector = mkSelector "disableCollectorForPointer:"

-- | @Selector@ for @enableCollectorForPointer:@
enableCollectorForPointerSelector :: Selector
enableCollectorForPointerSelector = mkSelector "enableCollectorForPointer:"

-- | @Selector@ for @zone@
zoneSelector :: Selector
zoneSelector = mkSelector "zone"

