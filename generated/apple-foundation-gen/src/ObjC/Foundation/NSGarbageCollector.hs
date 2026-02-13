{-# LANGUAGE DataKinds #-}
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
  , collectExhaustivelySelector
  , collectIfNeededSelector
  , defaultCollectorSelector
  , disableCollectorForPointerSelector
  , disableSelector
  , enableCollectorForPointerSelector
  , enableSelector
  , isCollectingSelector
  , isEnabledSelector
  , zoneSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ defaultCollector@
defaultCollector :: IO RawId
defaultCollector  =
  do
    cls' <- getRequiredClass "NSGarbageCollector"
    sendClassMessage cls' defaultCollectorSelector

-- | @- isCollecting@
isCollecting :: IsNSGarbageCollector nsGarbageCollector => nsGarbageCollector -> IO Bool
isCollecting nsGarbageCollector =
  sendMessage nsGarbageCollector isCollectingSelector

-- | @- disable@
disable :: IsNSGarbageCollector nsGarbageCollector => nsGarbageCollector -> IO ()
disable nsGarbageCollector =
  sendMessage nsGarbageCollector disableSelector

-- | @- enable@
enable :: IsNSGarbageCollector nsGarbageCollector => nsGarbageCollector -> IO ()
enable nsGarbageCollector =
  sendMessage nsGarbageCollector enableSelector

-- | @- isEnabled@
isEnabled :: IsNSGarbageCollector nsGarbageCollector => nsGarbageCollector -> IO Bool
isEnabled nsGarbageCollector =
  sendMessage nsGarbageCollector isEnabledSelector

-- | @- collectIfNeeded@
collectIfNeeded :: IsNSGarbageCollector nsGarbageCollector => nsGarbageCollector -> IO ()
collectIfNeeded nsGarbageCollector =
  sendMessage nsGarbageCollector collectIfNeededSelector

-- | @- collectExhaustively@
collectExhaustively :: IsNSGarbageCollector nsGarbageCollector => nsGarbageCollector -> IO ()
collectExhaustively nsGarbageCollector =
  sendMessage nsGarbageCollector collectExhaustivelySelector

-- | @- disableCollectorForPointer:@
disableCollectorForPointer :: IsNSGarbageCollector nsGarbageCollector => nsGarbageCollector -> Const (Ptr ()) -> IO ()
disableCollectorForPointer nsGarbageCollector ptr =
  sendMessage nsGarbageCollector disableCollectorForPointerSelector ptr

-- | @- enableCollectorForPointer:@
enableCollectorForPointer :: IsNSGarbageCollector nsGarbageCollector => nsGarbageCollector -> Const (Ptr ()) -> IO ()
enableCollectorForPointer nsGarbageCollector ptr =
  sendMessage nsGarbageCollector enableCollectorForPointerSelector ptr

-- | @- zone@
zone :: IsNSGarbageCollector nsGarbageCollector => nsGarbageCollector -> IO (Ptr ())
zone nsGarbageCollector =
  sendMessage nsGarbageCollector zoneSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @defaultCollector@
defaultCollectorSelector :: Selector '[] RawId
defaultCollectorSelector = mkSelector "defaultCollector"

-- | @Selector@ for @isCollecting@
isCollectingSelector :: Selector '[] Bool
isCollectingSelector = mkSelector "isCollecting"

-- | @Selector@ for @disable@
disableSelector :: Selector '[] ()
disableSelector = mkSelector "disable"

-- | @Selector@ for @enable@
enableSelector :: Selector '[] ()
enableSelector = mkSelector "enable"

-- | @Selector@ for @isEnabled@
isEnabledSelector :: Selector '[] Bool
isEnabledSelector = mkSelector "isEnabled"

-- | @Selector@ for @collectIfNeeded@
collectIfNeededSelector :: Selector '[] ()
collectIfNeededSelector = mkSelector "collectIfNeeded"

-- | @Selector@ for @collectExhaustively@
collectExhaustivelySelector :: Selector '[] ()
collectExhaustivelySelector = mkSelector "collectExhaustively"

-- | @Selector@ for @disableCollectorForPointer:@
disableCollectorForPointerSelector :: Selector '[Const (Ptr ())] ()
disableCollectorForPointerSelector = mkSelector "disableCollectorForPointer:"

-- | @Selector@ for @enableCollectorForPointer:@
enableCollectorForPointerSelector :: Selector '[Const (Ptr ())] ()
enableCollectorForPointerSelector = mkSelector "enableCollectorForPointer:"

-- | @Selector@ for @zone@
zoneSelector :: Selector '[] (Ptr ())
zoneSelector = mkSelector "zone"

