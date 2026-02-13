{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A GCExtendedGamepadSnapshot snapshot is a concrete GCExtendedGamepad implementation. It can be used directly in an application to implement controller input replays. It is also returned as the result of polling a controller.
--
-- The current snapshotData is readily available to access as NSData. A developer can serialize this to any destination necessary using the NSData API.
--
-- The data contains some version of a GCExtendedGamepadSnapShotData structure.
--
-- See: -[GCExtendedGamepad saveSnapshot]
--
-- Generated bindings for @GCExtendedGamepadSnapshot@.
module ObjC.GameController.GCExtendedGamepadSnapshot
  ( GCExtendedGamepadSnapshot
  , IsGCExtendedGamepadSnapshot(..)
  , initWithSnapshotData
  , initWithController_snapshotData
  , snapshotData
  , setSnapshotData
  , initWithController_snapshotDataSelector
  , initWithSnapshotDataSelector
  , setSnapshotDataSelector
  , snapshotDataSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameController.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithSnapshotData:@
initWithSnapshotData :: (IsGCExtendedGamepadSnapshot gcExtendedGamepadSnapshot, IsNSData data_) => gcExtendedGamepadSnapshot -> data_ -> IO (Id GCExtendedGamepadSnapshot)
initWithSnapshotData gcExtendedGamepadSnapshot data_ =
  sendOwnedMessage gcExtendedGamepadSnapshot initWithSnapshotDataSelector (toNSData data_)

-- | @- initWithController:snapshotData:@
initWithController_snapshotData :: (IsGCExtendedGamepadSnapshot gcExtendedGamepadSnapshot, IsGCController controller, IsNSData data_) => gcExtendedGamepadSnapshot -> controller -> data_ -> IO (Id GCExtendedGamepadSnapshot)
initWithController_snapshotData gcExtendedGamepadSnapshot controller data_ =
  sendOwnedMessage gcExtendedGamepadSnapshot initWithController_snapshotDataSelector (toGCController controller) (toNSData data_)

-- | @- snapshotData@
snapshotData :: IsGCExtendedGamepadSnapshot gcExtendedGamepadSnapshot => gcExtendedGamepadSnapshot -> IO (Id NSData)
snapshotData gcExtendedGamepadSnapshot =
  sendMessage gcExtendedGamepadSnapshot snapshotDataSelector

-- | @- setSnapshotData:@
setSnapshotData :: (IsGCExtendedGamepadSnapshot gcExtendedGamepadSnapshot, IsNSData value) => gcExtendedGamepadSnapshot -> value -> IO ()
setSnapshotData gcExtendedGamepadSnapshot value =
  sendMessage gcExtendedGamepadSnapshot setSnapshotDataSelector (toNSData value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSnapshotData:@
initWithSnapshotDataSelector :: Selector '[Id NSData] (Id GCExtendedGamepadSnapshot)
initWithSnapshotDataSelector = mkSelector "initWithSnapshotData:"

-- | @Selector@ for @initWithController:snapshotData:@
initWithController_snapshotDataSelector :: Selector '[Id GCController, Id NSData] (Id GCExtendedGamepadSnapshot)
initWithController_snapshotDataSelector = mkSelector "initWithController:snapshotData:"

-- | @Selector@ for @snapshotData@
snapshotDataSelector :: Selector '[] (Id NSData)
snapshotDataSelector = mkSelector "snapshotData"

-- | @Selector@ for @setSnapshotData:@
setSnapshotDataSelector :: Selector '[Id NSData] ()
setSnapshotDataSelector = mkSelector "setSnapshotData:"

