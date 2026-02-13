{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A GCGamepadSnapshot snapshot is a concrete GCGamepad implementation. It can be used directly in an application to implement controller input replays. It is also returned as the result of polling a controller.
--
-- The current snapshotData is readily available to access as NSData. A developer can serialize this to any destination necessary using the NSData API.
--
-- The data contains some version of a GCGamepadSnapShotData structure.
--
-- See: -[GCGamepad saveSnapshot]
--
-- Generated bindings for @GCGamepadSnapshot@.
module ObjC.GameController.GCGamepadSnapshot
  ( GCGamepadSnapshot
  , IsGCGamepadSnapshot(..)
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
initWithSnapshotData :: (IsGCGamepadSnapshot gcGamepadSnapshot, IsNSData data_) => gcGamepadSnapshot -> data_ -> IO (Id GCGamepadSnapshot)
initWithSnapshotData gcGamepadSnapshot data_ =
  sendOwnedMessage gcGamepadSnapshot initWithSnapshotDataSelector (toNSData data_)

-- | @- initWithController:snapshotData:@
initWithController_snapshotData :: (IsGCGamepadSnapshot gcGamepadSnapshot, IsGCController controller, IsNSData data_) => gcGamepadSnapshot -> controller -> data_ -> IO (Id GCGamepadSnapshot)
initWithController_snapshotData gcGamepadSnapshot controller data_ =
  sendOwnedMessage gcGamepadSnapshot initWithController_snapshotDataSelector (toGCController controller) (toNSData data_)

-- | @- snapshotData@
snapshotData :: IsGCGamepadSnapshot gcGamepadSnapshot => gcGamepadSnapshot -> IO (Id NSData)
snapshotData gcGamepadSnapshot =
  sendMessage gcGamepadSnapshot snapshotDataSelector

-- | @- setSnapshotData:@
setSnapshotData :: (IsGCGamepadSnapshot gcGamepadSnapshot, IsNSData value) => gcGamepadSnapshot -> value -> IO ()
setSnapshotData gcGamepadSnapshot value =
  sendMessage gcGamepadSnapshot setSnapshotDataSelector (toNSData value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSnapshotData:@
initWithSnapshotDataSelector :: Selector '[Id NSData] (Id GCGamepadSnapshot)
initWithSnapshotDataSelector = mkSelector "initWithSnapshotData:"

-- | @Selector@ for @initWithController:snapshotData:@
initWithController_snapshotDataSelector :: Selector '[Id GCController, Id NSData] (Id GCGamepadSnapshot)
initWithController_snapshotDataSelector = mkSelector "initWithController:snapshotData:"

-- | @Selector@ for @snapshotData@
snapshotDataSelector :: Selector '[] (Id NSData)
snapshotDataSelector = mkSelector "snapshotData"

-- | @Selector@ for @setSnapshotData:@
setSnapshotDataSelector :: Selector '[Id NSData] ()
setSnapshotDataSelector = mkSelector "setSnapshotData:"

