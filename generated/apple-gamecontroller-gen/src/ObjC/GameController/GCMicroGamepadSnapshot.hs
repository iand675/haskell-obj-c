{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A GCMicroGamepadSnapshot snapshot is a concrete GCMicroGamepad implementation. It can be used directly in an application to implement controller input replays. It is also returned as the result of polling a controller.
--
-- The current snapshotData is readily available to access as NSData. A developer can serialize this to any destination necessary using the NSData API.
--
-- The data contains some version of a GCMicroGamepadSnapShotData structure.
--
-- See: -[GCMicroGamepad saveSnapshot]
--
-- Generated bindings for @GCMicroGamepadSnapshot@.
module ObjC.GameController.GCMicroGamepadSnapshot
  ( GCMicroGamepadSnapshot
  , IsGCMicroGamepadSnapshot(..)
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
initWithSnapshotData :: (IsGCMicroGamepadSnapshot gcMicroGamepadSnapshot, IsNSData data_) => gcMicroGamepadSnapshot -> data_ -> IO (Id GCMicroGamepadSnapshot)
initWithSnapshotData gcMicroGamepadSnapshot data_ =
  sendOwnedMessage gcMicroGamepadSnapshot initWithSnapshotDataSelector (toNSData data_)

-- | @- initWithController:snapshotData:@
initWithController_snapshotData :: (IsGCMicroGamepadSnapshot gcMicroGamepadSnapshot, IsGCController controller, IsNSData data_) => gcMicroGamepadSnapshot -> controller -> data_ -> IO (Id GCMicroGamepadSnapshot)
initWithController_snapshotData gcMicroGamepadSnapshot controller data_ =
  sendOwnedMessage gcMicroGamepadSnapshot initWithController_snapshotDataSelector (toGCController controller) (toNSData data_)

-- | @- snapshotData@
snapshotData :: IsGCMicroGamepadSnapshot gcMicroGamepadSnapshot => gcMicroGamepadSnapshot -> IO (Id NSData)
snapshotData gcMicroGamepadSnapshot =
  sendMessage gcMicroGamepadSnapshot snapshotDataSelector

-- | @- setSnapshotData:@
setSnapshotData :: (IsGCMicroGamepadSnapshot gcMicroGamepadSnapshot, IsNSData value) => gcMicroGamepadSnapshot -> value -> IO ()
setSnapshotData gcMicroGamepadSnapshot value =
  sendMessage gcMicroGamepadSnapshot setSnapshotDataSelector (toNSData value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSnapshotData:@
initWithSnapshotDataSelector :: Selector '[Id NSData] (Id GCMicroGamepadSnapshot)
initWithSnapshotDataSelector = mkSelector "initWithSnapshotData:"

-- | @Selector@ for @initWithController:snapshotData:@
initWithController_snapshotDataSelector :: Selector '[Id GCController, Id NSData] (Id GCMicroGamepadSnapshot)
initWithController_snapshotDataSelector = mkSelector "initWithController:snapshotData:"

-- | @Selector@ for @snapshotData@
snapshotDataSelector :: Selector '[] (Id NSData)
snapshotDataSelector = mkSelector "snapshotData"

-- | @Selector@ for @setSnapshotData:@
setSnapshotDataSelector :: Selector '[Id NSData] ()
setSnapshotDataSelector = mkSelector "setSnapshotData:"

