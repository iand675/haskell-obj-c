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
  , initWithSnapshotDataSelector
  , initWithController_snapshotDataSelector
  , snapshotDataSelector
  , setSnapshotDataSelector


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

import ObjC.GameController.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithSnapshotData:@
initWithSnapshotData :: (IsGCGamepadSnapshot gcGamepadSnapshot, IsNSData data_) => gcGamepadSnapshot -> data_ -> IO (Id GCGamepadSnapshot)
initWithSnapshotData gcGamepadSnapshot  data_ =
withObjCPtr data_ $ \raw_data_ ->
    sendMsg gcGamepadSnapshot (mkSelector "initWithSnapshotData:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithController:snapshotData:@
initWithController_snapshotData :: (IsGCGamepadSnapshot gcGamepadSnapshot, IsGCController controller, IsNSData data_) => gcGamepadSnapshot -> controller -> data_ -> IO (Id GCGamepadSnapshot)
initWithController_snapshotData gcGamepadSnapshot  controller data_ =
withObjCPtr controller $ \raw_controller ->
  withObjCPtr data_ $ \raw_data_ ->
      sendMsg gcGamepadSnapshot (mkSelector "initWithController:snapshotData:") (retPtr retVoid) [argPtr (castPtr raw_controller :: Ptr ()), argPtr (castPtr raw_data_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- snapshotData@
snapshotData :: IsGCGamepadSnapshot gcGamepadSnapshot => gcGamepadSnapshot -> IO (Id NSData)
snapshotData gcGamepadSnapshot  =
  sendMsg gcGamepadSnapshot (mkSelector "snapshotData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSnapshotData:@
setSnapshotData :: (IsGCGamepadSnapshot gcGamepadSnapshot, IsNSData value) => gcGamepadSnapshot -> value -> IO ()
setSnapshotData gcGamepadSnapshot  value =
withObjCPtr value $ \raw_value ->
    sendMsg gcGamepadSnapshot (mkSelector "setSnapshotData:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSnapshotData:@
initWithSnapshotDataSelector :: Selector
initWithSnapshotDataSelector = mkSelector "initWithSnapshotData:"

-- | @Selector@ for @initWithController:snapshotData:@
initWithController_snapshotDataSelector :: Selector
initWithController_snapshotDataSelector = mkSelector "initWithController:snapshotData:"

-- | @Selector@ for @snapshotData@
snapshotDataSelector :: Selector
snapshotDataSelector = mkSelector "snapshotData"

-- | @Selector@ for @setSnapshotData:@
setSnapshotDataSelector :: Selector
setSnapshotDataSelector = mkSelector "setSnapshotData:"

