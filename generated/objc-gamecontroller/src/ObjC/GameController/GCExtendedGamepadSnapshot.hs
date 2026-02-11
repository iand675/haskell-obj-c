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
initWithSnapshotData :: (IsGCExtendedGamepadSnapshot gcExtendedGamepadSnapshot, IsNSData data_) => gcExtendedGamepadSnapshot -> data_ -> IO (Id GCExtendedGamepadSnapshot)
initWithSnapshotData gcExtendedGamepadSnapshot  data_ =
withObjCPtr data_ $ \raw_data_ ->
    sendMsg gcExtendedGamepadSnapshot (mkSelector "initWithSnapshotData:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithController:snapshotData:@
initWithController_snapshotData :: (IsGCExtendedGamepadSnapshot gcExtendedGamepadSnapshot, IsGCController controller, IsNSData data_) => gcExtendedGamepadSnapshot -> controller -> data_ -> IO (Id GCExtendedGamepadSnapshot)
initWithController_snapshotData gcExtendedGamepadSnapshot  controller data_ =
withObjCPtr controller $ \raw_controller ->
  withObjCPtr data_ $ \raw_data_ ->
      sendMsg gcExtendedGamepadSnapshot (mkSelector "initWithController:snapshotData:") (retPtr retVoid) [argPtr (castPtr raw_controller :: Ptr ()), argPtr (castPtr raw_data_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- snapshotData@
snapshotData :: IsGCExtendedGamepadSnapshot gcExtendedGamepadSnapshot => gcExtendedGamepadSnapshot -> IO (Id NSData)
snapshotData gcExtendedGamepadSnapshot  =
  sendMsg gcExtendedGamepadSnapshot (mkSelector "snapshotData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSnapshotData:@
setSnapshotData :: (IsGCExtendedGamepadSnapshot gcExtendedGamepadSnapshot, IsNSData value) => gcExtendedGamepadSnapshot -> value -> IO ()
setSnapshotData gcExtendedGamepadSnapshot  value =
withObjCPtr value $ \raw_value ->
    sendMsg gcExtendedGamepadSnapshot (mkSelector "setSnapshotData:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

