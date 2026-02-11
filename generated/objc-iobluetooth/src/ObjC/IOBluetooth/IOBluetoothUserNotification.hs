{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | IOBluetoothUserNotification
--
-- Represents a registered notification.
--
-- When registering for various notifications in the system, an IOBluetoothUserNotification				object is returned.  To unregister from the notification, call -unregister on the				IOBluetoothUserNotification object.  Once -unregister is called, the object will no				longer be valid.
--
-- Generated bindings for @IOBluetoothUserNotification@.
module ObjC.IOBluetooth.IOBluetoothUserNotification
  ( IOBluetoothUserNotification
  , IsIOBluetoothUserNotification(..)
  , unregister
  , unregisterSelector


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

import ObjC.IOBluetooth.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | unregister
--
-- Called to unregister the target notification.
--
-- Once this method has completed, the target IOBluetoothUserNotification will				no longer be valid.
--
-- ObjC selector: @- unregister@
unregister :: IsIOBluetoothUserNotification ioBluetoothUserNotification => ioBluetoothUserNotification -> IO ()
unregister ioBluetoothUserNotification  =
  sendMsg ioBluetoothUserNotification (mkSelector "unregister") retVoid []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @unregister@
unregisterSelector :: Selector
unregisterSelector = mkSelector "unregister"

