{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.IOBluetoothUI.Internal.Classes (
    module ObjC.IOBluetoothUI.Internal.Classes,
    module ObjC.AppKit.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
    module ObjC.IOBluetooth.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.IOBluetooth.Internal.Classes

-- ---------- IOBluetoothAccessibilityIgnoredImageCell ----------

-- | Phantom type for @IOBluetoothAccessibilityIgnoredImageCell@.
data IOBluetoothAccessibilityIgnoredImageCell

instance IsObjCObject (Id IOBluetoothAccessibilityIgnoredImageCell) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "IOBluetoothAccessibilityIgnoredImageCell"

class IsNSImageCell a => IsIOBluetoothAccessibilityIgnoredImageCell a where
  toIOBluetoothAccessibilityIgnoredImageCell :: a -> Id IOBluetoothAccessibilityIgnoredImageCell

instance IsIOBluetoothAccessibilityIgnoredImageCell (Id IOBluetoothAccessibilityIgnoredImageCell) where
  toIOBluetoothAccessibilityIgnoredImageCell = unsafeCastId

instance IsNSCell (Id IOBluetoothAccessibilityIgnoredImageCell) where
  toNSCell = unsafeCastId

instance IsNSImageCell (Id IOBluetoothAccessibilityIgnoredImageCell) where
  toNSImageCell = unsafeCastId

instance IsNSObject (Id IOBluetoothAccessibilityIgnoredImageCell) where
  toNSObject = unsafeCastId

-- ---------- IOBluetoothPasskeyDisplay ----------

-- | Phantom type for @IOBluetoothPasskeyDisplay@.
data IOBluetoothPasskeyDisplay

instance IsObjCObject (Id IOBluetoothPasskeyDisplay) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "IOBluetoothPasskeyDisplay"

class IsNSView a => IsIOBluetoothPasskeyDisplay a where
  toIOBluetoothPasskeyDisplay :: a -> Id IOBluetoothPasskeyDisplay

instance IsIOBluetoothPasskeyDisplay (Id IOBluetoothPasskeyDisplay) where
  toIOBluetoothPasskeyDisplay = unsafeCastId

instance IsNSObject (Id IOBluetoothPasskeyDisplay) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id IOBluetoothPasskeyDisplay) where
  toNSResponder = unsafeCastId

instance IsNSView (Id IOBluetoothPasskeyDisplay) where
  toNSView = unsafeCastId

-- ---------- IOBluetoothDeviceSelectorController ----------

-- | IOBluetoothDeviceSelectorController
--
-- A NSWindowController subclass to display a window to initiate pairing to other bluetooth devices.
--
-- Implementation of a window controller to return a NSArray of selected bluetooth devices.  This                    class will handle connecting to the Bluetooth Daemon for the purposes of searches, and displaying                    the results.  This controller will return a NSArray of IOBluetoothDevice objects to the user.
-- 
-- Phantom type for @IOBluetoothDeviceSelectorController@.
data IOBluetoothDeviceSelectorController

instance IsObjCObject (Id IOBluetoothDeviceSelectorController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "IOBluetoothDeviceSelectorController"

class IsNSWindowController a => IsIOBluetoothDeviceSelectorController a where
  toIOBluetoothDeviceSelectorController :: a -> Id IOBluetoothDeviceSelectorController

instance IsIOBluetoothDeviceSelectorController (Id IOBluetoothDeviceSelectorController) where
  toIOBluetoothDeviceSelectorController = unsafeCastId

instance IsNSObject (Id IOBluetoothDeviceSelectorController) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id IOBluetoothDeviceSelectorController) where
  toNSResponder = unsafeCastId

instance IsNSWindowController (Id IOBluetoothDeviceSelectorController) where
  toNSWindowController = unsafeCastId

-- ---------- IOBluetoothObjectPushUIController ----------

-- | An NSWindowController subclass that supports the creation of an IOBluetoothObjectPushUIController object.
-- 
-- Phantom type for @IOBluetoothObjectPushUIController@.
data IOBluetoothObjectPushUIController

instance IsObjCObject (Id IOBluetoothObjectPushUIController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "IOBluetoothObjectPushUIController"

class IsNSWindowController a => IsIOBluetoothObjectPushUIController a where
  toIOBluetoothObjectPushUIController :: a -> Id IOBluetoothObjectPushUIController

instance IsIOBluetoothObjectPushUIController (Id IOBluetoothObjectPushUIController) where
  toIOBluetoothObjectPushUIController = unsafeCastId

instance IsNSObject (Id IOBluetoothObjectPushUIController) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id IOBluetoothObjectPushUIController) where
  toNSResponder = unsafeCastId

instance IsNSWindowController (Id IOBluetoothObjectPushUIController) where
  toNSWindowController = unsafeCastId

-- ---------- IOBluetoothPairingController ----------

-- | IOBluetoothPairingController
--
-- A NSWindowController subclass to display a window to initiate pairing to other bluetooth devices.
--
-- Implementation of a window controller to handle pairing with a bluetooth device.  This					class will handle connecting to the Bluetooth Daemon for the purposes of searches, and displaying the results.					When necessary this class will display a sheet asking the user for a PIN code.  This window will not return					anything to the caller if it is canceled or if pairing occurs.
-- 
-- Phantom type for @IOBluetoothPairingController@.
data IOBluetoothPairingController

instance IsObjCObject (Id IOBluetoothPairingController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "IOBluetoothPairingController"

class IsNSWindowController a => IsIOBluetoothPairingController a where
  toIOBluetoothPairingController :: a -> Id IOBluetoothPairingController

instance IsIOBluetoothPairingController (Id IOBluetoothPairingController) where
  toIOBluetoothPairingController = unsafeCastId

instance IsNSObject (Id IOBluetoothPairingController) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id IOBluetoothPairingController) where
  toNSResponder = unsafeCastId

instance IsNSWindowController (Id IOBluetoothPairingController) where
  toNSWindowController = unsafeCastId

-- ---------- IOBluetoothServiceBrowserController ----------

-- | IOBluetoothServiceBrowserController
--
-- A NSWindowController subclass to display a window to search for and perform SDP queries on bluetooth                    devices within range.
--
-- This NSWindowController subclass will bring up a generic Bluetooth search and SDP browsing window                    allowing the user to find devices within range, perform SDP queries on a particular device, and                    select a SDP service to connect to.  The client application can provide NSArrays of valid service                    UUIDs to allow, and an NSArray of valid device types to allow.  The device type filter is not                    yet implemented.
-- 
-- Phantom type for @IOBluetoothServiceBrowserController@.
data IOBluetoothServiceBrowserController

instance IsObjCObject (Id IOBluetoothServiceBrowserController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "IOBluetoothServiceBrowserController"

class IsNSWindowController a => IsIOBluetoothServiceBrowserController a where
  toIOBluetoothServiceBrowserController :: a -> Id IOBluetoothServiceBrowserController

instance IsIOBluetoothServiceBrowserController (Id IOBluetoothServiceBrowserController) where
  toIOBluetoothServiceBrowserController = unsafeCastId

instance IsNSObject (Id IOBluetoothServiceBrowserController) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id IOBluetoothServiceBrowserController) where
  toNSResponder = unsafeCastId

instance IsNSWindowController (Id IOBluetoothServiceBrowserController) where
  toNSWindowController = unsafeCastId

-- ---------- IOBluetoothAccessibilityIgnoredTextFieldCell ----------

-- | Phantom type for @IOBluetoothAccessibilityIgnoredTextFieldCell@.
data IOBluetoothAccessibilityIgnoredTextFieldCell

instance IsObjCObject (Id IOBluetoothAccessibilityIgnoredTextFieldCell) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "IOBluetoothAccessibilityIgnoredTextFieldCell"

class IsNSTextFieldCell a => IsIOBluetoothAccessibilityIgnoredTextFieldCell a where
  toIOBluetoothAccessibilityIgnoredTextFieldCell :: a -> Id IOBluetoothAccessibilityIgnoredTextFieldCell

instance IsIOBluetoothAccessibilityIgnoredTextFieldCell (Id IOBluetoothAccessibilityIgnoredTextFieldCell) where
  toIOBluetoothAccessibilityIgnoredTextFieldCell = unsafeCastId

instance IsNSActionCell (Id IOBluetoothAccessibilityIgnoredTextFieldCell) where
  toNSActionCell = unsafeCastId

instance IsNSCell (Id IOBluetoothAccessibilityIgnoredTextFieldCell) where
  toNSCell = unsafeCastId

instance IsNSObject (Id IOBluetoothAccessibilityIgnoredTextFieldCell) where
  toNSObject = unsafeCastId

instance IsNSTextFieldCell (Id IOBluetoothAccessibilityIgnoredTextFieldCell) where
  toNSTextFieldCell = unsafeCastId
