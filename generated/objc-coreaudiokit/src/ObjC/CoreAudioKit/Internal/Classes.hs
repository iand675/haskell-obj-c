{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.CoreAudioKit.Internal.Classes (
    module ObjC.CoreAudioKit.Internal.Classes,
    module ObjC.AppKit.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------- AUAudioUnitViewConfiguration ----------

-- | AUAudioUnitViewConfiguration
--
-- Properties of the configuration that a host uses to embed the view of an audio unit.
--
-- Hosts may support embedding the view of an audio unit in different configurations. These		configurations may vary in the size reserved for the audio unit's view and the additional 		control surfaces that are displayed along with it. The host can propose several view 		configurations and the audio unit should report the ones which it supports.
--
-- See the documentation for supportedViewConfigurations.
-- 
-- Phantom type for @AUAudioUnitViewConfiguration@.
data AUAudioUnitViewConfiguration

instance IsObjCObject (Id AUAudioUnitViewConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AUAudioUnitViewConfiguration"

class IsNSObject a => IsAUAudioUnitViewConfiguration a where
  toAUAudioUnitViewConfiguration :: a -> Id AUAudioUnitViewConfiguration

instance IsAUAudioUnitViewConfiguration (Id AUAudioUnitViewConfiguration) where
  toAUAudioUnitViewConfiguration = unsafeCastId

instance IsNSObject (Id AUAudioUnitViewConfiguration) where
  toNSObject = unsafeCastId

-- ---------- AUGenericView ----------

-- | AUGenericView
--
-- An AUGenericView object retrieves and instantiates a generic user interface view for the given audio unit
-- 
-- Phantom type for @AUGenericView@.
data AUGenericView

instance IsObjCObject (Id AUGenericView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AUGenericView"

class IsNSView a => IsAUGenericView a where
  toAUGenericView :: a -> Id AUGenericView

instance IsAUGenericView (Id AUGenericView) where
  toAUGenericView = unsafeCastId

instance IsNSObject (Id AUGenericView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id AUGenericView) where
  toNSResponder = unsafeCastId

instance IsNSView (Id AUGenericView) where
  toNSView = unsafeCastId

-- ---------- AUPannerView ----------

-- | AUPannerView
--
-- An AUPannerView object retrieves and instantiates a generic panner view for the given panner unit
-- 
-- Phantom type for @AUPannerView@.
data AUPannerView

instance IsObjCObject (Id AUPannerView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AUPannerView"

class IsNSView a => IsAUPannerView a where
  toAUPannerView :: a -> Id AUPannerView

instance IsAUPannerView (Id AUPannerView) where
  toAUPannerView = unsafeCastId

instance IsNSObject (Id AUPannerView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id AUPannerView) where
  toNSResponder = unsafeCastId

instance IsNSView (Id AUPannerView) where
  toNSView = unsafeCastId

-- ---------- AUGenericViewController ----------

-- | Phantom type for @AUGenericViewController@.
data AUGenericViewController

instance IsObjCObject (Id AUGenericViewController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AUGenericViewController"

class IsNSViewController a => IsAUGenericViewController a where
  toAUGenericViewController :: a -> Id AUGenericViewController

instance IsAUGenericViewController (Id AUGenericViewController) where
  toAUGenericViewController = unsafeCastId

instance IsNSObject (Id AUGenericViewController) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id AUGenericViewController) where
  toNSResponder = unsafeCastId

instance IsNSViewController (Id AUGenericViewController) where
  toNSViewController = unsafeCastId

-- ---------- AUViewController ----------

-- | Phantom type for @AUViewController@.
data AUViewController

instance IsObjCObject (Id AUViewController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AUViewController"

class IsNSViewController a => IsAUViewController a where
  toAUViewController :: a -> Id AUViewController

instance IsAUViewController (Id AUViewController) where
  toAUViewController = unsafeCastId

instance IsNSObject (Id AUViewController) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id AUViewController) where
  toNSResponder = unsafeCastId

instance IsNSViewController (Id AUViewController) where
  toNSViewController = unsafeCastId

-- ---------- CAInterDeviceAudioViewController ----------

-- | CAInterDeviceAudioViewController
--
-- A view controller object that manages a view displaying iOS devices that are connected to the Mac and support inter-device audio. The user can select one of those peripherals and connect it to their mac. This class is only available in 64-bit runtimes.
--
-- To use this class, create an instance of the CAInterDeviceAudioController, get the view and add it as a subview of a NSWindow.
-- 
-- Phantom type for @CAInterDeviceAudioViewController@.
data CAInterDeviceAudioViewController

instance IsObjCObject (Id CAInterDeviceAudioViewController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CAInterDeviceAudioViewController"

class IsNSViewController a => IsCAInterDeviceAudioViewController a where
  toCAInterDeviceAudioViewController :: a -> Id CAInterDeviceAudioViewController

instance IsCAInterDeviceAudioViewController (Id CAInterDeviceAudioViewController) where
  toCAInterDeviceAudioViewController = unsafeCastId

instance IsNSObject (Id CAInterDeviceAudioViewController) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id CAInterDeviceAudioViewController) where
  toNSResponder = unsafeCastId

instance IsNSViewController (Id CAInterDeviceAudioViewController) where
  toNSViewController = unsafeCastId

-- ---------- CABTLEMIDIWindowController ----------

-- | CABTLEMIDIWindowController
--
-- A window controller object that can present a window that displays nearby Bluetooth-based MIDI peripherals. The user can select one of those peripherals and pair it with their mac. Additionally, the user can advertise the mac as a Bluetooth-based MIDI peripheral.
--
-- To use this class, create an instance of the CABTLEMIDIWindowController, initialize it, and call showWindow: to display the UI.
-- 
-- Phantom type for @CABTLEMIDIWindowController@.
data CABTLEMIDIWindowController

instance IsObjCObject (Id CABTLEMIDIWindowController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CABTLEMIDIWindowController"

class IsNSWindowController a => IsCABTLEMIDIWindowController a where
  toCABTLEMIDIWindowController :: a -> Id CABTLEMIDIWindowController

instance IsCABTLEMIDIWindowController (Id CABTLEMIDIWindowController) where
  toCABTLEMIDIWindowController = unsafeCastId

instance IsNSObject (Id CABTLEMIDIWindowController) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id CABTLEMIDIWindowController) where
  toNSResponder = unsafeCastId

instance IsNSWindowController (Id CABTLEMIDIWindowController) where
  toNSWindowController = unsafeCastId

-- ---------- CANetworkBrowserWindowController ----------

-- | CANetworkBrowserWindowController
--
-- A window controller object that can present a window that displays available network audio devices (including AVB). The user can connect to one or more of those devices to use exclusively with his mac.
--
-- To use this class, create an instance of the CANetworkBrowserWindowController, initialize it, and call showWindow: to display the UI.
-- 
-- Phantom type for @CANetworkBrowserWindowController@.
data CANetworkBrowserWindowController

instance IsObjCObject (Id CANetworkBrowserWindowController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CANetworkBrowserWindowController"

class IsNSWindowController a => IsCANetworkBrowserWindowController a where
  toCANetworkBrowserWindowController :: a -> Id CANetworkBrowserWindowController

instance IsCANetworkBrowserWindowController (Id CANetworkBrowserWindowController) where
  toCANetworkBrowserWindowController = unsafeCastId

instance IsNSObject (Id CANetworkBrowserWindowController) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id CANetworkBrowserWindowController) where
  toNSResponder = unsafeCastId

instance IsNSWindowController (Id CANetworkBrowserWindowController) where
  toNSWindowController = unsafeCastId
