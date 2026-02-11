{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.DiscRecordingUI.Internal.Classes (
    module ObjC.DiscRecordingUI.Internal.Classes,
    module ObjC.AppKit.Internal.Classes,
    module ObjC.DiscRecording.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AppKit.Internal.Classes
import ObjC.DiscRecording.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------- DRBurnProgressPanel ----------

-- | DRBurnProgressPanel
--
-- Manages a panel that displays progress while burning data to media.
--
-- A DRBurnProgressPanel object manages a panel that displays 					and updates burn progress. The burn panel is responsible 					for begining the burn.
--
-- The burn is begun and a progress panel is displayed on screen 					by calling
--
-- //apple_ref/occ/instm/DRBurnProgressPanel/beginProgressSheetForBurn:layout:modalForWindow: beginProgressSheetForBurn:layout:modalForWindow:
--
-- if a sheet interface is desired, or
--
-- //apple_ref/occ/instm/DRBurnProgressPanel/beginProgressPanelForBurn:layout: beginProgressPanelForBurn:layout:
--
-- for a non-modal panel.
--
-- A DRBurnProgressPanel sends a
--
-- //apple_ref/occ/instm/NSObject/burnProgressPanel:burnDidFinish: burnProgressPanel:burnDidFinish:
--
-- message to it's delegate 					when the burn completes. This method allows the delegate 					to take over end-of-burn handling from the burn progress 					panel to customize error dialogs or user notification.
-- 
-- Phantom type for @DRBurnProgressPanel@.
data DRBurnProgressPanel

instance IsObjCObject (Id DRBurnProgressPanel) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DRBurnProgressPanel"

class IsNSPanel a => IsDRBurnProgressPanel a where
  toDRBurnProgressPanel :: a -> Id DRBurnProgressPanel

instance IsDRBurnProgressPanel (Id DRBurnProgressPanel) where
  toDRBurnProgressPanel = unsafeCastId

instance IsNSObject (Id DRBurnProgressPanel) where
  toNSObject = unsafeCastId

instance IsNSPanel (Id DRBurnProgressPanel) where
  toNSPanel = unsafeCastId

instance IsNSResponder (Id DRBurnProgressPanel) where
  toNSResponder = unsafeCastId

instance IsNSWindow (Id DRBurnProgressPanel) where
  toNSWindow = unsafeCastId

-- ---------- DREraseProgressPanel ----------

-- | DREraseProgressPanel
--
-- Manages a panel that displays progress while erasing media.
--
-- A DREraseProgressPanel object manages a panel that displays 					and updates erase progress. The erase panel is responsible 					for begining the erase.
--
-- The erase is begun and a progress panel is displayed on screen 					by calling
--
-- //apple_ref/occ/instm/DREraseProgressPanel/beginProgressSheetForErase:modalForWindow: beginProgressSheetForErase:modalForWindow:
--
-- if a sheet interface is desired, or
--
-- //apple_ref/occ/instm/DREraseProgressPanel/beginProgressPanelForErase: beginProgressPanelForErase:
--
-- for a non-modal panel.
--
-- A DREraseProgressPanel sends a
--
-- //apple_ref/occ/instm/NSObject/eraseProgressPanel:eraseDidFinish: eraseProgressPanel:eraseDidFinish:
--
-- message to it's delegate 					when the erase completes. This method allows the delegate 					to take over end-of-erase handling from the erase progress 					panel to customize error dialogs or user notification.
-- 
-- Phantom type for @DREraseProgressPanel@.
data DREraseProgressPanel

instance IsObjCObject (Id DREraseProgressPanel) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DREraseProgressPanel"

class IsNSPanel a => IsDREraseProgressPanel a where
  toDREraseProgressPanel :: a -> Id DREraseProgressPanel

instance IsDREraseProgressPanel (Id DREraseProgressPanel) where
  toDREraseProgressPanel = unsafeCastId

instance IsNSObject (Id DREraseProgressPanel) where
  toNSObject = unsafeCastId

instance IsNSPanel (Id DREraseProgressPanel) where
  toNSPanel = unsafeCastId

instance IsNSResponder (Id DREraseProgressPanel) where
  toNSResponder = unsafeCastId

instance IsNSWindow (Id DREraseProgressPanel) where
  toNSWindow = unsafeCastId

-- ---------- DRSetupPanel ----------

-- | DRSetupPanel
--
-- This class is the base class for setup panels in the DiscRecordingUI				framework. It provides a base framework for handling device				selection, media ejection and confirming or cancelling the panel.
-- 
-- Phantom type for @DRSetupPanel@.
data DRSetupPanel

instance IsObjCObject (Id DRSetupPanel) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DRSetupPanel"

class IsNSPanel a => IsDRSetupPanel a where
  toDRSetupPanel :: a -> Id DRSetupPanel

instance IsDRSetupPanel (Id DRSetupPanel) where
  toDRSetupPanel = unsafeCastId

instance IsNSObject (Id DRSetupPanel) where
  toNSObject = unsafeCastId

instance IsNSPanel (Id DRSetupPanel) where
  toNSPanel = unsafeCastId

instance IsNSResponder (Id DRSetupPanel) where
  toNSResponder = unsafeCastId

instance IsNSWindow (Id DRSetupPanel) where
  toNSWindow = unsafeCastId

-- ---------- DRBurnSetupPanel ----------

-- | DRBurnSetupPanel
--
-- Manages a panel that allows users to specify the parameters of an burn.
--
-- This class supports choosing the the device to use, whether or not				to verify the burned data and how to handle the burned disc when it completes.
-- 
-- Phantom type for @DRBurnSetupPanel@.
data DRBurnSetupPanel

instance IsObjCObject (Id DRBurnSetupPanel) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DRBurnSetupPanel"

class IsDRSetupPanel a => IsDRBurnSetupPanel a where
  toDRBurnSetupPanel :: a -> Id DRBurnSetupPanel

instance IsDRBurnSetupPanel (Id DRBurnSetupPanel) where
  toDRBurnSetupPanel = unsafeCastId

instance IsDRSetupPanel (Id DRBurnSetupPanel) where
  toDRSetupPanel = unsafeCastId

instance IsNSObject (Id DRBurnSetupPanel) where
  toNSObject = unsafeCastId

instance IsNSPanel (Id DRBurnSetupPanel) where
  toNSPanel = unsafeCastId

instance IsNSResponder (Id DRBurnSetupPanel) where
  toNSResponder = unsafeCastId

instance IsNSWindow (Id DRBurnSetupPanel) where
  toNSWindow = unsafeCastId

-- ---------- DREraseSetupPanel ----------

-- | DREraseSetupPanel
--
-- Manages a panel that allows users to specify the					parameters of an erase.
--
-- This class supports choosing the 					device to use and what sort of erase to perform.
--
-- When the panel is closed by the user choosing to					erase the media in the device, the device is					exclusively held by the application for its own use					to prevent possible bad or corrupt media from					causing problem for the rest of the system. This					means that if the erase object obtained from the					panel is not used to do an erase, the device will					remain unavailable to other applications until the					exclusive access is released.
-- 
-- Phantom type for @DREraseSetupPanel@.
data DREraseSetupPanel

instance IsObjCObject (Id DREraseSetupPanel) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DREraseSetupPanel"

class IsDRSetupPanel a => IsDREraseSetupPanel a where
  toDREraseSetupPanel :: a -> Id DREraseSetupPanel

instance IsDREraseSetupPanel (Id DREraseSetupPanel) where
  toDREraseSetupPanel = unsafeCastId

instance IsDRSetupPanel (Id DREraseSetupPanel) where
  toDRSetupPanel = unsafeCastId

instance IsNSObject (Id DREraseSetupPanel) where
  toNSObject = unsafeCastId

instance IsNSPanel (Id DREraseSetupPanel) where
  toNSPanel = unsafeCastId

instance IsNSResponder (Id DREraseSetupPanel) where
  toNSResponder = unsafeCastId

instance IsNSWindow (Id DREraseSetupPanel) where
  toNSWindow = unsafeCastId
