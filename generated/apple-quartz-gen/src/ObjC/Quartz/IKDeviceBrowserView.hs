{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | IKDeviceBrowserView
--
-- IKDeviceBrowserView displays Image Capture cameras and scanners.
--
-- Generated bindings for @IKDeviceBrowserView@.
module ObjC.Quartz.IKDeviceBrowserView
  ( IKDeviceBrowserView
  , IsIKDeviceBrowserView(..)
  , delegate
  , setDelegate
  , displaysLocalCameras
  , setDisplaysLocalCameras
  , displaysNetworkCameras
  , setDisplaysNetworkCameras
  , displaysLocalScanners
  , setDisplaysLocalScanners
  , displaysNetworkScanners
  , setDisplaysNetworkScanners
  , mode
  , setMode
  , delegateSelector
  , displaysLocalCamerasSelector
  , displaysLocalScannersSelector
  , displaysNetworkCamerasSelector
  , displaysNetworkScannersSelector
  , modeSelector
  , setDelegateSelector
  , setDisplaysLocalCamerasSelector
  , setDisplaysLocalScannersSelector
  , setDisplaysNetworkCamerasSelector
  , setDisplaysNetworkScannersSelector
  , setModeSelector

  -- * Enum types
  , IKDeviceBrowserViewDisplayMode(IKDeviceBrowserViewDisplayMode)
  , pattern IKDeviceBrowserViewDisplayModeTable
  , pattern IKDeviceBrowserViewDisplayModeOutline
  , pattern IKDeviceBrowserViewDisplayModeIcon

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Quartz.Internal.Classes
import ObjC.Quartz.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | delegate
--
-- delegate of the IKDeviceBrowserView.
--
-- ObjC selector: @- delegate@
delegate :: IsIKDeviceBrowserView ikDeviceBrowserView => ikDeviceBrowserView -> IO RawId
delegate ikDeviceBrowserView =
  sendMessage ikDeviceBrowserView delegateSelector

-- | delegate
--
-- delegate of the IKDeviceBrowserView.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsIKDeviceBrowserView ikDeviceBrowserView => ikDeviceBrowserView -> RawId -> IO ()
setDelegate ikDeviceBrowserView value =
  sendMessage ikDeviceBrowserView setDelegateSelector value

-- | displaysLocalCameras
--
-- for device filtering - indicates that the IKDeviceBrowserView should include local cameras.
--
-- ObjC selector: @- displaysLocalCameras@
displaysLocalCameras :: IsIKDeviceBrowserView ikDeviceBrowserView => ikDeviceBrowserView -> IO Bool
displaysLocalCameras ikDeviceBrowserView =
  sendMessage ikDeviceBrowserView displaysLocalCamerasSelector

-- | displaysLocalCameras
--
-- for device filtering - indicates that the IKDeviceBrowserView should include local cameras.
--
-- ObjC selector: @- setDisplaysLocalCameras:@
setDisplaysLocalCameras :: IsIKDeviceBrowserView ikDeviceBrowserView => ikDeviceBrowserView -> Bool -> IO ()
setDisplaysLocalCameras ikDeviceBrowserView value =
  sendMessage ikDeviceBrowserView setDisplaysLocalCamerasSelector value

-- | displaysNetworkCameras
--
-- for device filtering - indicates that the IKDeviceBrowserView should include network/shared cameras.
--
-- ObjC selector: @- displaysNetworkCameras@
displaysNetworkCameras :: IsIKDeviceBrowserView ikDeviceBrowserView => ikDeviceBrowserView -> IO Bool
displaysNetworkCameras ikDeviceBrowserView =
  sendMessage ikDeviceBrowserView displaysNetworkCamerasSelector

-- | displaysNetworkCameras
--
-- for device filtering - indicates that the IKDeviceBrowserView should include network/shared cameras.
--
-- ObjC selector: @- setDisplaysNetworkCameras:@
setDisplaysNetworkCameras :: IsIKDeviceBrowserView ikDeviceBrowserView => ikDeviceBrowserView -> Bool -> IO ()
setDisplaysNetworkCameras ikDeviceBrowserView value =
  sendMessage ikDeviceBrowserView setDisplaysNetworkCamerasSelector value

-- | displaysLocalScanners
--
-- for device filtering - indicates that the IKDeviceBrowserView should include local scanners.
--
-- ObjC selector: @- displaysLocalScanners@
displaysLocalScanners :: IsIKDeviceBrowserView ikDeviceBrowserView => ikDeviceBrowserView -> IO Bool
displaysLocalScanners ikDeviceBrowserView =
  sendMessage ikDeviceBrowserView displaysLocalScannersSelector

-- | displaysLocalScanners
--
-- for device filtering - indicates that the IKDeviceBrowserView should include local scanners.
--
-- ObjC selector: @- setDisplaysLocalScanners:@
setDisplaysLocalScanners :: IsIKDeviceBrowserView ikDeviceBrowserView => ikDeviceBrowserView -> Bool -> IO ()
setDisplaysLocalScanners ikDeviceBrowserView value =
  sendMessage ikDeviceBrowserView setDisplaysLocalScannersSelector value

-- | displaysNetworkScanners
--
-- for device filtering - indicates that the IKDeviceBrowserView should include network/shared scanners.
--
-- ObjC selector: @- displaysNetworkScanners@
displaysNetworkScanners :: IsIKDeviceBrowserView ikDeviceBrowserView => ikDeviceBrowserView -> IO Bool
displaysNetworkScanners ikDeviceBrowserView =
  sendMessage ikDeviceBrowserView displaysNetworkScannersSelector

-- | displaysNetworkScanners
--
-- for device filtering - indicates that the IKDeviceBrowserView should include network/shared scanners.
--
-- ObjC selector: @- setDisplaysNetworkScanners:@
setDisplaysNetworkScanners :: IsIKDeviceBrowserView ikDeviceBrowserView => ikDeviceBrowserView -> Bool -> IO ()
setDisplaysNetworkScanners ikDeviceBrowserView value =
  sendMessage ikDeviceBrowserView setDisplaysNetworkScannersSelector value

-- | mode
--
-- one of the supported display modes (table, outline, or icon mode).
--
-- ObjC selector: @- mode@
mode :: IsIKDeviceBrowserView ikDeviceBrowserView => ikDeviceBrowserView -> IO IKDeviceBrowserViewDisplayMode
mode ikDeviceBrowserView =
  sendMessage ikDeviceBrowserView modeSelector

-- | mode
--
-- one of the supported display modes (table, outline, or icon mode).
--
-- ObjC selector: @- setMode:@
setMode :: IsIKDeviceBrowserView ikDeviceBrowserView => ikDeviceBrowserView -> IKDeviceBrowserViewDisplayMode -> IO ()
setMode ikDeviceBrowserView value =
  sendMessage ikDeviceBrowserView setModeSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @displaysLocalCameras@
displaysLocalCamerasSelector :: Selector '[] Bool
displaysLocalCamerasSelector = mkSelector "displaysLocalCameras"

-- | @Selector@ for @setDisplaysLocalCameras:@
setDisplaysLocalCamerasSelector :: Selector '[Bool] ()
setDisplaysLocalCamerasSelector = mkSelector "setDisplaysLocalCameras:"

-- | @Selector@ for @displaysNetworkCameras@
displaysNetworkCamerasSelector :: Selector '[] Bool
displaysNetworkCamerasSelector = mkSelector "displaysNetworkCameras"

-- | @Selector@ for @setDisplaysNetworkCameras:@
setDisplaysNetworkCamerasSelector :: Selector '[Bool] ()
setDisplaysNetworkCamerasSelector = mkSelector "setDisplaysNetworkCameras:"

-- | @Selector@ for @displaysLocalScanners@
displaysLocalScannersSelector :: Selector '[] Bool
displaysLocalScannersSelector = mkSelector "displaysLocalScanners"

-- | @Selector@ for @setDisplaysLocalScanners:@
setDisplaysLocalScannersSelector :: Selector '[Bool] ()
setDisplaysLocalScannersSelector = mkSelector "setDisplaysLocalScanners:"

-- | @Selector@ for @displaysNetworkScanners@
displaysNetworkScannersSelector :: Selector '[] Bool
displaysNetworkScannersSelector = mkSelector "displaysNetworkScanners"

-- | @Selector@ for @setDisplaysNetworkScanners:@
setDisplaysNetworkScannersSelector :: Selector '[Bool] ()
setDisplaysNetworkScannersSelector = mkSelector "setDisplaysNetworkScanners:"

-- | @Selector@ for @mode@
modeSelector :: Selector '[] IKDeviceBrowserViewDisplayMode
modeSelector = mkSelector "mode"

-- | @Selector@ for @setMode:@
setModeSelector :: Selector '[IKDeviceBrowserViewDisplayMode] ()
setModeSelector = mkSelector "setMode:"

