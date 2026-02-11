{-# LANGUAGE PatternSynonyms #-}
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
  , displaysLocalCamerasSelector
  , setDisplaysLocalCamerasSelector
  , displaysNetworkCamerasSelector
  , setDisplaysNetworkCamerasSelector
  , displaysLocalScannersSelector
  , setDisplaysLocalScannersSelector
  , displaysNetworkScannersSelector
  , setDisplaysNetworkScannersSelector
  , modeSelector
  , setModeSelector

  -- * Enum types
  , IKDeviceBrowserViewDisplayMode(IKDeviceBrowserViewDisplayMode)
  , pattern IKDeviceBrowserViewDisplayModeTable
  , pattern IKDeviceBrowserViewDisplayModeOutline
  , pattern IKDeviceBrowserViewDisplayModeIcon

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

import ObjC.Quartz.Internal.Classes
import ObjC.Quartz.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | displaysLocalCameras
--
-- for device filtering - indicates that the IKDeviceBrowserView should include local cameras.
--
-- ObjC selector: @- displaysLocalCameras@
displaysLocalCameras :: IsIKDeviceBrowserView ikDeviceBrowserView => ikDeviceBrowserView -> IO Bool
displaysLocalCameras ikDeviceBrowserView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ikDeviceBrowserView (mkSelector "displaysLocalCameras") retCULong []

-- | displaysLocalCameras
--
-- for device filtering - indicates that the IKDeviceBrowserView should include local cameras.
--
-- ObjC selector: @- setDisplaysLocalCameras:@
setDisplaysLocalCameras :: IsIKDeviceBrowserView ikDeviceBrowserView => ikDeviceBrowserView -> Bool -> IO ()
setDisplaysLocalCameras ikDeviceBrowserView  value =
  sendMsg ikDeviceBrowserView (mkSelector "setDisplaysLocalCameras:") retVoid [argCULong (if value then 1 else 0)]

-- | displaysNetworkCameras
--
-- for device filtering - indicates that the IKDeviceBrowserView should include network/shared cameras.
--
-- ObjC selector: @- displaysNetworkCameras@
displaysNetworkCameras :: IsIKDeviceBrowserView ikDeviceBrowserView => ikDeviceBrowserView -> IO Bool
displaysNetworkCameras ikDeviceBrowserView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ikDeviceBrowserView (mkSelector "displaysNetworkCameras") retCULong []

-- | displaysNetworkCameras
--
-- for device filtering - indicates that the IKDeviceBrowserView should include network/shared cameras.
--
-- ObjC selector: @- setDisplaysNetworkCameras:@
setDisplaysNetworkCameras :: IsIKDeviceBrowserView ikDeviceBrowserView => ikDeviceBrowserView -> Bool -> IO ()
setDisplaysNetworkCameras ikDeviceBrowserView  value =
  sendMsg ikDeviceBrowserView (mkSelector "setDisplaysNetworkCameras:") retVoid [argCULong (if value then 1 else 0)]

-- | displaysLocalScanners
--
-- for device filtering - indicates that the IKDeviceBrowserView should include local scanners.
--
-- ObjC selector: @- displaysLocalScanners@
displaysLocalScanners :: IsIKDeviceBrowserView ikDeviceBrowserView => ikDeviceBrowserView -> IO Bool
displaysLocalScanners ikDeviceBrowserView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ikDeviceBrowserView (mkSelector "displaysLocalScanners") retCULong []

-- | displaysLocalScanners
--
-- for device filtering - indicates that the IKDeviceBrowserView should include local scanners.
--
-- ObjC selector: @- setDisplaysLocalScanners:@
setDisplaysLocalScanners :: IsIKDeviceBrowserView ikDeviceBrowserView => ikDeviceBrowserView -> Bool -> IO ()
setDisplaysLocalScanners ikDeviceBrowserView  value =
  sendMsg ikDeviceBrowserView (mkSelector "setDisplaysLocalScanners:") retVoid [argCULong (if value then 1 else 0)]

-- | displaysNetworkScanners
--
-- for device filtering - indicates that the IKDeviceBrowserView should include network/shared scanners.
--
-- ObjC selector: @- displaysNetworkScanners@
displaysNetworkScanners :: IsIKDeviceBrowserView ikDeviceBrowserView => ikDeviceBrowserView -> IO Bool
displaysNetworkScanners ikDeviceBrowserView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ikDeviceBrowserView (mkSelector "displaysNetworkScanners") retCULong []

-- | displaysNetworkScanners
--
-- for device filtering - indicates that the IKDeviceBrowserView should include network/shared scanners.
--
-- ObjC selector: @- setDisplaysNetworkScanners:@
setDisplaysNetworkScanners :: IsIKDeviceBrowserView ikDeviceBrowserView => ikDeviceBrowserView -> Bool -> IO ()
setDisplaysNetworkScanners ikDeviceBrowserView  value =
  sendMsg ikDeviceBrowserView (mkSelector "setDisplaysNetworkScanners:") retVoid [argCULong (if value then 1 else 0)]

-- | mode
--
-- one of the supported display modes (table, outline, or icon mode).
--
-- ObjC selector: @- mode@
mode :: IsIKDeviceBrowserView ikDeviceBrowserView => ikDeviceBrowserView -> IO IKDeviceBrowserViewDisplayMode
mode ikDeviceBrowserView  =
  fmap (coerce :: CLong -> IKDeviceBrowserViewDisplayMode) $ sendMsg ikDeviceBrowserView (mkSelector "mode") retCLong []

-- | mode
--
-- one of the supported display modes (table, outline, or icon mode).
--
-- ObjC selector: @- setMode:@
setMode :: IsIKDeviceBrowserView ikDeviceBrowserView => ikDeviceBrowserView -> IKDeviceBrowserViewDisplayMode -> IO ()
setMode ikDeviceBrowserView  value =
  sendMsg ikDeviceBrowserView (mkSelector "setMode:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @displaysLocalCameras@
displaysLocalCamerasSelector :: Selector
displaysLocalCamerasSelector = mkSelector "displaysLocalCameras"

-- | @Selector@ for @setDisplaysLocalCameras:@
setDisplaysLocalCamerasSelector :: Selector
setDisplaysLocalCamerasSelector = mkSelector "setDisplaysLocalCameras:"

-- | @Selector@ for @displaysNetworkCameras@
displaysNetworkCamerasSelector :: Selector
displaysNetworkCamerasSelector = mkSelector "displaysNetworkCameras"

-- | @Selector@ for @setDisplaysNetworkCameras:@
setDisplaysNetworkCamerasSelector :: Selector
setDisplaysNetworkCamerasSelector = mkSelector "setDisplaysNetworkCameras:"

-- | @Selector@ for @displaysLocalScanners@
displaysLocalScannersSelector :: Selector
displaysLocalScannersSelector = mkSelector "displaysLocalScanners"

-- | @Selector@ for @setDisplaysLocalScanners:@
setDisplaysLocalScannersSelector :: Selector
setDisplaysLocalScannersSelector = mkSelector "setDisplaysLocalScanners:"

-- | @Selector@ for @displaysNetworkScanners@
displaysNetworkScannersSelector :: Selector
displaysNetworkScannersSelector = mkSelector "displaysNetworkScanners"

-- | @Selector@ for @setDisplaysNetworkScanners:@
setDisplaysNetworkScannersSelector :: Selector
setDisplaysNetworkScannersSelector = mkSelector "setDisplaysNetworkScanners:"

-- | @Selector@ for @mode@
modeSelector :: Selector
modeSelector = mkSelector "mode"

-- | @Selector@ for @setMode:@
setModeSelector :: Selector
setModeSelector = mkSelector "setMode:"

